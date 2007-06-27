module CmmCPS (
  -- | Converts C-- with full proceedures and parameters
  -- to a CPS transformed C-- with the stack made manifest.
  cmmCPS
) where

#include "HsVersions.h"

import Cmm
import CmmLint
import PprCmm

import CmmLive
import CmmBrokenBlock
import CmmProcPoint
import CmmCallConv
import CmmInfo
import CmmUtils

import Bitmap
import ClosureInfo
import MachOp
import ForeignCall
import CLabel
import SMRep
import Constants

import DynFlags
import ErrUtils
import Maybes
import Outputable
import UniqSupply
import UniqFM
import UniqSet
import Unique

import Monad
import IO
import Data.List

-----------------------------------------------------------------------------
-- |Top level driver for the CPS pass
-----------------------------------------------------------------------------
cmmCPS :: DynFlags -- ^ Dynamic flags: -dcmm-lint -ddump-cps-cmm
       -> [GenCmm CmmStatic CmmInfo CmmStmt]    -- ^ Input C-- with Proceedures
       -> IO [GenCmm CmmStatic [CmmStatic] CmmStmt] -- ^ Output CPS transformed C--
cmmCPS dflags abstractC = do
  when (dopt Opt_DoCmmLinting dflags) $
       do showPass dflags "CmmLint"
	  case firstJust $ map cmmLint abstractC of
	    Just err -> do printDump err
			   ghcExit dflags 1
	    Nothing  -> return ()
  showPass dflags "CPS"

  -- TODO: more lint checking
  --        check for use of branches to non-existant blocks
  --        check for use of Sp, SpLim, R1, R2, etc.

  uniqSupply <- mkSplitUniqSupply 'p'
  let supplies = listSplitUniqSupply uniqSupply
  let doCpsProc s (Cmm c) =
          Cmm $ concat $ zipWith cpsProc (listSplitUniqSupply s) c
  let continuationC = zipWith doCpsProc supplies abstractC

  dumpIfSet_dyn dflags Opt_D_dump_cps_cmm "CPS Cmm" (pprCmms continuationC)

  -- TODO: add option to dump Cmm to file

  return continuationC

-----------------------------------------------------------------------------
-- |CPS a single CmmTop (proceedure)
-- Only 'CmmProc' are transformed 'CmmData' will be left alone.
-----------------------------------------------------------------------------

cpsProc :: UniqSupply 
        -> GenCmmTop CmmStatic CmmInfo CmmStmt     -- ^Input proceedure
        -> [GenCmmTop CmmStatic [CmmStatic] CmmStmt]   -- ^Output proceedure and continuations
cpsProc uniqSupply (CmmData sec dat) = [CmmData sec dat]
cpsProc uniqSupply (CmmProc info ident params blocks) = info_procs
    where
      uniques :: [[Unique]]
      uniques = map uniqsFromSupply $ listSplitUniqSupply uniqSupply
      info_uniques:block_uniques = uniques

      -- Break the block at each function call.
      -- The part after the function call will have to become a continuation.
      broken_blocks :: [BrokenBlock]
      broken_blocks =
          concat $ zipWith3 breakBlock block_uniques blocks
                     (FunctionEntry info ident params:repeat ControlEntry)

      -- Calculate live variables for each broken block.
      --
      -- Nothing can be live on entry to the first block
      -- so we could take the tail, but for now we wont
      -- to help future proof the code.
      live :: BlockEntryLiveness
      live = cmmLiveness $ map cmmBlockFromBrokenBlock broken_blocks

      -- Calculate which blocks must be made into full fledged procedures.
      proc_points :: UniqSet BlockId
      proc_points = calculateProcPoints broken_blocks

      -- Construct a map so we can lookup a broken block by its 'BlockId'.
      block_env :: BlockEnv BrokenBlock
      block_env = blocksToBlockEnv broken_blocks

      -- Group the blocks into continuations based on the set of proc-points.
      continuations :: [Continuation (Either C_SRT CmmInfo)]
      continuations = map (gatherBlocksIntoContinuation proc_points block_env)
                          (uniqSetToList proc_points)

      -- Select the stack format on entry to each continuation.
      -- Return the max stack offset and an association list
      --
      -- This is an association list instead of a UniqFM because
      -- CLabel's don't have a 'Uniqueable' instance.
      formats :: [(CLabel, (Maybe CLabel, [Maybe LocalReg]))]
      formats = selectStackFormat live continuations

      -- Do a little meta-processing on the stack formats such as
      -- getting the individual frame sizes and the maximum frame size
      formats' :: (WordOff, [(CLabel, StackFormat)])
      formats' = processFormats formats

      -- TODO FIXME NOW: calculate a real max stack (including function call args)
      -- TODO: from the maximum frame size get the maximum stack size.
      -- The difference is due to the size taken by function calls.

      -- Update the info table data on the continuations with
      -- the selected stack formats.
      continuations' :: [Continuation CmmInfo]
      continuations' = map (applyStackFormat (snd formats')) continuations

      -- Do the actual CPS transform.
      cps_procs :: [CmmTop]
      cps_procs = map (continuationToProc formats') continuations'

      -- Convert the info tables from CmmInfo to [CmmStatic]
      -- We might want to put this in another pass eventually
      info_procs :: [RawCmmTop]
      info_procs = concat (zipWith mkInfoTable info_uniques cps_procs)

--------------------------------------------------------------------------------

-- The format for the call to a continuation
-- The fst is the arguments that must be passed to the continuation
-- by the continuation's caller.
-- The snd is the live values that must be saved on stack.
-- A Nothing indicates an ignored slot.
-- The head of each list is the stack top or the first parameter.

-- The format for live values for a particular continuation
-- All on stack for now.
-- Head element is the top of the stack (or just under the header).
-- Nothing means an empty slot.
-- Future possibilities include callee save registers (i.e. passing slots in register)
-- and heap memory (not sure if that's usefull at all though, but it may
-- be worth exploring the design space).

continuationLabel (Continuation _ l _ _) = l
data Continuation info =
  Continuation
     info --(Either C_SRT CmmInfo)   -- Left <=> Continuation created by the CPS
                       -- Right <=> Function or Proc point
     CLabel            -- Used to generate both info & entry labels
     CmmFormals        -- Argument locals live on entry (C-- procedure params)
     [BrokenBlock]     -- Code, may be empty.  The first block is
                       -- the entry point.  The order is otherwise initially 
                       -- unimportant, but at some point the code gen will
                       -- fix the order.

		       -- the BlockId of the first block does not give rise
		       -- to a label.  To jump to the first block in a Proc,
		       -- use the appropriate CLabel.

data StackFormat
    = StackFormat {
         stack_label :: Maybe CLabel,	-- The label occupying the top slot
         stack_frame_size :: WordOff,	-- Total frame size in words (not including arguments)
         stack_live :: [Maybe LocalReg]	-- local reg offsets from stack top
      }

-- A block can be a continuation of a call
-- A block can be a continuation of another block (w/ or w/o joins)
-- A block can be an entry to a function

-----------------------------------------------------------------------------

collectNonProcPointTargets ::
    UniqSet BlockId -> BlockEnv BrokenBlock
    -> UniqSet BlockId -> BlockId -> UniqSet BlockId
collectNonProcPointTargets proc_points blocks current_targets block =
    if sizeUniqSet current_targets == sizeUniqSet new_targets
       then current_targets
       else foldl (collectNonProcPointTargets proc_points blocks) new_targets targets
    where
      block' = lookupWithDefaultUFM blocks (panic "TODO") block
      targets =
        -- Note the subtlety that since the extra branch after a call
        -- will always be to a block that is a proc-point,
        -- this subtraction will always remove that case
        uniqSetToList $ (mkUniqSet $ brokenBlockTargets block') `minusUniqSet` proc_points
        -- TODO: remove redundant uniqSetToList
      new_targets = current_targets `unionUniqSets` (mkUniqSet targets)

-- TODO: insert proc point code here
--  * Branches and switches to proc points may cause new blocks to be created
--    (or proc points could leave behind phantom blocks that just jump to them)
--  * Proc points might get some live variables passed as arguments

gatherBlocksIntoContinuation ::
    UniqSet BlockId -> BlockEnv BrokenBlock
    -> BlockId -> Continuation (Either C_SRT CmmInfo)
gatherBlocksIntoContinuation proc_points blocks start =
  Continuation info_table clabel params body
    where
      children = (collectNonProcPointTargets proc_points blocks (unitUniqSet start) start) `delOneFromUniqSet` start
      start_block = lookupWithDefaultUFM blocks (panic "TODO") start
      children_blocks = map (lookupWithDefaultUFM blocks (panic "TODO")) (uniqSetToList children)
      body = start_block : children_blocks

      -- We can't properly annotate the continuation's stack parameters
      -- at this point because this is before stack selection
      -- but we want to keep the C_SRT around so we use 'Either'.
      info_table = case start_block_entry of
                     FunctionEntry info _ _ -> Right info
                     ContinuationEntry _ srt -> Left srt
                     ControlEntry -> Right CmmNonInfo

      start_block_entry = brokenBlockEntry start_block
      clabel = case start_block_entry of
                 FunctionEntry _ label _ -> label
                 _ -> mkReturnPtLabel $ getUnique start
      params = case start_block_entry of
                 FunctionEntry _ _ args -> args
                 ContinuationEntry args _ -> args
                 ControlEntry -> [] -- TODO: it's a proc-point, we could pass lives in parameter registers

--------------------------------------------------------------------------------
-- For now just select the continuation orders in the order they are in the set with no gaps

selectStackFormat :: BlockEnv CmmLive
                  -> [Continuation (Either C_SRT CmmInfo)]
                  -> [(CLabel, (Maybe CLabel, [Maybe LocalReg]))]
selectStackFormat live continuations =
    map (\c -> (continuationLabel c, selectStackFormat' c)) continuations
    where
      selectStackFormat' (Continuation
                          (Right (CmmInfo _ _ _ (ContInfo format srt)))
                          label _ _) = (Just label, format)
      selectStackFormat' (Continuation (Right _) _ _ _) = (Nothing, [])
      selectStackFormat' (Continuation (Left srt) label _ blocks) =
          -- TODO: assumes the first block is the entry block
          let ident = brokenBlockId $ head blocks -- TODO: CLabel isn't a uniquable, but we need a better way than this
          in (Just label,
              map Just $ uniqSetToList $
              lookupWithDefaultUFM live unknown_block ident)

      unknown_block = panic "unknown BlockId in selectStackFormat"

processFormats :: [(CLabel, (Maybe CLabel, [Maybe LocalReg]))]
               -> (WordOff, [(CLabel, StackFormat)])
processFormats formats = (max_size, formats')
    where
      max_size = foldl max 0 (map (stack_frame_size . snd) formats')
      formats' = map make_format formats
      make_format (label, format) =
          (label,
           StackFormat {
             stack_label = fst format,
             stack_frame_size = stack_size (snd format) +
                                if isJust (fst format)
                                then label_size
                                else 0,
             stack_live = snd format })

      -- TODO: get rid of "+ 1" etc.
      label_size = 1 :: WordOff

      stack_size [] = 0
      stack_size (Nothing:formats) = 1 + stack_size formats -- one dead word
      stack_size (Just reg:formats) = width + stack_size formats
          where
            width = machRepByteWidth (localRegRep reg) `quot` wORD_SIZE
            -- TODO: it would be better if we had a machRepWordWidth

-----------------------------------------------------------------------------
applyStackFormat :: [(CLabel, StackFormat)]
                 -> Continuation (Either C_SRT CmmInfo)
                 -> Continuation CmmInfo

-- User written continuations
applyStackFormat formats (Continuation
                          (Right (CmmInfo prof gc tag (ContInfo _ srt)))
                          label formals blocks) =
    Continuation (CmmInfo prof gc tag (ContInfo format srt))
                 label formals blocks
    where
      format = stack_live $ maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in applyStackFormat"

-- User written non-continuation code
applyStackFormat formats (Continuation (Right info) label formals blocks) =
    Continuation info label formals blocks

-- CPS generated continuations
applyStackFormat formats (Continuation (Left srt) label formals blocks) =
    Continuation (CmmInfo prof gc tag (ContInfo (stack_live $ format) srt))
                 label formals blocks
    where
      gc = Nothing -- Generated continuations never need a stack check
      -- TODO prof: this is the same as the current implementation
      -- but I think it could be improved
      prof = ProfilingInfo zeroCLit zeroCLit
      tag = if stack_frame_size format > mAX_SMALL_BITMAP_SIZE
            then rET_BIG
            else rET_SMALL
      format = maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in applyStackFormat"

-----------------------------------------------------------------------------
continuationToProc :: (WordOff, [(CLabel, StackFormat)])
                   -> Continuation CmmInfo
                   -> CmmTop
continuationToProc (max_stack, formats)
                   (Continuation info label formals blocks) =
    CmmProc info label formals (map continuationToProc' blocks)
    where
      curr_format = maybe unknown_block id $ lookup label formats
      unknown_block = panic "unknown BlockId in continuationToProc"

      continuationToProc' :: BrokenBlock -> CmmBasicBlock
      continuationToProc' (BrokenBlock ident entry stmts _ exit) =
          BasicBlock ident (prefix++stmts++postfix)
          where
            prefix = case entry of
                       ControlEntry -> []
                       FunctionEntry (CmmInfo _ (Just gc_block) _ _) _ formals ->
                           gc_stack_check gc_block max_stack ++
                           function_entry formals curr_format
                       FunctionEntry (CmmInfo _ Nothing _ _) _ formals ->
                           panic "continuationToProc: TODO generate GC block" ++
                           function_entry formals curr_format
                       FunctionEntry CmmNonInfo _ formals ->
                           panic "TODO: gc_stack_check gc_block max_stack" ++
                           function_entry formals curr_format
                       ContinuationEntry formals _ ->
                           function_entry formals curr_format
            postfix = case exit of
                        FinalBranch next -> [CmmBranch next]
                        FinalSwitch expr targets -> [CmmSwitch expr targets]
                        FinalReturn arguments ->
                            tail_call (stack_frame_size curr_format)
                                (CmmLoad (CmmReg spReg) wordRep)
                                arguments
                        FinalJump target arguments ->
                            tail_call (stack_frame_size curr_format) target arguments
                        FinalCall next (CmmForeignCall target CmmCallConv)
                            results arguments ->
                                pack_continuation curr_format cont_format ++
                                tail_call (stack_frame_size curr_format - stack_frame_size cont_format)
                                              target arguments
                            where
                              cont_format = maybe unknown_block id $
                                            lookup (mkReturnPtLabel $ getUnique next) formats
                        FinalCall next _ results arguments -> panic "unimplemented CmmCall"

-----------------------------------------------------------------------------
-- Functions that generate CmmStmt sequences
-- for packing/unpacking continuations
-- and entering/exiting functions

tail_call :: WordOff -> CmmExpr -> CmmActuals -> [CmmStmt]
tail_call spRel target arguments
  = store_arguments ++ adjust_spReg ++ jump where
    store_arguments =
        [stack_put spRel expr offset
         | ((expr, _), StackParam offset) <- argument_formats] ++
        [global_put expr global
         | ((expr, _), RegisterParam global) <- argument_formats]
    adjust_spReg =
        if spRel == 0
        then []
        else [CmmAssign spReg (CmmRegOff spReg (spRel*wORD_SIZE))]
    jump = [CmmJump target arguments]

    argument_formats = assignArguments (cmmExprRep . fst) arguments

gc_stack_check :: BlockId -> WordOff -> [CmmStmt]
gc_stack_check gc_block max_frame_size
  = check_stack_limit where
    check_stack_limit = [
     CmmCondBranch
     (CmmMachOp (MO_U_Lt $ cmmRegRep spReg)
                    [CmmRegOff spReg max_frame_size, CmmReg spLimReg])
     gc_block]

-- TODO: fix branches to proc point (we have to insert a new block to marshel the continuation)
pack_continuation :: StackFormat -> StackFormat -> [CmmStmt]
pack_continuation (StackFormat curr_id curr_frame_size _)
                       (StackFormat cont_id cont_frame_size live_regs)
  = store_live_values ++ set_stack_header where
    -- TODO: only save variables when actually needed
    -- (may be handled by latter pass)
    store_live_values =
        [stack_put spRel (CmmReg (CmmLocal reg)) offset
         | (reg, offset) <- cont_offsets]
    set_stack_header =
        if needs_header_set
        then [stack_put spRel continuation_function 0]
        else []

    -- TODO: factor with function_entry and CmmInfo.hs(?)
    cont_offsets = mkOffsets label_size live_regs

    label_size = 1 :: WordOff

    mkOffsets size [] = []
    mkOffsets size (Nothing:regs) = mkOffsets (size+1) regs
    mkOffsets size (Just reg:regs) = (reg, size):mkOffsets (size + width) regs
        where
          width = machRepByteWidth (localRegRep reg) `quot` wORD_SIZE
          -- TODO: it would be better if we had a machRepWordWidth

    spRel = curr_frame_size - cont_frame_size
    continuation_function = CmmLit $ CmmLabel $ fromJust cont_id
    needs_header_set =
        case (curr_id, cont_id) of
          (Just x, Just y) -> x /= y
          _ -> isJust cont_id

-- Lazy adjustment of stack headers assumes all blocks
-- that could branch to eachother (i.e. control blocks)
-- have the same stack format (this causes a problem
-- only for proc-point).
function_entry :: CmmFormals -> StackFormat -> [CmmStmt]
function_entry formals (StackFormat _ _ live_regs)
  = load_live_values ++ load_args where
    -- TODO: only save variables when actually needed
    -- (may be handled by latter pass)
    load_live_values =
        [stack_get 0 reg offset
         | (reg, offset) <- curr_offsets]
    load_args =
        [stack_get 0 reg offset
         | (reg, StackParam offset) <- argument_formats] ++
        [global_get reg global
         | (reg, RegisterParam global) <- argument_formats]

    argument_formats = assignArguments (localRegRep) formals

    -- TODO: eliminate copy/paste with pack_continuation
    curr_offsets = mkOffsets label_size live_regs

    label_size = 1 :: WordOff

    mkOffsets size [] = []
    mkOffsets size (Nothing:regs) = mkOffsets (size+1) regs
    mkOffsets size (Just reg:regs) = (reg, size):mkOffsets (size + width) regs
        where
          width = machRepByteWidth (localRegRep reg) `quot` wORD_SIZE
          -- TODO: it would be better if we had a machRepWordWidth

-----------------------------------------------------------------------------
-- Section: Stack and argument register puts and gets
-----------------------------------------------------------------------------
-- TODO: document

-- |Construct a 'CmmStmt' that will save a value on the stack
stack_put :: WordOff            -- ^ Offset from the real 'Sp' that 'offset'
                                -- is relative to (added to offset)
          -> CmmExpr            -- ^ What to store onto the stack
          -> WordOff            -- ^ Where on the stack to store it
                                -- (positive <=> higher addresses)
          -> CmmStmt
stack_put spRel expr offset =
    CmmStore (CmmRegOff spReg (wORD_SIZE*(spRel + offset))) expr

--------------------------------
-- |Construct a 
stack_get :: WordOff
          -> LocalReg
          -> WordOff
          -> CmmStmt
stack_get spRel reg offset =
    CmmAssign (CmmLocal reg)
              (CmmLoad (CmmRegOff spReg (wORD_SIZE*(spRel + offset)))
                       (localRegRep reg))
global_put :: CmmExpr -> GlobalReg -> CmmStmt
global_put expr global = CmmAssign (CmmGlobal global) expr
global_get :: LocalReg -> GlobalReg -> CmmStmt
global_get reg global = CmmAssign (CmmLocal reg) (CmmReg (CmmGlobal global))

