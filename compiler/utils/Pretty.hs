{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Pretty
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  David Terei <code@davidterei.com>
-- Stability   :  stable
-- Portability :  portable
--
-- John Hughes's and Simon Peyton Jones's Pretty Printer Combinators
--
-- Based on /The Design of a Pretty-printing Library/
-- in Advanced Functional Programming,
-- Johan Jeuring and Erik Meijer (eds), LNCS 925
-- <http://www.cs.chalmers.se/~rjmh/Papers/pretty.ps>
--
-----------------------------------------------------------------------------

{-
Note [Differences between libraries/pretty and compiler/utils/Pretty.hs]

For historical reasons, there are two different copies of `Pretty` in the GHC
source tree:
 * `libraries/pretty` is a submodule containing
   https://github.com/haskell/pretty. This is the `pretty` library as released
   on hackage. It is used by several other libraries in the GHC source tree
   (e.g. template-haskell and Cabal).
 * `compiler/utils/Pretty.hs` (this module). It is used by GHC only.

There is an ongoing effort in https://github.com/haskell/pretty/issues/1 and
https://ghc.haskell.org/trac/ghc/ticket/10735 to try to get rid of GHC's copy
of Pretty.

Currently, GHC's copy of Pretty resembles pretty-1.1.2.0, with the following
major differences:
 * GHC's copy uses `Faststring` for performance reasons.
 * GHC's copy has received a backported bugfix for #12227, which was
   released as pretty-1.1.3.4 ("Remove harmful $! forcing in beside",
   https://github.com/haskell/pretty/pull/35).

Other differences are minor. Both copies define some extra functions and
instances not defined in the other copy. To see all differences, do this in a
ghc git tree:

    $ cd libraries/pretty
    $ git checkout v1.1.2.0
    $ cd -
    $ vimdiff compiler/utils/Pretty.hs \
              libraries/pretty/src/Text/PrettyPrint/HughesPJ.hs

For parity with `pretty-1.1.2.1`, the following two `pretty` commits would
have to be backported:
  * "Resolve foldr-strictness stack overflow bug"
    (307b8173f41cd776eae8f547267df6d72bff2d68)
  * "Special-case reduce for horiz/vert"
    (c57c7a9dfc49617ba8d6e4fcdb019a3f29f1044c)
This has not been done sofar, because these commits seem to cause more
allocation in the compiler (see thomie's comments in
https://github.com/haskell/pretty/pull/9).
-}

module Pretty (

        -- * The document type
        Doc,

        -- * Constructing documents

        -- ** Converting values into documents
        char, text, ftext, ptext, ztext, sizedText, zeroWidthText,
        int, integer, float, double, rational, hex,

        -- ** Simple derived documents
        semi, comma, colon, space, equals,
        lparen, rparen, lbrack, rbrack, lbrace, rbrace,

        -- ** Wrapping documents in delimiters
        parens, brackets, braces, quotes, quote, doubleQuotes,
        maybeParens,

        -- ** Combining documents
        empty,
        (<>), (<+>), hcat, hsep,
        ($$), ($+$), vcat,
        sep, cat,
        fsep, fcat,
        nest,
        Pretty.hang, hangNotEmpty, punctuate,

        -- * Predicates on documents
        isEmpty,

        -- * Rendering documents

        -- ** Rendering with a particular style
        Style(..),
        style,
        renderStyle,
        Mode(..),

        -- ** General rendering
        -- fullRender,

        -- ** GHC-specific rendering
        printDoc, printDoc_,
        -- bufLeftRender -- performance hack

  ) where

import GhcPrelude hiding (error)

import FastString
import System.IO
import Numeric (showHex)

--for a RULES
import GHC.Base ( unpackCString#, unpackNBytes#, Int(..) )
import GHC.Ptr  ( Ptr(..) )

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Data.Text.Prettyprint.Doc hiding (vcat, (<+>))
import qualified Data.Text.Prettyprint.Doc as P
import qualified Data.Text.Prettyprint.Doc.Internal as PI
import Data.Text.Prettyprint.Doc.Render.Text

import  GHC.Float (float2Double)

infixl 6 <+>
infixl 5 $$, $+$

($+$) :: Doc a -> Doc a -> Doc a
($+$) a b = a <> hardlineCollapse 1 <> b

($$) :: Doc a -> Doc a -> Doc a
($$) = ($+$)

(<+>) :: Doc a -> Doc a -> Doc a
PI.Empty <+> b        = b
a        <+> PI.Empty = a
a        <+> b        = a <> char ' ' <> b

vcat :: [Doc a] -> Doc a
vcat = foldr ($$) mempty

char :: Char -> Doc a
char = pretty

text :: String -> Doc a
text = pretty

-- DEPRECATED
{-# DEPRECATED zeroWidthText "woo" #-}
zeroWidthText :: String -> Doc a
zeroWidthText s = PI.Text 0 (T.pack s)


ftext :: FastString -> Doc a
ftext = pretty . unpackFS

ptext :: LitString -> Doc a
ptext = pretty . unpackLitString

ztext :: FastZString -> Doc a
ztext = pretty . zString

-- | Some text with any width. (@text s = sizedText (length s) s@)
sizedText :: Int -> String -> Doc a
sizedText size s = PI.Text size (T.pack s)

int      :: Int      -> Doc a -- ^ @int n = text (show n)@
integer  :: Integer  -> Doc a -- ^ @integer n = text (show n)@
float    :: Float    -> Doc a -- ^ @float n = text (show n)@
double   :: Double   -> Doc a -- ^ @double n = text (show n)@
rational :: Rational -> Doc a -- ^ @rational n = text (show n)@
hex      :: Integer  -> Doc a -- ^ @rational n = text (show n)@
int       = pretty
integer   = pretty
float     = pretty
double    = pretty
rational  = pretty . show
hex       = pretty . show -- TODO

lbrack :: Doc a -- ^ A '[' character
lbrack = lbracket

rbrack :: Doc a -- ^ A ']' character
rbrack = rbracket

quotes       :: Doc a -> Doc a -- ^ Wrap document in @\'...\'@
quotes = squotes

doubleQuotes :: Doc a -> Doc a -- ^ Wrap document in @\"...\"@
doubleQuotes = dquotes

quote     :: Doc a -> Doc a-- ^Prefix document with @\'@
quote p        = char '\'' <> p


-- | Apply 'parens' to 'Doc' if boolean is true.
maybeParens :: Bool -> Doc a -> Doc a
maybeParens False = id
maybeParens True = parens

empty :: Doc a
empty = emptyDoc

-- | \"Paragraph fill\" version of 'sep'.
fsep :: [Doc a] -> Doc a
fsep = sep

fcat :: [Doc a] -> Doc a
fcat = cat

-- | Returns 'True' if the document is empty
isEmpty :: Doc a -> Bool
isEmpty PI.Empty = True
isEmpty _     = False

-- | @hang d1 n d2 = sep [d1, nest n d2]@
hang :: Doc a -> Int -> Doc a -> Doc a
hang d1 n d2 = nest n (sep [d1, d2])



-- | Apply 'hang' to the arguments if the first 'Doc' is not empty.
hangNotEmpty :: Doc a -> Int -> Doc a -> Doc a
hangNotEmpty d1 n d2 = if isEmpty d1
                       then d2
                       else Pretty.hang d1 n d2

-- | A rendering style.
data Style
  = Style { mode           :: Mode  -- ^ The rendering mode
          , lineLength     :: Int   -- ^ Length of line, in chars
          , ribbonsPerLine :: Float -- ^ Ratio of line length to ribbon length
          }

-- | The default style (@mode=PageMode, lineLength=100, ribbonsPerLine=1.5@).
style :: Style
style = Style { lineLength = 100, ribbonsPerLine = 1.5, mode = PageMode }

-- | Rendering mode.
data Mode = PageMode     -- ^ Normal
          | ZigZagMode   -- ^ With zig-zag cuts
          | LeftMode     -- ^ No indentation, infinitely long lines
          | OneLineMode  -- ^ All on one line

-- currently ignoring Mode. Need to respect this later on
styleToLayoutOptions :: Style -> LayoutOptions
styleToLayoutOptions s = case (mode s) of
                          LeftMode -> LayoutOptions Unbounded
                          _ -> LayoutOptions (AvailablePerLine (lineLength s) (float2Double (ribbonsPerLine s)))

-- | Render the @Doc@ to a String using the given @Style@.
renderStyle :: Style -> Doc a -> String
renderStyle s d = TL.unpack $ renderLazy (layoutPretty (styleToLayoutOptions s) d)

printDoc :: Mode -> Int -> Handle -> Doc a -> IO ()
-- printDoc adds a newline to the end
printDoc mode cols hdl doc = printDoc_ mode cols hdl (doc)

printDoc_ :: Mode -> Int -> Handle -> Doc a -> IO ()
printDoc_ mode pprCols hdl doc = TL.hPutStrLn hdl (renderLazy $ layoutPretty (mkLayoutOptions mode pprCols) doc) where
  mkLayoutOptions :: Mode -> Int -> LayoutOptions
  -- Note that this should technically be 1.5 as per the old implementation.
  -- I have no idea why that is.
  mkLayoutOptions PageMode pprCols = LayoutOptions (AvailablePerLine pprCols 1.0)
  mkLayoutOptions LeftMode pprCols = LayoutOptions Unbounded


