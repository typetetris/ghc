{-# LANGUAGE TypeFamilies, PolyKinds #-}

module T15704 where

data family D :: k

type family F (a :: k) :: *

type instance F D = Int
type instance F (D a) = Char
