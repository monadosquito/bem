{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


-- | low-level extra utilities
module Bem.Utl.Intr where


import Data.Kind


{- |
a generated string of some decorated non-modifier entities combination
and their optional decorated modifiers
-}
type Class = String

-- | from an element along with its modifiers into something
type FromFullElem (b :: (Type -> Type) -> Type -> Type) r
         = forall e m pm
         . (Show (b e pm), Show (e m), Show m)
         => b e pm -> e m -> [m] -> r

-- | from a block without its modifiers into something
type FromBlk (b :: (Type -> Type) -> Type -> Type) r
         =
         forall e m . (Show (b e m), Show m) => b e m -> r

-- | from an element without its modifiers into something
type FromElem (b :: (Type -> Type) -> Type -> Type) r
         = forall e m pm
         . (Show (b e pm), Show (e m), Show m)
         => b e pm -> e m -> r

-- | from a block with its modifiers into something
type FromFullBlk (b :: (Type -> Type) -> Type -> Type) r
         =
         forall e m . (Show (b e m), Show m) => b e m -> [m] -> r

-- | from a block and an element with their modifiers into something
type FromBlkElem r = forall b . FromFullBlk b (FromFullElem b r)

{- |
from a block with its modifiers
and an element without its modifiers into something
-}
type FromBlkNoModsElem r = forall b . FromFullBlk b (FromElem b r)

{- |
from a block without its modifiers
and an element with its modifiers into something
-}
type FromNoModsBlkElem r = forall b . FromBlk b (FromFullElem b r)

-- | from a block and an element without their modifiers into something
type FromNoModsBlkNoModsElem r = forall b . FromBlk b (FromElem b r)
