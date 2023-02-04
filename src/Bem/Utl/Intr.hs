{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


-- | the extra low-level utilities
module Bem.Utl.Intr where


import Data.Kind


{- |
a generated string
of a combination
of some decorated non-modifier entities and their optional decorated modifiers
-}
type Class = String

-- | from an element with its modifiers into something
type FromFullElem (b :: (Type -> Type) -> Type -> Type) r
         = forall e m pm
         . (Show (b e pm), Show (e m), Show m)
         => b e pm -> e m -> [m] -> r
