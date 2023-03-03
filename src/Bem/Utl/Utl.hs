{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}


-- | high-level extra utilities
module Bem.Utl.Utl where


import Bem.Cls.Gen.Gen
import Bem.Utl.Intr

import Data.Kind

import Bem.Cls.Gen.Intr


-- | Denote absent elements.
data NoElem m

-- | Denote absent modifiers.
data NoMod deriving Show


-- | Generate a class of a block without its modifiers and full element.
genNoBlkModsBlk :: FromBlk b (FromFullElem b Class)
genNoBlkModsBlk blk prntBlk elem' elemMods
    =
    genBlkElem blk [] prntBlk elem' elemMods

-- | Generate a class of a block and element that is without its modifiers.
genNoElemModsBlk :: FromFullBlk b (FromElem b Class)
genNoElemModsBlk blk blkMods prntBlk elem'
    =
    genBlkElem blk blkMods prntBlk elem' []

-- | Generate a class of a block and element without any modifiers.
genNoModsBlk :: FromBlk b (FromElem b Class)
genNoModsBlk blk prntBlk elem' = genBlkElem blk [] prntBlk elem' []

-- | Generate a class of an element that is without its modifiers.
genNoModsElem :: FromElem b Class
genNoModsElem prntBlk elem' = genElem prntBlk elem' []

-- | Decorate a single block.
decorSingleton :: (Show (b e m)) => b (e :: Type -> Type) m -> Class
decorSingleton = decor . Blk
