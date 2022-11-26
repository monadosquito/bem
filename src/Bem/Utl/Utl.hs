{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RecordWildCards #-}


-- | high-level extra utilities
module Bem.Utl.Utl where


import qualified Bem.Cls.Gen.Gen as Gen
import Bem.Cfg.Cfg
import Bem.Utl.Intr
import qualified Control.Monad.Reader as Rdr
import qualified Bem.Cls.Gen.Cfg as Cfg
import qualified Bem.Cls.Gen.Intr as IntrGen

import Data.Kind


-- | the configurable extra class generators
data Gens = Gens { _genBlk :: FromBlkElem Class
                 , _genElem :: forall b . FromFullElem b Class
                 , _genNoBlkModsBlk
                       :: forall b
                       . FromBlk b (FromFullElem b Class)
                 , _genNoElemModsBlk :: FromBlkNoModsElem Class
                 , _genNoModsBlk :: FromNoModsBlkNoModsElem Class
                 , _genNoModsElem :: forall b . FromElem b Class
                 }

-- | Denote absent elements.
data NoElem m

-- | Denote absent modifiers.
data NoMod deriving Show


-- | Generate a class of a block without its modifiers and full element.
genNoBlkModsBlk :: FromNoModsBlkElem Class
genNoBlkModsBlk blk prntBlk elem' elemMods
    =
    Gen.genBlkElem blk [] prntBlk elem' elemMods

-- | Generate a class of a block and element that is without its modifiers.
genNoElemModsBlk :: FromBlkNoModsElem Class
genNoElemModsBlk blk blkMods prntBlk elem'
    =
    Gen.genBlkElem blk blkMods prntBlk elem' []

-- | Generate a class of a block and element without any modifiers.
genNoModsBlk :: FromNoModsBlkNoModsElem Class
genNoModsBlk blk prntBlk elem' = Gen.genBlkElem blk [] prntBlk elem' []

-- | Generate a class of an element that is without its modifiers.
genNoModsElem :: FromElem b Class
genNoModsElem prntBlk elem' = Gen.genElem prntBlk elem' []

-- | Decorate a single block.
decorSingleton :: (Show (b e m)) => b (e :: Type -> Type) m -> Class
decorSingleton = (`Rdr.runReader` (fix defCfg)) . (IntrGen.decor . IntrGen.Blk)

-- | Initialise the utility configurable class generators with a configuration.
init :: Cfg -> Gens
init cfg
    =
    Gens { _genBlk = _genBlk
         , _genElem = _genElem
         , _genNoBlkModsBlk
               =
               \blk prntBlk elem' elemMods
               ->
               Rdr.runReader
                   (IntrGen.genBlk blk [] prntBlk elem' elemMods)
                   fixedCfg
         , _genNoElemModsBlk
               =
               \blk blkMods prntBlk elem'
               ->
               Rdr.runReader
                   (IntrGen.genBlk blk blkMods prntBlk elem' [])
                   fixedCfg
         , _genNoModsBlk
               =
               \blk prntBlk elem'
               ->
               Rdr.runReader (IntrGen.genBlk blk [] prntBlk elem' []) fixedCfg
         , _genNoModsElem
               =
               \prntBlk elem'
               ->
               Rdr.runReader (IntrGen.genElem prntBlk elem' []) fixedCfg
         }
  where
    Cfg.Gens {..} = Cfg.init cfg
    fixedCfg = fix cfg
