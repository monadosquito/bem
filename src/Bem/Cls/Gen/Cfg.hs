{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of a combination of some entities from a scheme
with custom decorations.
-}
module Bem.Cls.Gen.Cfg where


import Bem.Cfg.Cfg
import Bem.Cls.Gen.Intr
import Bem.Utl.Intr

import Control.Monad.Reader


-- | the configurable class generators
data Gens
         =
         Gens { _genBlk :: FromBlkElem Class
              , _genElem :: forall b . FromFullElem b Class
              }


-- | Initialise the configurable class generators with a configuration.
init :: Cfg -> Gens
init cfg
    =
    Gens { _genBlk
               =
               \blk blkMods prntBlk elem' elemMods
               ->
               runReader (genBlk blk blkMods prntBlk elem' elemMods) cfg
         , _genElem
               =
               \prntBlk elem' elemMods
               ->
               runReader (genElem prntBlk elem' elemMods) cfg
         }
