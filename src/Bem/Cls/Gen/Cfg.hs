{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of a combination of some entities from a scheme
with custom decorations.
-}
module Bem.Cls.Gen.Cfg where


import Bem.Cfg.Intr
import Bem.Cls.Gen.Intr
import Bem.Utl.Intr

import qualified Control.Monad.Reader as Rdr


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
               Rdr.runReader
                   (genBlk blk blkMods prntBlk elem' elemMods)
                   fixedCfg
         , _genElem
               =
               \prntBlk elem' elemMods
               ->
               Rdr.runReader (genElem prntBlk elem' elemMods) fixedCfg
         }
  where
    fixedCfg = fix cfg
