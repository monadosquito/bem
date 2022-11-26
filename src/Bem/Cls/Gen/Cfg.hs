{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of some entities combination from a scheme
using custom decorations.
-}
module Bem.Cls.Gen.Cfg where


import Bem.Cfg.Cfg
import Bem.Cls.Gen.Intr
import Bem.Utl.Intr

import Control.Monad.Reader


-- | the configurable class generators
data Gens
         =
         Gens { _genBlk :: forall b . FromFullBlk b (FromFullElem b Class)
              , _genElem :: forall b . FromFullElem b Class
              }


-- | Initialise the configurable class generators using a configuration.
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
