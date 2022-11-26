{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of a combination of some entities from a scheme
with the default decorations.
-}
module Bem.Cls.Gen.Gen where


import qualified Bem.Cls.Gen.Intr as IntrGen
import Bem.Utl.Intr
import Bem.Cfg.Cfg
import Bem.Cfg.Intr

import qualified Control.Monad.Reader as Rdr


-- | Generate a class of a block and element with their modifiers.
genBlk :: FromBlkElem Class
genBlk blk blkMods prntBlk elem' elemMods
    =
    Rdr.runReader (IntrGen.genBlk blk blkMods prntBlk elem' elemMods)
        $ fix defCfg

-- | Generate a class of a element with its modifiers.
genElem :: FromFullElem b Class
genElem prntBlk elem' elemMods
    =
    Rdr.runReader (IntrGen.genElem prntBlk elem' elemMods) $ fix defCfg
