{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of some entities combination from a scheme
using the default decorations.
-}
module Bem.Cls.Gen.Gen where


import qualified Bem.Cls.Gen.Intr as IntrGen
import Bem.Utl.Intr
import Bem.Cfg.Cfg

import Control.Monad.Reader


-- | Generate a class of a block and element along with their modifiers.
genBlkElem :: FromBlkElem Class
genBlkElem blk blkMods prntBlk elem' elemMods
    =
    runReader (IntrGen.genBlk blk blkMods prntBlk elem' elemMods) defCfg
  where
    defCfg = Cfg { _elemSep = "__"
                 , _modSep = "_"
                 , _partSep = "-"
                 , _partsAreCptled = False
                 }

-- | Generate a class of a element along with its modifiers.
genElem :: FromFullElem b Class
genElem prntBlk elem' elemMods
    =
    runReader (IntrGen.genElem prntBlk elem' elemMods) defCfg
  where
    defCfg = Cfg { _elemSep = "__"
                 , _modSep = "_"
                 , _partSep = "-"
                 , _partsAreCptled = False
                 }
