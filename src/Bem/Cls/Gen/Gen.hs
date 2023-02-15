{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of a combination of some entities from a scheme
with the default decorations.
-}
module Bem.Cls.Gen.Gen where


import qualified Bem.Cls.Gen.Intr as IntrGen
import Bem.Utl.Intr
import Bem.Cfg.Cfg

import Control.Monad.Reader


-- | Generate a class of a block and element with their modifiers.
genBlk :: FromFullBlk b (FromFullElem b Class)
genBlk blk blkMods prntBlk elem' elemMods
    =
    runReader (IntrGen.genBlk blk blkMods prntBlk elem' elemMods) defCfg
  where
    defCfg = Cfg { _elemSep = "__"
                 , _modSep = "_"
                 , _partSep = "-"
                 , _partsAreCptled = False
                 }

-- | Generate a class of a element with its modifiers.
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
