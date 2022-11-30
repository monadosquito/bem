{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of some entities combination from a scheme
using the default decorations.
-}
module Bem.Cls.Gen.Gen where


import qualified Bem.Cls.Gen.Intr as IntrGen
import Bem.Utl.Intr
import qualified Bem.Cfg.Cfg as Cfg

import Control.Monad.Reader


-- | Generate a class of a block and element along with their modifiers.
genBlkElem :: FromBlkElem Class
genBlkElem blk blkMods prntBlk elem' elemMods
    =
    runReader (IntrGen.genBlk blk blkMods prntBlk elem' elemMods) defCfg
  where
    defCfg = Cfg.Cfg { Cfg._elemSep = "__"
                     , Cfg._modSep = "_"
                     , Cfg._partSep = "-"
                     , Cfg._partsAreCptled = False
                     }

-- | Generate a class of a element along with its modifiers.
genElem :: FromFullElem b Class
genElem prntBlk elem' elemMods
    =
    runReader (IntrGen.genElem prntBlk elem' elemMods) defCfg
  where
    defCfg = Cfg.Cfg { Cfg._elemSep = "__"
                     , Cfg._modSep = "_"
                     , Cfg._partSep = "-"
                     , Cfg._partsAreCptled = False
                     }
