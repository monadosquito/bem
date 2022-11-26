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
genBlkElem :: FromFullBlk b (FromFullElem b Class)
genBlkElem blk blkMods prntBlk elem' elemMods
    =
    decoredBlk
    ++ (if null decoredBlkMods then "" else " ")
    ++ unwords decoredBlkMods
    ++ " "
    ++ decoredFullElem
  where
    decoredBlk = runReader (IntrGen.str $ IntrGen.Blk blk) defCfg
    decoredBlkMods
        =
        runReader
            (mapM (IntrGen.str . IntrGen.Mod (IntrGen.Blk blk)) blkMods)
            defCfg
    decoredFullElem = genElem prntBlk elem' elemMods
    defCfg = Cfg { _elemSep = "__"
                 , _modSep = "_"
                 , _partSep = "-"
                 , _partsAreCptled = False
                 }

-- | Generate a class of a element along with its modifiers.
genElem :: FromFullElem b Class
genElem prntBlk elem' elemMods
    =
    adoptedElem
    ++ (if null decoredElemMods then "" else " ")
    ++ unwords decoredElemMods
  where
    adoptedElem = runReader (IntrGen.str $ IntrGen.Elem prntBlk elem') defCfg
    decoredElemMods
        =
        runReader
            (mapM
                 (IntrGen.str . IntrGen.Mod (IntrGen.Elem prntBlk elem'))
                 elemMods
            )
            defCfg
    defCfg = Cfg { _elemSep = "__"
                 , _modSep = "_"
                 , _partSep = "-"
                 , _partsAreCptled = False
                 }
