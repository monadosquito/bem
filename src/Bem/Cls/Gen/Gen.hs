{-# LANGUAGE RankNTypes #-}


{- |
Generate a class of some entities combination from a scheme
using the default decorations.
-}
module Bem.Cls.Gen.Gen where


import Bem.Cls.Gen.Intr
import Bem.Utl.Intr


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
    decoredBlk = str $ Blk blk
    decoredBlkMods = map (str . Mod (Blk blk)) blkMods 
    decoredFullElem = genElem prntBlk elem' elemMods

-- | Generate a class of a element along with its modifiers.
genElem :: FromFullElem b Class
genElem prntBlk elem' elemMods
    =
    adoptedElem
    ++ (if null decoredElemMods then "" else " ")
    ++ unwords decoredElemMods
  where
    adoptedElem = str $ Elem prntBlk elem'
    decoredElemMods = map (str . Mod (Elem prntBlk elem')) elemMods
