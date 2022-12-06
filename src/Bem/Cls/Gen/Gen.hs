{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}


-- | Generate a class of some entities combination from a scheme.
module Bem.Cls.Gen.Gen
           ( genBlkElem
           , genElem
           ) where


import Bem.Utl.Intr

import Data.Char
import Data.Kind
import Data.List
import Data.List.Split


type FullStredElem = String

type FullStredEnt = String

type PrefixedStredEnt = String

type StredEnt = String

type StredEntPart = String


data Ent (isMod :: Bool) where
         Blk :: Show (b e m) => b (e :: Type -> Type) m -> Ent 'False
         BoolMod :: Show m => Ent 'False -> m -> Ent 'True
         Elem
            :: (Show (b e bm), Show (e em), Show em)
            => b e bm -> e em -> Ent 'False
         Mod :: Show m => Ent 'False -> m -> Ent 'True


adopt :: FromElem b FullStredElem
adopt blk elem' = decoredBlk ++ "__" ++ decoredElem
  where
    decoredBlk = decor $ Blk blk
    decoredElem = decor $ Elem blk elem'

decor :: Ent isMod -> StredEnt
decor ent
    =
    map toLower
        $ case ent of
              Blk blk
                  ->
                  concat . sep . part . trimFlds $ show blk
              BoolMod _ boolMod
                  ->
                  concat . sep . part . trimFlds . unprefix $ show boolMod
              Elem _ elem'
                  ->
                  concat . sep . part . unprefix $ show elem'
              Mod prntEnt mod'
                  | [modName, modVal] <- words . unprefix $ show mod'
                  ->
                  (concat . sep . part) modName
                  ++
                  "_"
                  ++ (concat . sep . part) modVal
                  | otherwise
                  ->
                  decor $ BoolMod prntEnt mod'
  where
    part :: StredEnt -> [StredEntPart]
    part = tail . split (keepDelimsL $ whenElt isUpper)
    sep :: [StredEntPart] -> [String]
    sep = intersperse "-"
    trimFlds :: String -> StredEnt
    trimFlds = takeWhile (/= ' ')
    unprefix :: PrefixedStredEnt -> StredEnt
    unprefix prefixedStredEnt
        | null unprefixedEnt = prefixedStredEnt
        | otherwise = tail unprefixedEnt
      where
        unprefixedEnt = dropWhile (/= '_') prefixedStredEnt

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

str :: Ent isMod -> FullStredEnt
str ent
    =
    case ent of
        Elem blk elem' -> adopt blk elem'
        blk@(Blk _) -> decor blk 
        boolMod@(BoolMod prntEnt _)
            ->
            let
                decoredBoolMod = decor boolMod
                stredPrntEnt = str prntEnt
            in
            stredPrntEnt ++ "_" ++ decoredBoolMod
        modEnt@(Mod prntEnt mod')
            ->
            case words $ show mod' of
                [_, _]
                    ->
                    let
                        decoredMod = decor modEnt
                        stredPrntEnt = str prntEnt
                    in
                    stredPrntEnt ++ "_" ++ decoredMod
                _
                    ->
                    str $ BoolMod prntEnt mod'
