{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}


-- | low-level API for class generation
module Bem.Cls.Gen.Intr
           ( Ent (..)
           , decor
           , str
           , FullStredEnt
           , StredEnt
           ) where


import Bem.Utl.Intr

import Data.Char
import Data.Kind
import Data.List
import Data.List.Split


type FullStredElem = String

-- | a final decorated entity with everyting needed attached
type FullStredEnt = String

type PrefixedStredEnt = String

-- | a decorated entity
type StredEnt = String

type StredEntPart = String


-- | the GADT of wrappers allowing entities to be of the same type
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

{- |
Decorate an entity,
wrapped into a corresponding 'Ent' data constructor,
using the default decorations
-}
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

{- |
the topmost helper function gathering all the other functions
to stringify a given wrapped entity
-}
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
