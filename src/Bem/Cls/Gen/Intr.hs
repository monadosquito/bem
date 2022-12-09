{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}


-- | low-level API for class generation with custom decorations
module Bem.Cls.Gen.Intr
           ( Ent (..)
           , decor
           , str
           , FullStredEnt
           , StredEnt
           , genBlk
           , genElem
           ) where


import Bem.Cfg.Cfg
import Bem.Utl.Intr

import Control.Monad.Reader
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


adopt :: FromElem b (Reader Cfg FullStredElem)
adopt blk elem' = do
    Cfg {..} <- ask
    decoredBlk <- decor $ Blk blk
    decoredElem <- decor $ Elem blk elem'
    return $ decoredBlk ++ _elemSep ++ decoredElem

{- |
Decorate an entity,
wrapped into a corresponding 'Ent' data constructor,
using custom decorations
-}
decor :: Ent isMod -> Reader Cfg StredEnt
decor ent = do
    Cfg {..} <- ask
    let
        sep :: [StredEntPart] -> [String]
        sep = intersperse _partSep
    decoredEnt <- case ent of
                      Blk blk
                          ->
                          return . concat . sep . part . trimFlds $ show blk
                      BoolMod _ boolMod
                          ->
                          return . concat . sep . part . trimFlds . unprefix
                              $ show boolMod
                      Elem _ elem'
                          ->
                          return . concat . sep . part . unprefix $ show elem'
                      Mod prntEnt mod'
                          | [modName, modVal] <- words . unprefix $ show mod'
                          ->
                          return
                              $ (concat . sep . part) modName
                              ++
                              _modSep
                              ++ (concat . sep . part) modVal
                          | otherwise
                          ->
                          decor $ BoolMod prntEnt mod'
    return $ if _partsAreCptled then decoredEnt else map toLower decoredEnt
  where
    part :: StredEnt -> [StredEntPart]
    part = tail . split (keepDelimsL $ whenElt isUpper)
    trimFlds :: String -> StredEnt
    trimFlds = takeWhile (/= ' ')
    unprefix :: PrefixedStredEnt -> StredEnt
    unprefix prefixedStredEnt
        | null unprefixedEnt = prefixedStredEnt
        | otherwise = tail unprefixedEnt
      where
        unprefixedEnt = dropWhile (/= '_') prefixedStredEnt

{- |
Generate a class of a block and element with their modifiers,
using custom decorations.
-}
genBlk :: FromBlkElem (Reader Cfg Class)
genBlk blk blkMods prntBlk elem' elemMods
    = do
    decoredBlk <- str $ Blk blk
    decoredBlkMods <- forM blkMods $ str . Mod (Blk blk)
    decoredFullElem <- genElem prntBlk elem' elemMods
    return
        $ decoredBlk
        ++ (if null decoredBlkMods then "" else " ")
        ++ unwords decoredBlkMods
        ++ " "
        ++ decoredFullElem

-- | Generate a class of a element with its modifiers, using custom decorations.
genElem :: FromFullElem b (Reader Cfg Class)
genElem prntBlk elem' elemMods
    = do
    adoptedElem <- str $ Elem prntBlk elem'
    decoredElemMods <- forM elemMods $ str . Mod (Elem prntBlk elem')
    return
        $ adoptedElem
        ++ (if null decoredElemMods then "" else " ")
        ++ unwords decoredElemMods

{- |
the topmost helper function gathering all the other functions
to stringify a given wrapped entity
-}
str :: Ent isMod -> Reader Cfg FullStredEnt
str ent = do
    Cfg {..} <- ask
    case ent of
        Elem blk elem' -> adopt blk elem'
        blk@(Blk _) -> decor blk 
        boolMod@(BoolMod prntEnt _) -> do
            decoredBoolMod <- decor boolMod
            stredPrntEnt <- str prntEnt
            return $ stredPrntEnt ++ _modSep ++ decoredBoolMod
        modEnt@(Mod prntEnt mod')
            ->
            case words $ show mod' of
                [_, _]
                    -> do
                    decoredMod <- decor modEnt
                    stredPrntEnt <- str prntEnt
                    return $ stredPrntEnt ++ _modSep ++ decoredMod
                _
                    -> do
                    str $ BoolMod prntEnt mod'
