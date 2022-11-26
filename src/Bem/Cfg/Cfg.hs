{-# LANGUAGE NamedFieldPuns #-}


{- |
general configuration

The settings are described regarding an entity as shown, that is, as a string.
-}
module Bem.Cfg.Cfg
           ( Cfg (..)
           , FixedCfg
           , Sep
           , defCfg
           , fix
           , unfix
           ) where


import Data.Char


-- | an entity part separator inside a class
type Sep = String


-- | the available settings (along with their default values)
data Cfg = Cfg {
                 {- |
                 a separator between a block and its element
                 inside a class ("__")
                 -}
                 _elemSep :: Sep
                 {- |
                 a separator between a non-modififer entity, a modifier,
                 and an optional value of the latter
                 inside a class ("_")
                 -}
               , _modSep :: Sep
                 {- |
                 a separator between entity parts inside a class ("-")
                 -}
               , _partSep :: Sep
                 {- |
                 whether to capitalise entity parts inside a class (False) 
                 -}
               , _partsAreCptled :: Bool
               }


-- | the default settings
defCfg :: Cfg
defCfg = Cfg { _elemSep = "__"
             , _modSep = "_"
             , _partSep = "-"
             , _partsAreCptled = False
             }


{- |
the safety wrapper around configuration
whose value can only be created using the 'fix' function
-}
newtype FixedCfg = FixedCfg Cfg


{- |
If a particular combination of setting values is occuried,
then some settings are defaulted back to avoid malformed custom configurations.
-}
fix :: Cfg -> FixedCfg
fix
    =
    FixedCfg
    . fixAllSepsIsEq
    . fixElemSepAndNamePartSepIsEq
    . fixElemSepAndModSepIsEq
    . fixModSepAndNamePartSepIsEq
    . fixNoSeptn
  where
    fixAllSepsIsEq cfg@Cfg { _elemSep = elemSep
                           , _modSep = modSep
                           , _partSep = namePartSep
                           }
        | elemSep == modSep && modSep == namePartSep
        = cfg { _elemSep = _elemSep defCfg
              , _modSep = _modSep defCfg
              , _partSep = _partSep defCfg
              }
        | otherwise = cfg
    fixElemSepAndModSepIsEq cfg@Cfg { _elemSep = elemSep
                                    , _modSep = modSep
                                    }
        | elemSep == modSep = cfg { _elemSep = _elemSep defCfg
                                  , _modSep = _modSep defCfg
                                  }
        | otherwise = cfg
    fixElemSepAndNamePartSepIsEq cfg@Cfg { _elemSep = elemSep
                                         , _partSep = namePartSep
                                         }
        | elemSep == namePartSep = cfg { _elemSep = _elemSep defCfg
                                       , _partSep = _partSep defCfg
                                       }
        | otherwise = cfg
    fixModSepAndNamePartSepIsEq cfg@Cfg { _modSep = modSep
                                        , _partSep = namePartSep
                                        }
        | modSep == namePartSep = cfg { _modSep = _modSep defCfg
                                      , _partSep = _partSep defCfg
                                      }
        | otherwise = cfg
    fixNoSeptn cfg@Cfg { _partSep = partSep
                       , _partsAreCptled
                       }
        | any isLetter partSep = defedCfg
        | False <- _partsAreCptled
        , null partSep = defedCfg
        | otherwise = cfg
      where
        defedCfg = cfg { _partSep = _partSep defCfg
                       }

-- | Obtain an actual configuration after fixing it.
unfix :: FixedCfg -> Cfg
unfix (FixedCfg cfg) = cfg
