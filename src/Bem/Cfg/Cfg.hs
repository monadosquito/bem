{- |
general configuration

The settings are described regarding an entity as shown, that is, as a string.
-}
module Bem.Cfg.Cfg where


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
                 whether to capitalize entity parts inside a class (False)
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
