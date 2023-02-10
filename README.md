# Description

The `bem` library is to generate *classes* in accordance with a *scheme*.

## Features

- to eliminate chances to misspell an *entity* because it is a type,
not a string
- to make a combination of *entities* impossible
unless it is defined by a *scheme*

## Terms

- A **modifier** is a data constructor of a nullary sum type.
- A **boolean modifier** is a nullary *modifier*.
- A **non-boolean modifier** is a unary *modifier*
whose only field is of a nullary sum type of nullary data constructors
that are its values.
- An **element** is a data constructor of a `Type -> Type` GADT
whose only type variable is a type of its optional *modifiers*.
- A **block** is a data constructor
of the only `(Type -> Type) -> Type -> Type` GADT
whose two type variables are types of its optional *elements*
and its optional *modifiers*,
respectively.
- An **entity** is either a *block*, an *element*, or a *modifier*.
- A **class** is a generated string
of a combination
of some decorated *entities*.
- A **generator** is a field-function,
using custom decorations,
or a function,
using the default decorations,
from some *entities* combination defined by a *scheme* into something
being or having a *class*.
- A **class generator** is a *generator* into a *class*.
- A **scheme** is all types of *entities* together.
- An **entity part** is a capitalised substring of a shown *entity*.

## Notes

- A generated *class* is decorated according to a [configuration](#table-1).
- A *scheme* defines how and which *entities* can be combined.
- Absence of any *elements*
or *modifiers* can be represented by any other appropriate type
that is empty
or its values are shown as the empty string
in place of a GADT or a nullary sum type of them, respectively.
- *Elements*
that are alike or *block*-alike and *modifiers*
that are alike
must be prefixed with the prefix distinguishing them and
ending in the `"_"` suffix
to avoid definition conflicts.
(The prefix is trimmed from a generated *class*.)
- The [`bem-example`](https://github.com/monadosquito/bem-example) repository is the working source code of all the examples
presented in this document.
- If a *modifier* is neither boolean nor non-boolean,
then it is interpreted as boolean,
trimming its fields.
- If a value of a *non-boolean modifier* is not nullary,
then all its fields are trimmed.
- In order that the [regular `Gens` field-functions](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens)
or the [utility `Gens` field-functions](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#t:Gens),
using custom decorations can be used,
they must be configured. ([?](#configure-class-generators))

## Hints

- The distinguishing prefix of an *entity* should be the *entity*
that it belongs to.

## Stipulations

- 'Intr' stands for internal/intricate.
- All the examples are decorated according to the [default settings](#table-1).
- The default settings are described in [Table 1](#table-1).
- The settings in [Table 1](#table-1) are described
regarding an *entity* as shown,
that is,
as a string.

# Installation flow

Include this library into
- the .cabal file ([?](#include-library-into-cabal-file)),
- the `default.nix`,
    - either using the `cabal-install` tool ([?](#include-library-into-default-using-cabal))
    - or not using it ([?](#include-library-into-default-not-using-cabal))
- and the `shell.nix` of the dependee
    - either using the `cabal-install` tool ([?](#include-library-into-shell-using-cabal))
    - or the `cabal-install` tool. ([?](#include-library-into-shell-not-using-cabal))

# Include library

Fetching this library at an appropriate commit-ish from a repository,
include its derivation into a nix expression of the dependee.

## Into default using cabal

1. Include the `bem` package
into the `build-depends` field
of the .cabal file
of the dependee.
2. Pass the derivation of this library
as a value of the `bem` key into the third argument
passed into a `callCabal2nix` function.

## Into default not using cabal

Include the derivation of this library into the `buildInputs` list.

## Into shell using cabal

Include the `env` attribute
of the derivation
of this library
into the `inputsFrom` attribute
of the attribute set
passed into the `mkShell` function.

### Example

`shell.nix`
<!-- cabal 'shell.nix' -->
```nix
let
    dvg-git = pin.dvg-git {};
    fetch-hash = pin.fetch-hash {};
    pin = import ./chr/pin.nix;
    traverse = pin.traverse {};
    nixpkgs = pin.nixpkgs {};
    bem = pin.bem {};
    ghcNixpkgs = pin.ghcNixpkgs {};
    miso = pin.miso {};
in
{
    pkgs ? nixpkgs,
    ghcPkgs ? ghcNixpkgs,
}
:
pkgs.mkShell
    {
        buildInputs
            =
            [
                dvg-git
                fetch-hash
                traverse
                (pkgs.writeShellScriptBin
                     "watch"
                     (pkgs.lib.readFile ./scr/watch.sh)
                )
                pkgs.ghcid
            ];
        inputsFrom
            =
            [
                (miso.pkgs.haskell.packages.ghc865.callCabal2nix
                     "bem-example"
                     ./.
                     {
                         inherit bem;
                     }
                ).env
            ];
    }
```

## Into shell not using cabal

Pass a lambda
returning a list
with the derivation
of this library
into a `ghcWithPackages` function.

### Example

`shell.nix`
<!-- no-cabal 'shell.nix' -->
```nix
let
    dvg-git = pin.dvg-git {};
    fetch-hash = pin.fetch-hash {};
    pin = import ./chr/pin.nix;
    traverse = pin.traverse {};
    nixpkgs = pin.nixpkgs {};
    bem = pin.bem {};
    ghcNixpkgs = pin.ghcNixpkgs {};
in
{
    pkgs ? nixpkgs,
    ghcPkgs ? ghcNixpkgs,
}
:
pkgs.mkShell
    {
        buildInputs
            =
            [
                dvg-git
                fetch-hash
                traverse
                (pkgs.writeShellScriptBin
                     "watch"
                     (pkgs.lib.readFile ./scr/watch.sh)
                )
                pkgs.ghcid
                (ghcPkgs.haskell.packages.ghc865.ghcWithPackages (_: [bem]))
            ];
    }
```

## Notes

- If the dependee is compiled
with the `GHCJS` compiler,
then apply the function
from the `default.nix` file
of this library
to an attribute set
with the `useGhcjs` key set
to the `true` value.

# Usage flow

1. Define a *scheme*. ([?](#define-scheme))
2. Generate *classes*
with the [default decorations](#table-1) ([?](#generate-classes-with-default-decorations))
    1. or configure ([?](#configure-class-generators))
        - either the [regular *class generators*](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens)
        - or the [utility `Gens` *class generators*](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#t:Gens)
    2. and generate *classes* with [custom decorations](#table-1).
    ([?](#generate-classes-with-custom-decorations))

# Define scheme

1. Define one type of *modifiers* for each *block* or *element* that has ones.
2. Derive a `Show` instance for each type of *modifiers*.
3. Define one `Type -> Type` GADT of *elements* for each *block*.
4. Derive a `Show` instance for each GADT of *elements*.
5. Define an only `(Type -> Type) -> Type -> Type` GADT of *blocks*.
6. Derive a `Show` instance for the GADT of *blocks*.

## Example

`src/Bem/Scheme.hs`
<!-- 'src/Bem/Scheme.hs' -->
```hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Bem.Scheme where


import Bem.Utl.Utl
import Data.Kind


data Blk (e :: Type -> Type) b where
         Btn :: Blk NoElem BtnMod
         Header :: Blk HeaderElem HeaderMod
         Logo :: Blk NoElem NoMod
         Root :: Blk RootElem NoMod
         Search :: Blk SearchElem SearchMod
         TextInput :: Blk NoElem TextInputMod
deriving instance Show (Blk e b)

data BtnMod = Btn_Dark deriving Show

data HeaderMod = Header_Dark deriving Show

data HeaderElem m where
         Header_Logo :: HeaderElem NoMod
         Header_Search :: HeaderElem NoMod
deriving instance Show (HeaderElem m)

data RootElem m where
         Root_Header :: RootElem NoMod
         Root_Logo :: RootElem NoMod
deriving instance Show (RootElem m)

data SearchBtnMod = SearchBtn_Size Size deriving Show

data SearchTextInputMod = SearchTextInput_Size Size deriving Show

data SearchElem m where
         Search_Btn :: SearchElem SearchBtnMod
         Search_TextInput :: SearchElem SearchTextInputMod
deriving instance Show (SearchElem m)

data SearchMod = Search_Dark deriving Show

data Size = Big | Small deriving Show

data TextInputMod = TextInput_Dark deriving Show
```

### SASS analogue

`src/Bem/style.sass`
<!-- 'src/Bem/style.sass' -->
```sass
.btn
.btn_dark
.header
.header_dark
.header__search
.logo
.logo_dark
.search
.search_dark
.search__btn
.search__btn_size_big
.search__btn_size_small
.text-input
.text-input_dark
```

## Notes

- Absence of the dark *modifier* on a block `search__btn` *element* would mean
that it is light,
which is set directly on it.
- Absence of the size *modifier* on a `search__btn` *element* would mean
that it is of the medium size,
which is set directly on it.
- Derive a `Show` instance for a GADT using the `StandaloneDeriving` extension.

## Hints
    
- The [`NoElem` empty type constructor](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#t:NoElem) can be used
to denote absent *elements*.
- The [`NoMod` empty type](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#t:NoMod) can be used
to denote absent *elements*/*modifiers*.

# Configure class generators

Apply the [`init` function](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#v:init)
to an appropriate [`Cfg` record](https://monadosquito.github.io/bem/Bem-Cfg-Cfg.html#t:Cfg)
assignining the result a name
in order that
the resulting configured [*class generators*](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens) can be referenced
with it.

## Example

`src/Bem/Init.hs`
<!-- 'src/Bem/Init.hs' -->
```hs
module Bem.Init where


import Bem.Cfg.Cfg
import qualified Bem.Cls.Gen.Cfg as Bem


gens :: Bem.Gens
gens = Bem.init defCfg
```

## Hints

- In order that the resulting [`Gens` record](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens) contains the [utility *class generators*](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#v:init),
make it
using the [utility `init` function](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#v:init)
in place
of the [regular one](https://monadosquito.github.io/bem/Bem-Gen-Cfg.html).

# Generate classes

## Using default decorations

Apply a corresponding [*class generator*](https://monadosquito.github.io/bem/Bem-Cls-Gen-Gen.html)
to an *entities* combination
defined
by a *scheme*.

### Example

`src/Main.hs`
<!-- 'src/DefDecorMain.hs' -->
```hs
import Bem.Scheme

import Bem.Bem
import Bem.Utl.Utl


main :: IO ()
main = do
    print $ genBlk Btn [Btn_Dark] Search Search_Btn [SearchBtn_Size Big]
    print $ decorSingleton Root
```

## Using custom decorations

Apply a corresponding configured [*class generator*](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens),
referencing its [`Gens` record](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens)
with the name
assigned during [configuring](#configure-class-generators),
to a combination
of some *entities*
from a *scheme*.

### Example

`src/Main.hs`
<!-- 'src/Main.hs' -->
```hs
import Bem.Scheme

import qualified Bem.Bem as Bem
import Bem.Utl.Utl

import Bem.Init


main :: IO ()
main = do
    print $ Bem._genBlk gens
                Btn [Btn_Dark] Search Search_Btn [SearchBtn_Size Big]
    print $ decorSingleton Root
```

## Notes

- The example prints the `"btn btn_dark search__btn search__btn_size_big"` string.
- The [utility *class generators*](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#t:Gens) can be used
instead of the [regular *class generators*](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens)
to avoid redundant modifier passing.

## Hints

- The [`decorSingleton` function](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#v:decorSingleton) can be used
to generate a *class* for the topmost BEM block to avoid passing it as a string.

# Contributing

## Build

Run the `nix-build` command.

## Interpret

Run the `nix-shell --pure --run watch` command.

## Notes

- All the commands must be executed from the project root directory
or supplied with a path to it
or either the `default.nix`
or the `shell.nix` script.

# Convention

## Commits

- New [source code groups](#source-code-groups)
introduced by a new commit
are placed after old ones.

## Documentation

### Main structure

1. Description
2. Installation flow
3. Installation
4. Usage flow
5. Usage
6. Contributing (optional)
7. Convention (optional)
8. Defined scopes (optional)
9. Appendix (optional)

### Description section structure

1. Body
2. Features
3. Terms (optional)
4. Hints (optional)
5. Stipulations (optional)

### Regular section structure

1. Body
2. Notes (optional)
3. Hints (optional)

### Format

- A too long [part of speech](#parts-of-speech) is split
moving each occurrence
of its inner [part of speech](#parts-of-speech)
onto the next line along with the ending comma
and the rest
of it
onto the line after the next line
till it fits 80 characters
or no inner [part of speech](#parts-of-speech) is left
to split on.

### Order

If the order
of some [parts of speech](#parts-of-speech) does not have to be specific,
then they are ordered
in the ASCII order
but ignoring special characters.

### Parts of speech

1. sentence
2. interjection
3. adverbial
4. clause
5. participle phrase
6. coordinating conjunction
7. preposition
8. coordinate adjective

## Haskell

### Format

- Local [`import`s](#source-code-groups) are separated
from external [ones](#source-code-groups) with one line.
- [Source code groups](#source-code-groups) are separated with two lines.
- [`type`s, `newtype`s, `data`s, top-level-bindings](#source-code-groups) are separated
with one line.

### Modules

- The module
ending in the `<A>.<A>` suffix
is a high-level module
either exporting or reexporting high-level API beneath itself
except for the modules
ending in the `Utl.Intr` or `Utl.Utl` suffix.
- The module
ending in the `<A>.<B>` suffix
is the module
ending in the `<A>.<A>` suffix
extended with the capabilities of the module
ending in the `<B>.<B>` suffix.
- The module
ending in the `<A>.Intr` suffix is a low-level module
exporting low-level API for the module
- The module
ending in the `Utl.Intr` or `Utl.Utl` suffix is a utility module
exporting extra utilities.
- If an `import` of the local low-level module
ending in the `<A>.Intr` suffix
introduces definition conflicts,
then it is `qualified as Intr<A>`.
- If an `import` of the local high-level module
ending in the `<A>` suffix
introduces definition conflicts,
then it is `qualified as <A>`.

### Order

- Items of a [source code group](#source-code-groups) are ordered
in the ASCII order.
- [Source code groups](#source-code-groups) are ordered
from the more abstract to the less abstract,
- `qualified` [`import`s](#source-code-groups) are placed
beneath non-`qualified` ones.

#### Groups

1. file-header pragmas
2. `export`s
3. `import`s
4. `type`s
5. `newtype`s
6. `data`s
7. top-level bindings

## Scopes

if changes are made
inside a `src/Bem/<scope>/<scope>.hs`/`src/Bem/<A>/<scope>.hs` path,
then they are made
within a *\<scope\>*.

## Notes

- A [part of speech](#parts-of-speech) is moved onto its next line
along with the [conjunction](#parts-of-speech) introducing it.
- All the [high-level utilities](https://monadosquito.github.io/bem/Bem-Utl-Utl.html)
and the [low-level utilities](https://monadosquito.github.io/bem/Bem-Utl-Intr.html) are optional,
that is,
all the features work without them.
- An image or a table has its own two-level heading of its type and number.
- Appendix, denoted with a horizontal line, contains images and tables.
- The [source code groups](#source-code-groups) are listed
from the more abstract to the less abstract.
- `instance`s of a `newtype` or a `data` are defined
after it and before the next one and ordered in the ASCII order.

# Defined scopes

- cfg
- cls
- utl

---

## Table 1

the available settings

|Setting          |Description                                                                                      |Default value|
|-----------------|-------------------------------------------------------------------------------------------------|-------------|
|`_elemSep`       |a separator between a *block* and its *element*                                                  |`"__"`       |
|`_modSep`        |a separator between a non-*modififer* *entity*, a *modifier*, and an optional value of the latter|`"_"`        |
|`_partSep`       |a separator between *entity parts*                                                               |`"-"`        |
|`_partsAreCptled`|whether to capitalize *entity parts*                                                             |`False`      |
