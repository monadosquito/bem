# Description

The library is to generate *classes* in accordance with a *scheme*.

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
whose type variables are types of its optional *elements*
and optional *modifiers*.
- An **entity** is either a *block*, an *element*, or a *modifier*.
- A **class** is a generated string
of a combination
of some decorated *entities*.
- A **class generator** is a [function](https://monadosquito.github.io/bem/Bem-Cls-Gen-Gen.html#v:genBlk)
from a defined
by a *scheme* combination
of some *entities*
into a *class*.
- A **scheme** is all types of *entities* together.
- An **entity part** is a capitalised substring of a shown *entity*.

## Notes

- A generated *class* is decorated according to [Table 1](#table-1).
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
- If a *modifier* is neither boolean nor non-boolean,
then it is interpreted as boolean,
trimming its fields.
- If a value of a *non-boolean modifier* is not nullary,
then all its fields are trimmed.

## Hints

- The distinguishing prefix of an *entity* should be the *entity*
that it belongs to.

## Stipulations

- 'Intr' stands for internal/intricate.
- All the examples are decorated according to [Table 1](#table-1).
- The decorations in [Table 1](#table-1) are described
regarding an *entity* as shown,
that is,
as a string.

# Installation flow

1. Include this library into
    - the `default.nix` of the dependee
        - either
        with a `callCabal2nix` function ([?](#include-library-into-default-with-callcabal2nix))
        - or
        without a `callCabal2nix` function ([?](#include-library-into-default-without-callcabal2nix))
    - and the `shell.nix` of the dependee
        - either
        with a `callCabal2nix` function ([?](#include-library-into-shell-with-callcabal2nix))
        - or
        without a `callCabal2nix` function. ([?](#include-library-into-shell-without-callcabal2nix))

# Include library

Fetching this library at an appropriate commit-ish from a repository,
include its derivation into a nix expression of the dependee.

## Include library into default with callCabal2nix

Pass the derivation of this library
as a value of the `bem` key into the third argument
passed into a `callCabal2nix` function.

### Including library into default with callCabal2nix example

`default.nix`

```nix
let
    bem
        =
        import
            (fetchTarball
                 https://github.com/monadosquito/bem/archive/<commit-ish>.tar.gz
            )
            {};
    nixpkgs = import <nixpkgs> {};
in
    {
        pkgs ? nixpkgs,
    }
    :
    nixpkgs.haskellPackages.callCabal2nix
        "<cabal file name>"
        ./.
        {
            inherit bem;
        }
```

## Include library into default without callCabal2nix

Insert the derivation of this library into the `buildInputs` list.

### example

`default.nix`

```nix
let
    bem
        =
        import
            (fetchTarball
                 https://github.com/monadosquito/bem/archive/<commit-ish>.tar.gz
            )
            {};
    nixpkgs = import <nixpkgs> {};
in
    {
        pkgs ? nixpkgs,
    }
    :
    pkgs.stdenv.mkDerivation
        {
            buildInputs = [bem];
            installPhase = "touch $out";
            name = "<project name>";
            src = ./src;
        }
```

## Include library into shell with callCabal2nix

Insert the `env` attribute
of the derivation
of this library
into the `inputsFrom` attribute
of the attribute set
passed into the `mkShell` function.

### Including library into shell with callCabal2nix example

`shell.nix`

```nix
let
    bem
        =
        import
            (fetchTarball
                 https://github.com/monadosquito/bem/archive/<commit-ish>.tar.gz
            )
            {};
    nixpkgs = import <nixpkgs> {};
in
{
    pkgs ? nixpkgs,
}
:
pkgs.mkShell
    {
        buildInputs = [pkgs.cabal-install];
        inputsFrom
            =
            [
                (pkgs.haskellPackages.callCabal2nix
                     "<cabal file name>"
                     ./.
                     {
                         inherit bem;
                     }
                ).env
            ];
    }
```

## Include library into shell without callCabal2nix

Pass a lambda
returning a list
with the derivation
of this library
into the `haskellPackages.ghcWithPackages` function.

### Including library into shell without callCabal2nix example

`shell.nix`

```nix
let
    bem
        =
        import
            (fetchTarball
                 https://github.com/monadosquito/bem/archive/<commit-ish>.tar.gz
            )
            {};
    nixpkgs = import <nixpkgs> {};
in
{
    pkgs ? nixpkgs,
}
:
pkgs.mkShell
    {
        buildInputs
            =
            [
                (pkgs.haskellPackages.ghcWithPackages (_: [bem]))
                pkgs.cabal-install
            ];
    }
```

## Notes

- If the dependee is compiled
with the GHCJS compiler,
then apply the function
from the `default.nix` file
of this library
to an attribute set
with the `withGhcjs` key set
to the `true` value.

# Usage flow

1. Define a *scheme*. ([?](#define-scheme))
2. Generate *classes*. ([?](#generate-classes))

# Define scheme

1. Define one type of *modifiers* for each *block* or *element* that has ones.
2. Derive a `Show` instance for each type of *modifiers*.
3. Define one `Type -> Type` GADT of *elements* for each *block*.
4. Derive a `Show` instance for each GADT of *elements*.
5. Define the only `(Type -> Type) -> Type -> Type` GADT of *blocks*.
6. Derive a `Show` instance for the GADT of *blocks*.

## Defining scheme example

`src/Bem/Scheme.hs`

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}


module Bem.Scheme where


import Bem.Utl.Utl
import Data.Kind


data Blk (e :: Type -> Type) m where
         Btn :: Blk NoElem BtnMod
         Header :: Blk HeaderElem HeaderMod
         Logo :: Blk NoElem NoMod
         Root :: Blk RootElem NoMod
         Search :: Blk SearchElem SearchMod
         TextInput :: Blk NoElem TextInputMod
deriving instance Show (Blk e m)

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

### Defining scheme example SASS analogue

`src/Bem/style.sass`

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

# Generate classes

Apply a corresponding *class generator*
to a defined
by a *scheme* combination
of some *entities*.

## Generating classes example

`src/Main.hs`

```haskell
import Bem.Scheme

import Bem.Bem


main :: IO ()
main = print $ genBlk Btn [Btn_Dark] Search Search_Btn [SearchBtn_Size Big]
```

## Notes

- The example
prints
the `"btn btn_dark search__btn search__btn_size_big"` string.
- The [utility *class generators*](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#t:Gens)
can be used
instead of the [full *class generators*](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html#t:Gens)
to avoid redundant modifier passing.

## Hints

- The [`decorRoot` function](https://monadosquito.github.io/bem/Bem-Utl-Utl.html#v:decorRoot) can be used
to generate a *class* for the topmost BEM block to avoid passing it as a string.

# Contributing

## Build

Run the `nix-build` command.

## Watch

Run the `nix-shell --pure --run watch` command.

## Notes

- All the commands must be run from the project root directory
or supplied with a path to it
or either the `default.nix`
or the `shell.nix` script.

# Convention

## Commits

- New [source code groups](#source-code-groups)
introduced by a new commit
are placed after old ones.

## Documentation

### Documentation structure

#### Documentation main structure

1. Description
2. Installation flow
3. Installation
4. Usage flow
5. Usage
6. Contributing
7. Convention (optional)
8. Defined scopes
9. Appendix (optional)

#### Documentation description section structure

1. Body
2. Features
3. Terms (optional)
4. Hints (optional)
5. Stipulations (optional)

#### Documentation regular section structure

1. Body
2. Notes (optional)
3. Hints (optional)

### Documentation format

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

### Documentation order

If the order
of some [parts of speech](#parts-of-speech) does not have to be specific,
then they are ordered
in the ASCII order
but ignoring special characters.

#### Documentation parts of speech

1. senctence
2. interjection
3. outermost adverbial
4. clause
5. participle phrase
6. coordinating conjunction
7. preposition

## Haskell

### Haskell format

- Local [`import`s](#source-code-groups) are separated
from external [ones](#source-code-groups) with one line.
- [Source code groups](#source-code-groups) are separated with two lines.
- [`type`s, `newtype`s, `data`s, top-level-bindings](#source-code-groups) are separated
with one line.

### Haskell modules

- The module
ending in the `<A>.<A>` suffix
is a high-level module
either exporting or reexporting high-level API beneath itself
except for the modules
ending in the `Utl.Intr`/`Utl.Utl` suffix.
- The module ending in the `<A>.Intr` suffix exports low-level API.
- The module ending in the `Utl.Intr`/`Utl.Utl` suffix exports extra utilities.

### Haskell order

- Items of a [source code group](#source-code-groups) are ordered
in the ASCII order.
- [Source code groups](#source-code-groups) are ordered
from the more abstract to the less abstract.

#### Haskell groups

1. pragmas
2. `export`s
3. `import`s
4. `type`s
5. `newtype`s
6. `data`s
7. top-level bindings

## Scopes

if changes are made inside a `src/Bem/<scope>/<scope>.hs` path,
then they are made within a *\<scope\>*.

## Notes

- A [part of speech](#parts-of-speech) is moved onto its next line
along with the [conjunction](#parts-of-speech) introducing it.
- All the [high-level utilities](https://monadosquito.github.io/bem/Bem-Utl-Utl.html)
and the [low-level utilities](https://monadosquito.github.io/bem/Bem-Utl-Intr.html) are optional,
that is,
all the features work without them.
- An image or a table has its own two-level heading of its type and its number.
- Appendix is denoted with a horizontal line and contains images and tables.
- The [source code groups](#source-code-groups) are listed
from the more abstract to the less abstract.
- `instance`s of a `newtype` or a `data` are defined
after it and before the next one and ordered in the ASCII order.

# Defined scopes

- 'gen'
- 'utl'

---

### Table 1

*Class* decorations

|Decoration                                                                                       |Value  |
|-------------------------------------------------------------------------------------------------|-------|
|a separator between a *block* and its *element*                                                  |`"__"` |
|a separator between a non-*modififer* *entity*, a *modifier*, and an optional value of the latter|`"_"`  |
|a separator between *entity parts*                                                               |`"-"`  |
|whether to capitalize *entity parts*                                                             |`False`|
