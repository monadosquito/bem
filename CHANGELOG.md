# [Unreleased]

# [1.0.0] - 2023-07-01

## Added

- The [partial *class* generators](https://monadosquito.github.io/bem/Bem-Utl-Utl.html) can be used
instead
of the [regular ones](https://monadosquito.github.io/bem/Bem-Gen-Gen.html)
to avoid passing corresponding *modifiers*.
- Defining an own type to denote absent *elements* can be avoided
passing the [`NoElem` empty type constructor](https://monadosquito.github.io/bem/Bem-Gen-Gen.html)
in place a type of them.
- Defining an own type to denote absent *modifiers* can be avoided
passing the [`NoMod` empty type](https://monadosquito.github.io/bem/Bem-Gen-Gen.html)
in place of a type of them.
- *Classes* can be generated
in accordance
with a *scheme*
using the [`genBlk`](https://monadosquito.github.io/bem/Bem-Gen-Gen.html)
and [`genElem`](https://monadosquito.github.io/bem/Bem-Gen-Gen.html) functions.
