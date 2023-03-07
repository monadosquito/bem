# [Unreleased]

# [1.0.0] - 2023-07-01

## Added

- If a particular combination of [decorations](https://github.com/monadosquito/bem/#table-1) is occuried,
then some of them are defaulted back
to avoid [malformed custom decorations](https://github.com/monadosquito/bem/#table-2).
- The [`defCfg` value](https://monadosquito.github.io/bem/Bem-Cfg-Cfg.html) can be passed
into the [regular `init` function](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html)
or the [utility `init` function](https://monadosquito.github.io/bem/Bem-Utl-Utl.html)
to avoid setting up an entire [`Cfg` record](https://monadosquito.github.io/bem/Bem-Cfg-Cfg.html)
using some defaults.
- The [utility `Gens` record](https://monadosquito.github.io/bem/Bem-Utl-Utl.html)
and the [utility `init` function](https://monadosquito.github.io/bem/Bem-Utl-Utl.html) can be used
in place of the regular ones
in order that the [utility configurable *class* generators](https://monadosquito.github.io/bem/Bem-Utl-Utl.html) are available
inside the [former](https://monadosquito.github.io/bem/Bem-Utl-Utl.html).
- The [type synonyms](https://monadosquito.github.io/bem/Bem-Utl-Intr.html) can be used
to avoid spelling of corresponding *entity* types combinations.
- The [*class* decorations](https://github.com/monadosquito/bem#table-1) can be configured
by applying the [`init` function](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html)
to an appropriate [`Cfg` record](https://monadosquito.github.io/bem/Bem-Cfg-Cfg.html)
and assigning the resulting [`Gens` record](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html)
of configured [*class* generators](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html) a name
in order that the [*class* generators](https://monadosquito.github.io/bem/Bem-Cls-Gen-Cfg.html) can be referenced
with it.

# [1.0.0] - 2023-07-01

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
