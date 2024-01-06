# [Soda](https://julianmendez.github.io/soda/)

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)][license]
[![build](https://github.com/julianmendez/soda/workflows/Scala%20CI/badge.svg)][build-status]
[![Documentation Status](https://readthedocs.org/projects/soda-lang/badge/?version=latest)][doc-status]

*Soda* (Symbolic Objective Descriptive Analysis) is an object-oriented functional language to
describe, analyze, and model human-centered problems.
Due to its readability, it can be applied to model ethical problems.

This project includes a translator to [Scala][scala].


## Reserved words

The reserved words are:

- `:` (type membership symbol)
- `->` (type mapping symbol)
- `=` (definition symbol)
- `:=` (parameter definition symbol)
- `lambda`, `any` (synonym of `lambda`), `-->` (lambda symbol)
- `if`, `then`, `else`
- `match`, `case`, `==>` (implication symbol in pattern matching)
- `class`, `extends`, `abstract`, `end`
- `this`, `subtype`, `supertype`
- `false`, `true` , `not`, `and`, `or`
- `package`, `import`
- `directive`


## Other Symbols

In addition, the language has:

- `(` and `)` (parentheses) for parameters and operator precedence
- `[` and `]` (square brackets) for parametric types

The main arithmetic operators are:

- `+`, `-`, `*`, `/`, `%` (modulus)

Comments are marked with `/*` and `*/`. Scaladoc / Javadoc markers are `/**` and `*/`.

Annotations:

- `@new` to create JVM instances in translations to Scala 2
- `@tailrec` to ensure a tail recursion
- `@override` to override a JVM function

Special names:

- `_tailrec_` as prefix indicates that the function is tail recursive
- `_rec_` as prefix indicates that the function is recursive
- `Main` is the entry point class, and `Main_ ()` its constructor


## Side Effects

In Soda, variables cannot change their value. Thus, it is not possible to write `x = x + 1`.
Loops can be managed with `range` and `fold` functions and tail recursion.

The language does not provide `throw`, `try`, and `catch`, because those commands do not follow
the functional style.


## Static Typing

This language is statically typed. It is possible to define abstract and concrete classes.
Each *class* (like a *trait* in Scala) can be extended and has a single class constructor.
This constructor is implemented with a *concrete class* (like a *case class* in Scala).


## Package Declaration and Imports

Soda is designed to be integrated via the Java Virtual Machine.
It is possible to define packages and to declare imports.
This can be done in a separate file `Package.soda`, which is in the same directory as the source
code.


## Syntax Highlighting

The following tools can be configured to have syntax highlighting:

- IntelliJ (instructions are provided)
- gedit (configuration file: soda.lang)


## Documentation

- [User manual][manual]


## Build

The project can be built with [sbt][sbt] with
`sbt clean compile test package assembly`

A Linux binary can be created with the script `makeall.sh`.


## Author

[Julian Alfredo Mendez][author]

[author]: https://julianmendez.github.io
[license]: https://www.apache.org/licenses/LICENSE-2.0.txt
[build-status]: https://github.com/julianmendez/soda/actions
[doc-status]: https://soda-lang.readthedocs.io/en/latest/?badge=latest
[manual]: https://soda-lang.readthedocs.io/en/latest/
[scala]: https://scala-lang.org
[sbt]: https://www.scala-sbt.org


