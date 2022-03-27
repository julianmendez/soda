# Soda

*Soda* (Symbolic Objective Descriptive Analysis) is a functional language to describe ethical problems. Its main purpose
is to be used to produce clear code. The source code should be easy to understand, somehow natural, although not
necessarily easy to write.

This project includes a translator to Scala.


## Reserved words

The reserved words are:

- `:` (type membership symbol)
- `->` (type mapping symbol)
- `=` (definition symbol)
- `:=` (parameter definition symbol)
- `lambda`
- `-->` (lambda symbol)
- `if`
- `then`
- `else`
- `match`
- `case`
- `==>` (implication symbol in pattern matching)
- `class`
- `extends`
- `abstract`
- `end`
- `this`
- `subtype`
- `supertype`
- `false`
- `true`
- `not`
- `and`
- `or`
- `package`
- `import`
- `theorem`
- `proof`


## Other Symbols

In addition, the language has:

- `(` and `)` (parentheses) for parameters and operator precedence
- `[` and `]` (square brackets) for types

The main arithmetic operators are:

- `+`
- `-`
- `*`
- `/`
- `%`

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

In this language, variables cannot change their value. Therefore, there is no equivalent to `var`.

For example, it is not possible to write `x = x + 1`. Loops can be managed with `range` and `fold` functions, tail
recursion, or directly using streams.

The language does not provide `throw`, `try`, and `catch`, because those commands do not follow the functional style.


## Static Typing

This language is statically typed. It is possible to define abstract and concrete classes.

A *class* is like a *trait* in Scala. Each class should have a single class constructor. The class constructor is
implemented with a *concrete class*, which is like a *case class* in Scala.


## Package Declaration and Imports

This language is designed to be integrated via the Java Virtual Machine. It is possible to define the package and to
declare the imports.


## Syntax Highlighting

The following tools can be configured to have syntax highlighting:

- IntelliJ (instructions are provided)
- gedit (configuration file: soda.lang)


## Build

The project can be build with [sbt](https://www.scala-sbt.org/).

The command is:
`sbt clean compile test package assembly`

It is also possible to compile it for different Scala versions.


