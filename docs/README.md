# Soda

*Soda* (Symbolic Objective Descriptive Analysis) is a functional language to describe ethical problems.
Its main purpose is to be used to produce clear code.
The source code should be easy to understand, somehow natural, although not necessarily easy to write.

This project includes a translator to Scala.


## Reserved words

The reserved words are:
- `=` (definition symbol)
- `:` (type symbol)
- `->` (lambda symbol)
- `=>` (implication symbol)
- `:=` (parameter definition symbol)
- `lambda`
- `if`
- `then`
- `else`
- `let`
- `in`
- `match`
- `case`
- `end`
- `class`
- `has`
- `extends`
- `with`
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


## Synonyms

The synonyms at the beginning of a line are:
- `*` for `class`
- `+` for `import`
- `|` for `case`

The synonyms in any part of a line are:
- `is` for `=`

Technical synonyms:
- `<:` for `subtype`
- `>:` for `supertype`


## Other Symbols

In addition, the language has:
- `(` and `)` (parentheses) for parameters, operator precedence, and arrays
- `{` and `}` (braces) for classes
- `[` and `]` (square brackets) for types

The main arithmetic operators are:
- `+`
- `-`
- `*`
- `/`

Comments are marked with `/*` and `*/`.
Scaladoc / Javadoc markers are `/**` and `*/`.

Annotations:
- `@new` to create JVM instances in translations to Scala 2
- `@tailrec` to ensure a tail recursion
- `@override` to override a JVM function
- `@main` to indicate the entry point

Special names:
- `_tailrec_` as prefix indicates that the function must be tail recursive
- `_rec_` as prefix indicates that the function is recursive
- `Main ()` is the entry point class


## Side Effects

In this language, variables cannot change their value.
Therefore, there is no equivalent to `var`.

For example, it is not possible to write `x = x + 1`.
Loops can be managed with tail recursion, or directly using streams.

The language does not provide `throw`, `try`, and `catch`, because those commands do not follow the functional style.


## Static Typing

This language is statically typed.
It is possible to define abstract and concrete classes.

An *abstract class* is like a *trait* in Scala.
A *concrete class* is like a *case class* in Scala.


## Package Declaration and Imports

This language is designed to be integrated via the Java Virtual Machine.
It is possible to define the package and to declare the imports.


## Syntax Highlighting

The following tools can be configured to have syntax highlighting:
- IntelliJ (instructions are provided)
- gedit (configuration file: soda.lang)


## Build

The project can be build with [sbt](https://www.scala-sbt.org/).

The command is:
`sbt "++ 3.0.2" clean compile test package assembly`

It is also possible to compile it for different Scala versions:
`sbt "++ 2.13.6" clean compile test package assembly`

If (scala_version) and (soda_version) is the Scala and Soda version respectively, the created jar file are:
- **translator**: library with classes that implement the Soda-Scala translator
  `translator/target/(scala_version)/translator_(scala_version)-(soda_version).jar`
- **library**: library with classes to be used in Soda programs
  `library/target/(scala_version)/library_(scala_version)-(soda_version).jar`
- **soda**: executable fat jar including the previous libraries and the Scala library.
  `target/(scala_version)/soda-(soda_version).jar`


