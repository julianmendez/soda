# Scopus

*Scopus* is a functional language to describe ethical problems.
Its main purpose is to be used to produce clear code.
The source code should be easy to understand, somehow natural, although not necessarily easy to write.

This project includes a translator to Scala.


## Reserved words

The reserved words are:
- `=` (definition symbol)
- `:` (type symbol)
- `->` (lambda symbol)
- `if`
- `then`
- `else`
- `class`
- `has`
- `extends`
- `with`
- `this`
- `false`
- `true`
- `not`
- `and`
- `or`
- `package`
- `import`
- `new`


## Synonyms

The synonyms at the beginning of a line are:
- `*` for `class`
- `-` for `has`
- `+` for `import`

The synonyms in any part of a line are:
- `is` for `=`
- `in` for `:`
- `to` for `->`
- `suchthat` for `->`


## Other Symbols

In addition, the language has:
- `(` and `)` (parentheses) for parameters, operator precedence, and arrays
- `{` and `}` (braces) for multi-line functions and classes
- `[` and `]` (square brackets) for types

The main arithmetic operators are:
- `+`
- `-`
- `*`
- `/`

Comments are marked with `/*` and `*/`.
Scaladoc / Javadoc markers are `/**` and `*/`.

Annotations:
- `@tailrec` to indicate tail recursion
- `@override` to indicate an overriding of a JVM function
- `@main` to indicate the entry point


## Side Effects

In this language, variables cannot change their value.
Therefore, there is no equivalent to 'var'.

For example, it is not possible to write `x = x + 1`.
Loops can be managed with tail recursion, or directly using streams.

The language does not provide 'throw', 'try', and 'catch', because those commands does not follow the functional style.


## Static Typing

This language is statically typed.
It is possible to define concrete and abstract classes.

A *concrete class* is like a *case class* in Scala.
An *abstract class* is like a *trait* in Scala.


## Package Declaration and Imports

This language is designed to be integrated via the Java Virtual Machine.
It is possible to define the package and to declare the imports.


## Syntax Highlighting

The following tools can be configured to have syntax highlighting:
- IntelliJ (instructions are provided)
- gedit (configuration file: scopus.lang)


