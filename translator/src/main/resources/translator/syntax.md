


# Syntax of Scopus

*Scopus* is a functional language characterized by its conciseness.
Its main purpose is to be used to produce clean, elegant, and descriptive code.
The source code should be easy to understand, somehow natural, although not necessarily easy to write.

For simplicity, the language is implemented in Scala.


## Keywords

The number of keywords in this language is small.


## Main Operators in Scopus

The main operators are:
- '=' (definition)
- '->' (lambda)

The main commands:
- if
- then
- else
- class
- trait
- extends
- package
- import
- new

In addition, the language has:
- '(' and ')' (parentheses)
- '{' and '}' (braces)

The language has also auxiliary operators, that work as functions of specific types.
These are inherited from Scala.

Some of the most important operators are:
- '+', '-', '*', '/' (arithmetic operators)
- and, or (logical operator)
- true, false (logical variables)
- '(' ')' (get from array)
- '[' ']' are used to define a parameterized type


## Side Effects

In this language, variables cannot change their value.
Therefore, there is no equivalent to 'var'.

For example, it is not possible to write `x = x + 1`.
Loops can be managed with tail recursion, or directly using streams.

The language does not provide 'throw', 'try', and 'catch', because those commands does not follow the functional style.


## Static Typing

This language is statically typed.
It is possible to define classes and traits.


## Package Declaration and Imports

This language is designed to be integrated via the Java Virtual Machine.
It is possible to define the package and to declare the imports.


