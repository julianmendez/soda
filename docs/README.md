# Scopus

*Scopus* is a functional language characterized by its conciseness.
Its main purpose is to be used to produce clean, elegant, and descriptive code.
The source code should be easy to understand, somehow natural, although not necessarily easy to write.

This project includes a translator to Scala.


## Reserved words

The number of reserved words in this language is small.


## Main Operators in Scopus

The reserved words are:
- '=' (definition symbol)
- ':' (type symbol)
- '->' (lambda symbol)
- if
- then
- else
- class
- has
- extends
- with
- this
- false
- true
- not
- and
- or
- package
- import
- new

There are also abbreviations.
- '*' at the beginning of the line is an abbreviation for 'class'
- '→' (\u2192) is an abbreviation for '->' (lambda symbol)

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


