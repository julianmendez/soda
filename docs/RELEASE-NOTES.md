## Release Notes
---
- - version: v0.20.0
  - date: '2024-02-01'
  - build: $ `sbt '++ 3.3.1' clean compile test package assembly`
  - release: target/scala-3.3.1/soda-0.20.0.jar
  - features:
    - includes translation of private functions to Lean
    - includes a default function `mk` to construct a type, which is invoked with the type name
      in a "static" fashion, e.g. `Pair_ (fst) (snd)` can be created with
      `Pair .mk (fst) (snd)`, and if there are no parameters, the parentheses are not needed
    - includes more advanced examples for the integration of theorems proved in Lean
    - accepts `fun` as a synonym of `lambda`
  - deprecated:
    - the `theorem` reserved word is no longer used, and its functionality is replaced
      by `directive`
- - version: v0.19.0
  - date: '2023-07-01'
  - build: $ `sbt '++ 3.3.0' clean compile test package assembly`
  - release: target/scala-3.3.0/soda-0.19.0.jar
  - features:
    - includes creation of instances with individual parameters, e.g. `Pair_ (fst)
      (snd)` instead of `Pair_ (fst , snd)`
    - includes an experimental translator to Lean 4, which can translate some snippets
    - uses `Type` to define type membership of parametric types
    - accepts that class `end` could have a class name
    - accepts `def` as an optional reserved word to define functions
    - includes the reserved word `directive` to include specific pieces of code depending
      on the translator, especially for the translators to Lean and to Coq
  - deprecated:
    - the `proof` reserved word is no longer used, and a `theorem` block should include
      the proof after stating the theorem
- - version: v0.18.0
  - date: '2023-06-01'
  - build: $ `sbt '++ 3.3.0' clean compile test package assembly`
  - release: target/scala-3.3.0/soda-0.18.0.jar
  - features:
    - assumes translation to `Package.scala` files as default configuration for the
      Scala translator
    - expects multiple type parameters to be between different square brackets, e.g.
      `MyPair [A] [B]` instead of `MyPair [A, B]`
    - accepts intersection types (built with `&`) and union types (built with `|`)
  - fixes:
    - fixes translation of reserved words after opening parenthesis
    - fixes edge case of directory scanner when translating multiple Soda files to
      Scala
    - fixes processing of Soda files when Package.soda file is missing
- - version: v0.17.0
  - date: '2023-03-01'
  - build: $ `sbt '++ 3.2.2' clean compile test package assembly`
  - release: target/scala-3.2.2/soda-0.17.0.jar
  - features:
    - includes an option to translate a whole Soda package into a single Scala file
    - reads `Package.soda` as prelude for a Soda file in the same directory
    - requires `match`-`case` structures to be used at most once in a function definition,
      and cannot be nested in another structure
    - improves function definition by allowing multiple lines in its signature
    - accepts reserved word `any` as a synonym for `lambda`
    - accepts Unicode characters for reserved words, like the letter lambda for `lambda`
      and right arrows for `->`, `-->`, and `==>`
    - includes a Bash script (`makeall.sh`) to build the project and create the binary
      file
  - deprecated:
    - the `end` reserved word for `match`-`case` is no longer used
- - version: v0.16.0
  - date: '2022-08-02'
  - features:
    - includes an extension to generate LaTeX files from source code
  - build: $ `sbt '++ 3.1.3' clean compile test package assembly`
  - release: target/scala-3.1.3/soda-0.16.0.jar
- - version: v0.15.0
  - date: '2022-03-10'
  - features:
    - accepts a long arrow `-->` for lambda expressions
    - accepts a long double arrow `==>` for pattern matching cases
    - can fully translate small snippets to Coq
    - translates constants and functions starting with `_` as private in Scala
  - deprecated:
    - the `let`-`in` structures are no longer supported
    - the following synonyms are no longer supported
    - (asterisk) `*` (synonym for `class`)
    - (minus) `-` (synonym for `has`)
    - (plus) `+` (synonym for `import`)
    - (vertical bar) `|` (synonym for `case`)
    - . `is` (synonym for `=`)
    - . `fun` (synonym for `lambda`)
  - build: $ `sbt '++ 3.1.1' clean compile test package assembly`
  - release: target/scala-3.1.1/soda-0.15.0.jar
- - version: v0.14.0
  - date: '2022-02-11'
  - features:
    - uses curried functions instead of the uncurried ones, for example, replacing
      `f (x, y)` by `f (x) (y)`
    - generates default constructors for all the abstract classes and these constructors
      are named like the class with an underscore (`_`) at the end
    - renames the main class to be `Main` and `Main_` for the abstract and concrete
      classes respectively
    - uses `Main` class to detect the entry point
    - does not support the `with` reserved word
    - changes the syntax of class declarations such that each super class needs its
      own line and the definition sign (`=`) is not used
    - includes reserved word `abstract` to define a block of abstract constants and
      functions
    - makes reserved word `import` define a block of classes to import
    - produces a more similar translation to Scala by removing the line joiners
    - restricts how a constant or function is defined, such that the definiendum cannot
      be written in multiple lines
    - does not need opening brace (`{`) to start a class definition
    - uses word `end` to end a class definition
    - has `lambda` reserved word as 'recommended' instead of 'optional'
    - accepts reserved word `fun` as synonym for `lambda`
    - accepts reserved word `def` as optional to define constants and functions
  - deprecated:
    - reserved word `has` is replaced by `abstract` block
    - abbreviation `+` is replaced by its long form `import`
    - classes are no longer defined with braces (`{` and `}`)
    - annotation `@main` is no longer user
  - build: $ `sbt '++ 3.1.1' clean compile test package assembly`
  - release: target/scala-3.1.1/soda-0.14.0.jar
- - version: v0.13.0
  - date: '2022-01-14'
  - build: $ `sbt '++ 3.1.0' clean compile test package assembly`
  - features:
    - compiles with Scala 3.1.0
    - is compatible with Scala 2.13.8
    - changes syntax of pattern matching to use `match`, `case`, `=>`, `end`
    - includes reserved words `theorem` and `proof` to write properties in Gallina
      (Coq)
    - makes annotation `@tailrec` only available to class methods
    - adopts special prefixes `_tailrec_` for tail recursive functions and `_rec_`
      for recursive functions
    - uses a `let`-`in` structure in functions of unit tests
    - discourages use of `let`-`in` structures and does not support nested use, to
      avoid long functions
  - deprecated:
    - use of a `let`-`in` structure is only supported as main structure inside a test
      or function definition
  - release: target/scala-3.1.0/soda-0.13.0.jar
- - version: v0.12.0
  - date: '2021-10-08'
  - features:
    - makes a stronger distinction between interfaces and other abstract classes
    - requires that the reserved word `match` needs to be at the beginning of the
      line
    - includes examples written in Coq
  - build: $ `sbt '++ 3.0.2' clean compile test package assembly`
  - release: target/scala-3.0.2/soda-0.12.0.jar
- - version: v0.11.0
  - date: '2021-09-12'
  - features:
    - compiles with Scala 3.0.2
    - has an improved documentation
    - includes `match` and `case` for pattern matching, and synonym `|` at the beginning
      of the line for `case`
    - includes an optional `lambda` reserved word to make lambda expressions more
      explicit
    - includes type aliases for code translated to Scala 3
    - allows defining the body of a class without braces when the code is translated
      to Scala 3
    - adopts a naming standard for concrete classes to end in underscore
    - includes new synonyms, `<:` for `subtype` and `>:` for `supertype`
    - supports (again) the use of tuples on the left-hand side of a definition sign
      (`=`)
    - renames (again) concrete classes in `OptionSD`, mapping `NoneSD` to `NoneSD_`
      and `SomeSD` to `SomeSD_`
  - build: $ `sbt '++ 3.0.2' clean compile test package assembly`
  - release: target/scala-3.0.2/soda-0.11.0.jar
- - version: v0.10.0
  - date: '2021-08-08'
  - features:
    - has its functions separated from concrete classes making abstract classes the
      only classes containing functions
    - uses a uniform standard for constant names, function names, and class names
    - renames `foldLeft` and `foldLeftWhile` to just `fold` in `Recursive` library
      class
    - renames concrete classes in `OptionSD`, mapping `NoneSD` to `NoElem` and `SomeSD`
      to `SomeElem`
    - includes a module for examples
  - build: $ `sbt '++ 3.0.0' clean compile test package assembly`
  - release: target/scala-3.0.0/soda-0.10.0.jar
- - version: v0.9.0
  - date: '2021-07-23'
  - features:
    - includes `let` and `in` reserved words to define a block of bindings
    - makes opening brackets (`[`) in the same line as the following
    - makes closing parenthesis (`)`) and closing brackets (`]`) be in the same line
      as the previous one
    - makes reserved words `extends` and `with` be able to join lines, either written
      at the beginning to join the previous line, or at the end to join the following
      line
    - is compatible with Scala 2.12.14
  - deprecated:
    - synonym `suchthat` for `->` is no longer supported
  - build: $ `sbt '++ 3.0.0' clean compile test package assembly`
  - release: target/scala-3.0.0/soda-0.9.0.jar
- - version: v0.8.0
  - date: '2021-05-23'
  - features:
    - compiles with Scala 3.0.0 and is compatible with Scala 2.13.6, Scala 2.12.13,
      and Scala 2.11.12
    - lets Scala 3 reserved words be usable as variable, function, and class names
    - makes `=` (definition symbol) used to define values, functions, and classes
    - replaces `new` command by `@new` annotation, which is only required to translations
      to Scala 2
    - expands Soda library in multiple files
    - accepts `(` (opening parenthesis) at the end of a line to join lines, like `,`
      (comma) does
    - replaces `extends` to declare upper bounds of type parameters by `subtype`
    - includes `supertype` to declare lower bounds of type parameters
    - replaces all `package.scala` files by `Package` abstract classes to document
      the package
  - build: $ `sbt '++ 3.0.0' clean compile test package assembly`
  - release: target/scala-3.0.0/soda-0.8.0.jar
- - version: v0.7.0
  - date: '2021-04-24'
  - features:
    - compiles with Scala 3.0.0-RC2
    - makes `:=` (parameter definition symbol) as the symbol to define values for
      named parameters
    - includes `extends` to declare upper bounds of type parameters, with the meaning
      'subtype of'
    - includes a file expansion of a basic library when it finds `lib.soda`
    - produces better looking Scala translated source code
  - deprecated:
    - does not support any longer the use of tuples on the left-hand side of a definition
      sign (`=`)
  - fixes:
    - fixes translation of constants that are lambda functions
  - build: $ `sbt '++ 3.0.0-RC2' clean compile test package assembly`
  - release: target/scala-3.0.0-RC2/soda-0.7.0.jar
- - version: v0.6.0
  - date: '2021-04-03'
  - features:
    - is renamed as 'Soda' (Symbolic Objective Descriptive Analysis)
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-3.0.0-RC1/soda-0.6.0.jar
- - version: v0.5.0
  - date: '2021-03-28'
  - features:
    - translates functions without parameters as `lazy val`
    - changes its main package to `scopus`, and includes subpackages
    - accepts `,` (comma) at the end of a line to join lines and allow multi-line
      function signatures
    - makes annotation `@tailrec` be automatically imported
    - allows annotation `@tailrec` only inside functions
    - compiles with Scala 3.0.0-RC1 and is compatible with Scala 2.11.12, Scala 2.12.13,
      Scala 2.13.5
  - deprecated:
    - synonym `to` for `->` and synonym `in` for `:` are no longer supported
  - fixes:
    - fixes the names of the created library jar files
    - fixes parsing of strings with escaped characters
    - fixes translation of `not`
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-3.0.0-RC1/scopus-0.5.0.jar
- - version: v0.4.0
  - date: '2021-02-15'
  - features:
    - has some of its functions renamed to follow snake case
    - is compatible with Scala 2.12.13, Scala 2.13.4, and Scala 3.0.0-M3
    - translates recursively all files starting in a given directory
  - deprecated:
    - synonym `-` for `has` is no longer supported
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-3.0.0-M3/scopus-0.4.0.jar
- - version: v0.3.1
  - date: '2021-02-09'
  - features:
    - is stricter in preventing the use of non-Scopus reserved words
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-3.0.0-M3/scopus-0.3.1.jar
- - version: v0.3.0
  - date: '2021-02-02'
  - features:
    - has its source code written in Scopus
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-3.0.0-M3/scopus-0.3.0.jar
- - version: v0.2.1
  - date: '2021-02-01'
  - features:
    - is compiled by default for Scala 3.0.0-M3
    - includes a highlighting configuration file for [gedit](https://gitlab.gnome.org/GNOME/gedit/)
    - renames synonym `that` to `suchthat`
    - includes annotation `@main` to indicate the entry point
    - has an updated manual
    - uses [JaCoCo](https://www.eclemma.org/jacoco/) for test code coverage analysis
  - deprecated:
    - synonym `that` is no longer supported
    - synonym `equals` is no longer supported
  - fixes:
    - fixes translation of tuple definition
    - fixes translation of synonyms at the beginning of a line
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-3.0.0-M3/scopus-0.2.1.jar
- - version: v0.2.0
  - date: '2020-11-21'
  - features:
    - has a more detailed manual
    - includes synonyms, which are
    - (asterisk) `*` (synonym for `class`)
    - (minus) `-` (synonym for `has`)
    - (plus) `+` (synonym for `import`)
    - . `is` (synonym for `=`)
    - . `in` (synonym for `:`)
    - . `to` (synonym for `->`)
    - . `that` (synonym for `->`)
    - . `equals` (synonym for `==`)
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-2.13/scopus-0.2.0.jar
- - version: v0.1.1
  - date: '2020-11-16'
  - features:
    - has the asterisk (`*`) as synonym for class
    - assumes the output file when it is omitted
  - fixes:
    - fixes translation of definition sign (`=`)
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-2.13/scopus-0.1.1.jar
- - version: v0.1.0
  - date: '2020-11-05'
  - features:
    - is an operational version with the basic reserved words, which are
    - (definition symbol) `=`
    - (type symbol) `:`
    - (lambda symbol) `->`
    - . `if`
    - . `then`
    - . `else`
    - . `class`
    - . `has`
    - . `extends`
    - . `with`
    - . `this`
    - . `false`
    - . `true`
    - . `not`
    - . `and`
    - . `or`
    - . `package`
    - . `import`
    - . `new`
    - has `@tailrec` annotation to indicate tail recursion, and `@override` annotation
      to allow overriding JVM functions
  - build: $ `sbt clean compile test package assembly`
  - release: target/scala-2.13/scopus-0.1.0.jar
- - schema: RELEASE-NOTES.md.schema.yaml


