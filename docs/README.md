# [Soda](https://julianmendez.github.io/soda/)

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)][license]
[![build](https://github.com/julianmendez/soda/workflows/Scala%20CI/badge.svg)][build-status]
[![Documentation Status](https://readthedocs.org/projects/soda-lang/badge/?version=latest)][doc-status]

*Soda* (Symbolic Objective Descriptive Analysis) is a functional language with
object-oriented notation to describe, analyze, and model human-centered problems, especially
ethical problems.

This project includes a translator (or transpiler) to [Scala 3][scala], experimental
translators to [Lean 4][lean] and [Coq 8][coq], and a [LaTeX][latex] documentation
extraction tool.


## Publications

* <a id="Me2023"></a>
  Julian Alfredo Mendez.
  **Soda: An Object-Oriented Functional Language for Specifying Human-Centered Problems**.
  arXiv, 2023.
  &nbsp; DOI:[10.48550/arXiv.2310.01961][soda-doi]
  &nbsp; [Abstract][soda-abstract]
  &nbsp; [BibTeX][soda-bibtex]
  &nbsp; [PDF][soda-pdf]
  &nbsp; [Implementation][soda-impl]

* <a id="MeKa2025"></a>
  Julian Alfredo Mendez, Timotheus Kampik.
  **Can Proof Assistants Verify Multi-Agent Systems?**.
  arXiv, 2025.
  &nbsp; DOI:[10.48550/arXiv.2503.06812][soda-mas-doi]
  &nbsp; [Abstract][soda-mas-abstract]
  &nbsp; [BibTeX][soda-mas-bibtex]
  &nbsp; [PDF][soda-mas-pdf]
  &nbsp; [Implementation][soda-mas-impl]


## Online manual

- [User manual][manual]


## Where to start

Steps to run a "Hello world!" example in Soda:

1. To run these steps, you need to install:
    - a. [Scala 3][scala]
    - b. [sbt][sbt] (if you need to build the binaries)
    - c. [Java][java] (to execute JAR files)
2. Get the translator binary by either doing the following:
    - a. download the Linux binary from [releases][soda-releases]
    - b. **or** clone the [GitHub repository][soda-repo] and compile it, by either:
        - i. run the `makeall.sh`, from a Linux compatible environment
        - ii. **or** run `sbt` to get an executable JAR file as indicated in the
          [release notes][release-notes]. The command itself is described in `build` and the
          file is `release`. To execute a JAR file, you need a [Java][java] environment
          installed, and you need to run `java -jar filename.jar`, for a JAR file named
          `filename.jar`.
3. Once you got the binary translator, go to an empty directory and try
   `soda manual`. It will output a piece of code with many examples, but most importantly,
   this mini-manual is a "Hello, World!" program itself. Write `soda manual > Manual.soda` and
   you get the manual.
4. Once `Manual.soda` has been created, run `soda .`. This will create two files:
   `Package.soda` and `Package.scala`.
5. Run `scalac Package.scala`, which will compile sources and put the result in the
   directory
   `soda/manual`.
6. Run `scala soda.manual.EntryPoint`, which will show you `Hello world!`.


## How to learn Soda

Soda is a functional language intended to be **easy to learn and to read**. However, writing
purely functional style requires some practice, as some things are different from the
imperative style. In addition, Soda includes an object-oriented notation to align it with
mainstream object-oriented programming languages, and to make its notation familiar to users
acquainted to those languages.

The first step is to **get familiar with the functional notation** and functional approach to
algorithms. This can be learned by rewriting imperative pieces of code, and use Scala, which
allows both paradigms, to check that both approaches give similar results.

The next step is to **get familiar with classes and packages**. Modeling simple examples can
give a good basis to see what possibilities and limitations has to model with Soda. You will
find that modeling will seem faster and easier, but you will find some limitations when
modeling constrained instances. It is also a good practice to get acquainted to the syntax
and naming conventions, which are useful when creating larger projects.

The next step is to **get familiar with the testing** possibilities. At the moment, this is
only
developed for the Scala translator. Some examples can be found in module
[examples][examples-test], in the Soda repository. They can be run with `sbt test`.

The last step is to **get familiar with the verification** possibilities. For this, it would
probably work better to write Lean 4 proofs, as the Soda translator to Lean is more developed
than the translator to Coq. A good way to start proving theorems in Lean is to
follow the tutorials at the [Lean Game Server][lean-game-server], like the **Natural Number
Game**.


## Technical details


### Reserved Words and Symbols

The **reserved words** are:

- `:` (type membership symbol)
- `->` (type mapping symbol)
- `=` (definition symbol), `def` (optional)
- `:=` (parameter definition symbol)
- `lambda`, `any` (synonym of `lambda`), `fun` (synonym of `lambda`), `-->` (lambda symbol)
- `if`, `then`, `else`
- `match`, `case`, `==>` (implication symbol in pattern matching)
- `datatype`, `inductive` (synonym of `datatype`), `data` (synonym of `datatype`)
- `class`, `extends`, `abstract`, `end`
- `this`, `subtype`, `supertype`
- `false`, `true` , `not`, `and`, `or`
- `package`, `import`
- `directive`

In addition, the language has:

- `(` and `)` (parentheses) for parameters and operator precedence
- `[` and `]` (square brackets) for parametric types

The main arithmetic operators are:

- `+`, `-`, `*`, `/`, `%` (modulus)

Comments are marked with `/*` and `*/`. Scaladoc / Javadoc markers are `/**` and `*/`.

The annotations (only available when translating to Scala) are:

- `@new` to create JVM instances in translations to Scala 2
- `@tailrec` to ensure a tail recursion
- `@override` to override a JVM function

The following are special names:

- `_tailrec_` as prefix indicates that the function is tail recursive
- `_rec_` as prefix indicates that the function is recursive
- `Main` is the entry point class, and `Main_ ()` its constructor


### Integration

This language is **statically typed**. It is possible to define abstract and concrete classes.
Each *class* (like a *trait* in Scala) can be extended and has a single class constructor.
This constructor is implemented with a *concrete class* (like a *case class* in Scala).

Soda is designed to be integrated via the Java Virtual Machine. It is possible to define
packages and to declare imports. This can be done in a separate
file `Package.soda`, which is in the same directory as the source
code.

In Soda, variables cannot change their value. Thus, it is not possible to write `x = x + 1`.
Loops can be managed with `range` and `fold` functions and tail recursion.
The language does not provide `throw`, `try`, and `catch`, because those commands do not follow
the functional style. Nevertheless, it is possible to produce side effects, like using files,
through packages provided by Scala.

The following tools can be configured to have **syntax highlighting**:

- [IntelliJ][intellij] [configuration][intellij-conf]
- [gedit][gedit] [configuration][gedit-conf]


## Build

The project can be built with [sbt][sbt] with
`sbt clean compile test package assembly`

A Linux binary can be created with the script `makeall.sh`.

More detailed information can be found in the [release notes][release-notes].


## Author

[Julian Alfredo Mendez][author]

[soda-doi]: https://doi.org/10.48550/arXiv.2310.01961

[soda-abstract]: https://arxiv.org/abs/2310.01961

[soda-bibtex]: https://julianmendez.github.io/soda/bibtex-2023.html

[soda-pdf]: https://arxiv.org/pdf/2310.01961

[soda-impl]: https://github.com/julianmendez/soda

[soda-mas-doi]: https://doi.org/10.48550/arXiv.2503.06812

[soda-mas-abstract]: https://arxiv.org/abs/2503.06812

[soda-mas-bibtex]: https://julianmendez.github.io/soda/mas-bibtex-2025.html

[soda-mas-pdf]: https://arxiv.org/pdf/2503.06812

[soda-mas-impl]: https://github.com/julianmendez/market

[author]: https://julianmendez.github.io

[license]: https://www.apache.org/licenses/LICENSE-2.0.txt

[build-status]: https://github.com/julianmendez/soda/actions

[doc-status]: https://soda-lang.readthedocs.io/en/latest/?badge=latest

[manual]: https://soda-lang.readthedocs.io/en/latest/

[release-notes]: https://julianmendez.github.io/soda/RELEASE-NOTES.html

[soda-repo]: https://github.com/julianmendez/soda

[soda-releases]: https://github.com/julianmendez/soda/releases

[examples-test]: https://github.com/julianmendez/soda/tree/master/examples/src/test/scala/soda/example

[lean-game-server]: https://adam.math.hhu.de

[sbt]: https://www.scala-sbt.org

[scala]: https://scala-lang.org

[java]: https://www.oracle.com/java/technologies/

[lean]: https://lean-lang.org

[coq]: https://coq.inria.fr

[latex]: https://www.latex-project.org

[sbt]: https://www.scala-sbt.org

[intellij]: https://www.jetbrains.com/idea/

[intellij-conf]: https://github.com/julianmendez/soda/blob/master/translator/src/main/resources/soda/translator/documentation/soda_for_intellij.txt

[gedit]: https://gedit-technology.github.io/apps/gedit/

[gedit-conf]: https://github.com/julianmendez/soda/blob/master/translator/src/main/resources/soda/translator/documentation/soda.lang


