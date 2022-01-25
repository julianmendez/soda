package soda.translator.documentation

/*
 * This is a Soda tutorial written in Soda.
 * Copyright 2021 Julian Mendez
 * Version: 2021-11-19
 */

/*
 * Source code written in Soda is intended to be descriptive.
 * It is usually written in different files, and each file has `blocks`.
 * These blocks are pieces of code that have some meaning.
 * A block should be short, maybe less than 10 lines.
 * However, it is more important to make things clear than concise.
 * These are examples of blocks:
 * 1. a constant or function definition
 * 2. the beginning of a class definition
 * 3. the end of a class definition
 * 4. a class declaration
 * 5. an abstract constant or function declaration
 * 6. a list of imports
 * 7. a package declaration
 * 8. a comment
 */

/* To declare a class, just add `class` before a class name.
 * It is recommended to use camel case style starting with a capital letter.
 * The name could be a noun or an adjective, but it should not be a verb. */
trait Shape

/* There is an abbreviation for class declaration.
 * For this, just start a line with an asterisk (`*`). */
trait Movable

/* A class can be parameterized.
 * The parameter type can be constrained using `subtype` and `supertype`.
 * For example, `A subtype B` means that `A` is a subtype of `B`. */
trait ShapePainter [A <: Shape]

/* It is possible to constrain a class parameter with more than one type.
 * Connect them using `with`. */
trait ShapeMover [A <: Shape with Movable]

/* The body of a class is declared between braces (`{` and `}`) after the equals sign (`=`).
 * It is recommended to indent the constants and functions declared inside. */
trait EqualsExample {

  /* A constant does not have parameters and it is declared with the equals sign (`=`).
   * It is recommended to use snake case and start in lowercase.
   * The constant name should be a noun.
   * In a function call the parameters can be specified with the colon-equals sign (`:=`).
   * This is especially recommended when the parameters are of the same type.
   * Constants are only evaluated once, which is the first time they are needed. */
  lazy val answer: Int = f (x = 20, y = 2 )

  /* A function has parameters.
   * If the parameters are empty, it is implied that the function produces some side effect.
   * Functions, even with empty parameters, are evaluated every time they are invoked. */
  def f (x: Int, y: Int ): Int = 2 * x + y

}

/* A class can extend another one by using `extends`.
 * Abstract classes cannot be instantiated but can be extended.
 * Conversely, concrete classes cannot be extended but can be instantiated.
 * Concrete classes are declared with parentheses `(` and `)`.
 * It is recommended that concrete classes do not have a body, because this cannot be reused.
 * Concrete classes extending only one class could be named as its superclass, but ending with an underscore (`_`). */
case class EqualsExample_ ()
  extends EqualsExample

/* A class does not need to define all its constants and functions. */
trait RegisteredPerson {

  /* A block starting with `abstract` denotes a constant or function that needs to be defined in extending classes.
   Only one `abstract` block should be defined per class, without leaving lines between the declared attributes. */
  abstract
    first_name: String
    last_name: String

  /* If a constant or function is not meant to be exported, its name should start with an underscore (`_`). */
  lazy val _separator = " "

  /* Strings can be concatenated by using the plus sign (`+`). */
  lazy val full_name = first_name + _separator + last_name

}

/* A concrete class can be declared with parameters. */
case class Person (name: String )

trait Agent {

  def   identifier: String

}

/* A concrete class needs as parameters all the constants and functions that have not been defined in its super classes.
 * Please note that an abstract class might have constants and functions that are not defined in its ancestor classes. */
case class Agent_ (identifier: String )
  extends Agent

trait RankedIndividual {

  def   rank: Int

}

case class RankedAgentPerson (identifier: String, rank: Int )
  extends Agent with RankedIndividual

trait Element {

  def   accept (v: Visitor ): Boolean

}

trait Visitor {

  def   visit (x: Element ): Boolean

}

case class Item (identifier: Int )
  extends Element {

  /* It is possible to refer to an object instance by using `this`. */
  def accept (v: Visitor ) = v.visit (this )

}

trait PersonName {

  def   name: String

  /* It is possible to override a function by using the `@override` annotation.
   * This is intended only for exceptional cases, like the `toString` function. */
  override
  lazy val toString = name

}

/**
  * This contains the examples shown in the manual.
  */
trait Manual {

  import   java.util.Date

  lazy val a = 1

  lazy val b: Int = 2

  /* An instance of a JVM class can be created with the `@new` annotation.
   * If the code is translated to Scala 3, this annotation is not required. */
  lazy val now = new Date ()

  def plus_one (x: Int ): Int = x + 1

  /* A piecewise function can be defined using an `if`-`then`-`else` structure.
   * The condition in the `if` is evaluated, and then only the corresponding branch is evaluated. */
  def max (x: Int, y: Int ): Int =
    if (x > y
    ) x
    else y

  /* Scala sequences (`Seq`) can be used, as well as other basic Scala classes.
   * Lambda functions are declared using a right arrow (`->`). */
  def plus_one (sequence: Seq [Int]  ): Seq [Int] =
    sequence.map (element => element + 1 )

  /* Boolean values `false` and `true` are available. */
  def my_not (x: Boolean ): Boolean =
    if (x
    ) false
    else true

  def my_and (x: Boolean, y: Boolean ): Boolean =
    if (x
    ) y
    else false

  def my_or (x: Boolean, y: Boolean ): Boolean =
    if (x
    ) true
    else y

  /* Boolean values have the standard `not`-`and`-`or` functions. */
  def my_xor (x: Boolean, y: Boolean ): Boolean =
    (x || y ) && ! (x && y )

  /* It is possible to use pattern matching with `match` and `case`.
   * Please observe the double arrow `=>`. */
  def if_then_else [A] (condition: Boolean, if_true: A, if_false: A ): A =
    condition match  {
      case true => if_true
      case false => if_false
    }

  /* A vertical bar `|` can be used as an abbreviation for `case`. */
  def another_if_then_else [A] (condition: Boolean, if_true: A, if_false: A ): A =
    condition match  {
      case true => if_true
      case false => if_false
    }

  /* To evaluate a constant or a function, it is possible to declare intermediate functions.
   * This is done in a `let`-`in` block.
   * The block starts with a `let` containing the intermediate functions in no particular order.
   * This block is evaluated in an expression after the `in`. */
  def sum (n: Int ) =
    _tailrec_ (n, 0 )

  /* A tail recursive function cannot be declared inside another function, and its name could start with underscore '_'.
   * Annotation `@tailrec` helps ensuring that the tail recursion is detected and optimized. */
  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_ (n: Int, accum: Int ): Int =
    if (n < 0
    ) accum
    else _tailrec_ (n - 1, n + accum )

  def f0 (x: Int ): Int =
    {
      lazy val a = g (x )
      lazy val b = g (a )
      a + b }

  /* Since the order in a `let`-`in` block is not important, a constant `result` at the beginning could easily indicate
   * what is the main result to be evaluated. */
  def f1 (x: Int ): Int =
    {
      lazy val result = a + b
      lazy val a = g (x )
      lazy val b = g (a )
      result }

  def g (x: Int ): Int = x + 1

}

trait AbstractFactorialConcise {

  def   factorial (n: Int ): Int

}

trait FactorialConcise
  extends AbstractFactorialConcise {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_ (n: Int, product: Int ): Int =
    /* The function to compare equality is a long equals (`==`). */
    if (n == 0
    ) product
    else _tailrec_ (n - 1, n * product )

  def factorial (n: Int ): Int =
    _tailrec_ (n, 1 )

}

/* The word `is` is a synonym for the equals sign (`=`) and they are interchangeable. */
trait AbstractFactorialVerbose {

  def   factorial (n: Int ): Int

}

trait FactorialVerbose
  extends AbstractFactorialVerbose {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_ (n: Int, product: Int ): Int =
    if (n == 0
    ) product
    else _tailrec_ (n - 1, n * product )

  def factorial (n: Int ): Int =
    _tailrec_ (n, 1 )

}

trait Recursion {

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_fold4 [A, B, C <: B] (sequence: Seq [A], current_value: C, next_value_function: (B, A ) => C, condition: (B, A ) => Boolean ): C =
    if (sequence.isEmpty
    ) current_value
    else
      if (! condition (current_value, sequence.head )
      ) current_value
      else _tailrec_fold4 (sequence.tail, next_value_function (current_value, sequence.head ), next_value_function, condition )

  def fold [A, B, C <: B] (sequence: Seq [A], initial_value: C, next_value_function: (B, A ) => C, condition: (B, A ) => Boolean ): C =
    _tailrec_fold4 (sequence, initial_value, next_value_function, condition )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_range (n: Int, sequence: Seq [Int]  ): Seq [Int] =
    if (n <= 0
    ) sequence
    else _tailrec_range (n - 1, sequence.+: (n - 1 )  )

  def range (length: Int ): Seq [Int] =
    _tailrec_range (length, Seq [Int] ()  )

}

case class Recursion_ ()
  extends Recursion

/* The main class is concrete and it is called `Main ()`. */
case class Main ()
  extends MainClass

/* The main class requires a `main` function that receives an `Array [String]` and returns a `Unit`. */
trait MainClass {

  def main (arguments: Array [String]  ): Unit =
    /* An output to the standard output can be sent with a `println` command.
     * This is a shorter form of JVM's `System.out.println`. */
    println ("Hello world!")

}

/* The main class needs to be indicated with the `@main` annotation.
 * Only one main class per package is allowed. */
object EntryPoint {
  def main (args: Array [String]): Unit = Main ().main (args)
}

