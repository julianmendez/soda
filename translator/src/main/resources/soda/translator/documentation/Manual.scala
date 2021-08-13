package soda.translator.documentation

/*
 * This is a Soda tutorial written in Soda.
 * Copyright 2021 Julian Mendez
 * Version: 2021-08-12
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

  /* A constant does not have parameters, and it is declared with the equals sign (`=`).
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
 * Concrete classes extending only one class should be named as its superclass, but ending with an underscore (`_`). */
case class EqualsExample_ () extends EqualsExample

/* A class does not need to declare the all its constants and functions. */
trait RegisteredPerson {

  /* A line starting with `has` denotes a constant or function that needs to be declared in extending classes. */
  def first_name: String

  def last_name: String

  /* If a constant or function is not meant to be exported, its name should start with an underscore (`_`). */
  lazy val _separator = " "

  /* Strings can be concatenated by using the plus sign (`+`). */
  lazy val full_name = first_name + _separator + last_name
}

/* A concrete class can be declared with parameters.
 * It is not recommended to use this pattern when using this class as receiving parameters in functions. */
case class Person (name: String )

trait Agent {

  def identifier: String
}

/* A concrete class needs as parameters all the constants and functions that have not be defined in its super class.
 * Please note that an abstract class might have constants and functions that are not defined in its ancestor classes. */
case class Agent_ (identifier: String ) extends Agent

trait RankedIndividual {

  def rank: Int
}

case class RankedAgentPerson (identifier: String, rank: Int )  extends Agent with RankedIndividual

trait Element {

  def accept (v: Visitor ): Boolean
}

trait Visitor {

  def visit (x: Element ): Boolean
}

case class Item (identifier: Int ) extends Element {

  /* It is possible to refer to an object instance by using `this`. */
  def accept (v: Visitor ) = v.visit (this )
}

trait PersonName {

  def name: String

  /* It is possible to override a function by using annotation `@override`.
   * This is intended only for exceptional cases, like the `toString` function. */
  override
  lazy val toString = name
}

/**
  * This contains the examples shown in the manual.
  */
trait Manual {
  import java.util.Date

  lazy val a = 1

  lazy val b: Int = 2

  /* An instance of a JVM class can be created with annotation `@new`.
   * If the code is translated to Scala 3, this annotation is not required. */
  lazy val now = new Date ()

  def plus_one (x: Int ): Int = x + 1

  /* A function can be defined using a `if`-`then`-`else` structure.
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
    )
      if (y
      ) true
      else false
    else false

  def my_or (x: Boolean, y: Boolean ): Boolean =
    if (x
    ) true
    else
      if (y
      ) true
      else false

  /* Boolean values have the standard `not`-`and`-`or` functions. */
  def my_xor (x: Boolean, y: Boolean ) =
    (x || y ) && ! (x && y )

  /* To evaluate a constant or a function, it is possible to declare intermediate functions.
   * This is done in a block `let`-`in`.
   * The block starts with a `let` containing the intermediate functions in no particular order.
   * This block is evaluated in an expression after the `in`. */
  def sum (n: Int ) =
    {
      lazy val result = rec (n, 0 )

      /* A tail recursive function should be declared inside another function.
       * Annotation `@tailrec` helps ensuring that the tail recursion is detected and optimized. */
      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, accum: Int ): Int =
        if (n < 0
        ) accum
        else rec (n - 1, n + accum )

      result }

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

  def factorial (n: Int ): Int
}

trait FactorialConcise  extends AbstractFactorialConcise {

  def factorial (n: Int ) =
    {
      lazy val result = rec (n, 1 )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, product: Int ): Int =
        /* The function to compare equality is a long equals (`==`). */
        if (n == 0
        ) product
        else rec (n - 1, n * product )

      result }
}

/* The word `is` is a synonym for the equals sign (`=`) and they are interchangeable. */
trait AbstractFactorialVerbose {

  def factorial (n: Int ): Int
}

trait FactorialVerbose  extends AbstractFactorialVerbose {

  def factorial (n: Int ) =
    {
      lazy val result = rec (n, 1 )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, product: Int ): Int =
        if (n == 0
        ) product
        else rec (n - 1, n * product )

      result }
}

trait Recursion {

  def fold [A, B, C <: B]  (sequence: Seq [A], initial_value: C, next_value_function: (B, A ) => C, condition: (B, A ) => Boolean  ): C =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (seq: Seq [A], acc: C ): C =
        if (seq.isEmpty
        ) acc
        else
          if (! condition (acc, seq.head )
          ) acc
          else rec (seq.tail, next_value_function (acc, seq.head )  )

      rec (sequence, initial_value ) }

  def range (length: Int ): Seq [Int] =
    {
      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, seq: Seq [Int]  ): Seq [Int] =
        if (n <= 0
        ) seq
        else rec (n - 1, seq.+: (n - 1 )  )

      rec (length, Seq [Int]  ()  ) }
}

case class Recursion_ () extends Recursion


/* The main class is concrete and it is called `Main()`. */
case class Main () extends MainClass

/* The main class requires a `main` function that receives an `Array[String]` and returns a `Unit`. */
trait MainClass {

  def main (arguments: Array [String]  ): Unit =
    /* An output to the standard output can be send with a `println` command.
     * This is a shorter form of JVM's `System.out.println`. */
    println ("Hello world!")
}

/* The main class needs to be indicated with annotation `@main`.
 * Only one main class per package is allowed. */
object EntryPoint {
  def main(args: Array[String]): Unit = Main().main(args)
}

