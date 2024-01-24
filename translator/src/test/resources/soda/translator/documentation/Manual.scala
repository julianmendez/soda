package soda.translator.documentation

/*
 * This is a Soda tutorial written in Soda.
 * Copyright 2020--2023 Julian Alfredo Mendez
 * Version: 2023-07-01
 */

/* This tutorial is itself a "Hello world!" program.
 * The example snippet can be found at the end.
 */

/* Source code written in Soda is intended to be descriptive and very readable.
 * It is usually written in different files, and each file has `blocks`.
 * These blocks are pieces of code that have some meaning.
 * A block should be short, maybe less than 10 lines.
 * However, it is more important to make things clear than concise.
 * These are examples of blocks:
 * 1. a constant or function definition
 * 2. the beginning of a class definition
 * 3. the end of a class definition
 * 4. a block declaration of abstract constants and functions
 * 5. a block of imports
 * 6. a package declaration
 * 7. a class alias
 * 8. a comment
 */

/* To declare a class, just add `class` before a class name, and end it with `end`.
 * It is recommended to use camel case style starting with a capital letter.
 * The name could be a noun or an adjective, but it should not be a verb. */

trait Shape
{

}

case class Shape_ () extends Shape

object Shape { def mk   : Shape  = Shape_  () }

/* The reserved word `class` declares a type, a namespace, and a default constructor.
 * The default constructor is the name of the class with an underscore as suffix.
 * For example, the constructor name for `Shape` is `Shape_`. */

trait Movable
{

}

case class Movable_ () extends Movable

object Movable { def mk   : Movable  = Movable_  () }

/* It is recommended to indent the constants and functions declared inside. */

trait EqualsExample
{

  /* A constant does not have parameters and it is declared with the equals sign (`=`).
   * It is recommended to use snake case and start in lowercase.
   * The constant name should be a noun. */

  lazy val my_number : Int = 2

  /* A function has parameters and a type.
   * Functions, even with empty parameters, are evaluated every time they are invoked. */

  def f (x : Int) (y : Int) : Int = 2 * x + y

  /* Constants are only evaluated once, which is the first time they are needed. */

  lazy val first_result : Int = f (12) (4)

  /* In a function call, the parameters can be specified with the colon-equals sign (`:=`).
   * This is especially recommended when several parameters are of the same type. */

  lazy val second_result : Int = f (x = 20) (y = -10)

}

case class EqualsExample_ () extends EqualsExample

object EqualsExample { def mk   : EqualsExample  = EqualsExample_  () }

/* A class can extend another one by using `extends`.
 * Abstract classes cannot be instantiated but can be extended.
 * Conversely, concrete classes cannot be extended but can be instantiated.
 * Concrete classes are declared with parentheses `(` and `)`.
 * It is recommended that concrete classes do not have a body, because this cannot be reused.
 * Concrete classes extending only one class could be named as its superclass, but ending with
 * an underscore. */

/* A class does not need to define all its constants and functions. */

trait RegisteredPerson
{

  /* A block starting with `abstract` denotes a constant or function that needs to be defined in
   * extending classes. Only one `abstract` block should be defined per class, without leaving
   * lines between the declared attributes. */

  def   first_name : String
  def   last_name : String

  /* If a constant or function is not meant to be exported, its name should start with an
   * underscore. */

  private lazy val _separator = " "

  /* Strings can be concatenated by using the plus sign (`+`). */

  lazy val full_name = first_name + _separator + last_name

}

case class RegisteredPerson_ (first_name : String, last_name : String) extends RegisteredPerson

object RegisteredPerson { def mk  (first_name : String) (last_name : String) : RegisteredPerson  = RegisteredPerson_  (first_name, last_name) }

trait Agent
{

  def   identifier : String

}

case class Agent_ (identifier : String) extends Agent

object Agent { def mk  (identifier : String) : Agent  = Agent_  (identifier) }

/* A concrete class needs as parameters all the constants and functions that have not been
 * defined in its super classes. */

trait RegisteredPersonAgent
  extends
    Agent
    with RegisteredPerson
{

  def   identifier : String
  def   first_name : String
  def   last_name : String

}

case class RegisteredPersonAgent_ (identifier : String, first_name : String, last_name : String) extends RegisteredPersonAgent

object RegisteredPersonAgent { def mk  (identifier : String) (first_name : String) (last_name : String) : RegisteredPersonAgent  = RegisteredPersonAgent_  (identifier, first_name, last_name) }

trait Element
{

  def   accept : Visitor => Boolean

}

case class Element_ (accept : Visitor => Boolean) extends Element

object Element { def mk  (accept : Visitor => Boolean) : Element  = Element_  (accept) }

trait Visitor
{

  def   visit : Element => Boolean

}

case class Visitor_ (visit : Element => Boolean) extends Visitor

object Visitor { def mk  (visit : Element => Boolean) : Visitor  = Visitor_  (visit) }

trait Item
  extends Element
{

  def   identifier : Int

  /* It is possible to refer to an object instance by using `this`.
   * The dot (`.`) notation is the standard way of accessing attributes and methods of an
   * object. The space before the dot is to improve readability. */

  lazy val accept : Visitor => Boolean =
     visitor =>
      visitor .visit (this)

}

case class Item_ (identifier : Int) extends Item

object Item { def mk  (identifier : Int) : Item  = Item_  (identifier) }

trait PersonName
{

  def   name : String

  /* It is possible to override a function by using the `@override` annotation.
   * This is intended only for exceptional cases, like the `toString` function, or a
   * diamond-shaped class hierarchy. */

  override
  lazy val toString = name

}

case class PersonName_ (name : String) extends PersonName

object PersonName { def mk  (name : String) : PersonName  = PersonName_  (name) }

/* A class can be parameterized using square brackets ('[' and ']').
 * The parameter needs to be of type Type. */

trait MyList [A ]
{

}

case class MyList_ [A] () extends MyList [A]

object MyList { def mk  [A]  : MyList  [A] = MyList_  [A] () }

/* It is possible to have multiple type parameters. */

trait MyPair [A , B ]
{

  def   fst : A
  def   snd : A

}

case class MyPair_ [A, B] (fst : A, snd : A) extends MyPair [A, B]

object MyPair { def mk  [A, B] (fst : A) (snd : A) : MyPair  [A, B] = MyPair_  [A, B] (fst, snd) }

/* The parameter type can be constrained using `subtype` and `supertype`.
 * In that case, it is not necessary to declare the parameter to be of type Type.
 * For example, `A subtype B` means that `A` is a subtype of `B`. */

trait ShapePainter [A <: Shape]
{

}

case class ShapePainter_ [A <: Shape] () extends ShapePainter [A]

object ShapePainter { def mk  [A <: Shape]  : ShapePainter  [A] = ShapePainter_  [A] () }

/**
  * This contains the examples shown in the manual.
  */

trait Manual
{

/* It is possible to import classes by listing them under an `import` reserved word. */

  import   java.util.Date

  lazy val a = 1

  lazy val b : Int = 2

  /* An instance of a JVM class can be created with the `@new` annotation. If the code is
   * translated to Scala 3, this annotation is not required. */

  lazy val now : Date = new Date ()

  def plus_one (x : Int) : Int = x + 1

  /* A piecewise function can be defined using an `if`-`then`-`else` structure. The condition in
   * the `if` is evaluated, and then only the corresponding branch is evaluated. */

  def max (x : Int) (y : Int) : Int =
    if ( x > y
    ) x
    else y

  /* Scala sequences (`Seq`) can be used, as well as other basic Scala classes.
   * Lambda functions are declared with `lambda` and a long right arrow (`-->`). */

  def plus_one (sequence : Seq [Int] ) : Seq [Int] =
    sequence .map ( element => element + 1)

  /* A synonym for `lambda` is `any`, which sometimes brings more readability. */

  def plus_two (sequence : Seq [Int] ) : Seq [Int] =
    sequence .map ( element => element + 2)

  /* Boolean values `false` and `true` are available. */

  def my_not (x : Boolean) : Boolean =
    if ( x
    ) false
    else true

  def my_and (x : Boolean) (y : Boolean) : Boolean =
    if ( x
    ) y
    else false

  def my_or (x : Boolean) (y : Boolean) : Boolean =
    if ( x
    ) true
    else y

  /* Boolean values have the standard `not`-`and`-`or` functions. */

  def my_xor (x : Boolean) (y : Boolean) : Boolean =
    (x || y) && ! (x && y)

  /* It is possible to use pattern matching with `match` and `case`.
   * The result of the matching case is put after a long double arrow `==>`.
   * The order matters, so the remaining cases are capture by the last variable. */

  def if_then_else [A ] (condition : Boolean) (if_true : A) (if_false : A) : A =
    condition match  {
      case true => if_true
      case otherwise => if_false
    }

  /* A constant or function name starting with underscore indicates that the constant or
   * function is private, and therefore is not visible outside the class. */

   private def _my_private_function (x : Float) : Float =
     x * x + x + 1

  /* A tail recursive function cannot be declared inside another function, and its name should
   * start with underscore. The annotation `@tailrec` helps ensuring that the tail recursion is
   * detected and optimized. */

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_sum (n : Int) (accum : Int) : Int =
    if ( n < 0
    ) accum
    else _tailrec_sum (n - 1) (n + accum)

  def sum (n : Int) =
    _tailrec_sum (n) (0)

}

case class Manual_ () extends Manual

object Manual { def mk   : Manual  = Manual_  () }

/* The function used to compare equality is a long equals (`==`). */

trait FactorialConcise
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_get_factorial (n : Int) (product : Int) : Int =
    if ( n == 0
    ) product
    else _tailrec_get_factorial (n - 1) (n * product)

  /* Ideally, each object should do exactly one thing. That thing is what `apply` defines. */

  def apply (n : Int) : Int =
    _tailrec_get_factorial (n) (1)

}

case class FactorialConcise_ () extends FactorialConcise

object FactorialConcise { def mk   : FactorialConcise  = FactorialConcise_  () }

/* FoldWhile captures the idea of a bounded loop. It starts in an initial state (`initial`) and
 * processes (`next`) a finite sequence (`sequence`) while a condition holds (`condition`). */

trait FoldWhile
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold_while [A , B ] (sequence : Seq [A] ) (current : B)
      (next : B => A => B) (condition : B => A => Boolean) : B =
    if ( sequence .isEmpty || (! condition (current) (sequence .head) )
    ) current
    else _tailrec_fold_while (sequence .tail) (next (current) (sequence .head) ) (next) (
      condition)

  def apply [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B)
    (condition : B => A => Boolean)
      : B =
    _tailrec_fold_while (sequence) (initial) (next) (condition)

}

case class FoldWhile_ () extends FoldWhile

object FoldWhile { def mk   : FoldWhile  = FoldWhile_  () }

/* Range generates a sequence of length `n` of consecutive natural numbers starting
 * from 0. */

trait Range
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_range (n : Int) (sequence : Seq [Int] ) : Seq [Int] =
    if ( n <= 0
    ) sequence
    else _tailrec_range (n - 1) (sequence .+: (n - 1) )

  def apply (length : Int) : Seq [Int] =
    _tailrec_range (length) (Seq [Int] () )

}

case class Range_ () extends Range

object Range { def mk   : Range  = Range_  () }

/* The main class has to be named `Main` and requires a `main` function that receives an
 * `Array [String]` and returns a `Unit`. Only one main class per package is allowed. */

trait Main
{

  /* An output to the standard output can be sent with a `println` command.
   * This is a shorter form of JVM's `System.out.println`. */

  def main (arguments : Array [String] ) : Unit =
    println ("Hello world!")

}

object EntryPoint {
  def main (args: Array [String]): Unit = Main_ ().main (args)
}


case class Main_ () extends Main

object Main { def mk   : Main  = Main_  () }

/* The main class has an extending concrete class. */
