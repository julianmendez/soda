package soda.manual

/* This is a Soda tutorial written in Soda.
 * Copyright 2020--2025 Julian Alfredo Mendez
 * Version: 2025-03-28 */

/* This tutorial is itself a "Hello world!" program.
 * The piece of code that prints the message is at the end of the file. */

/* Source code written in Soda is intended to be descriptive and readable.
 * It is usually written in different files, and each file has 'blocks'.
 * These blocks are pieces of code that have some meaning.
 * A block should be short, maybe less than 10 lines.
 * However, it is more important to make things clear than concise.
 * These are examples of blocks:
 * 1. a constant or function definition
 * 2. the beginning of a class definition
 * 3. the end of a class definition
 * 4. a block declaration of abstract constants and functions
 * 5. a definition of an algebraic data type
 * 6. a block of imports
 * 7. a package declaration
 * 8. a class alias
 * 9. a comment */

/* To declare a class, just add the reserved word `class` before a class name, and end it with
 * the reserved word `end`.
 * The reserved word `abstract` explicitly states whether constants and functions are required
 * to instantiate a class. In the example below, no constants or functions are required.
 * For the class name, it is recommended to use camel case style starting with a capital letter.
 * The name could be a noun phrase or an adjective, but it should not be a verb. */

trait Shape
{



}

case class Shape_ () extends Shape

object Shape {
  def mk : Shape =
    Shape_ ()
}

/* The reserved word `class` declares a type, a namespace, and a default constructor.
 * It is possible to instantiate a class using the function `mk`. The notation looks
 * as a static function of the type. For example, for `Shape` is `Shape .mk` without
 * parentheses. */

trait Movable
{



}

case class Movable_ () extends Movable

object Movable {
  def mk : Movable =
    Movable_ ()
}

/* It is recommended to indent the constants and functions declared inside. */

trait EqualsExample
{

  /* Note that `abstract` does not contain any constants or functions in this example, and the
   * declared constants and functions are in a different block. */



  /* A constant does not have parameters and it is declared with the equals sign (`=`).
   * For the constant name, it is recommended to use snake case and start in lowercase.
   * The constant name should be a noun phrase. */

  lazy val my_number : Int = 2

  /* A function has parameters and a type. Functions, even with empty parameters, are evaluated
   * every time they are invoked. The standard way of declaring and invoking a function
   * with multiple parameters is with parameters separated by spaces and not by commas.
   * For example, use `f (x) (y)` instead of `f(x, y)`. */

  def f (x : Int) (y : Int) : Int = 2 * x + y

  /* Constants are only evaluated once, which is the first time they are needed. */

  lazy val first_result : Int = f (12) (4)

  /* In a function call, the parameters can be specified with the colon-equals sign (`:=`).
   * This is especially recommended when several parameters are of the same type. */

  lazy val second_result : Int = f (x = 20) (y = -10)

  /* It is allowed to use the optional reserved word `def` to define constants and functions. */

  lazy val a_constant : Int = 1

  def a_function (x : Int) : Int = x * x

}

case class EqualsExample_ () extends EqualsExample

object EqualsExample {
  def mk : EqualsExample =
    EqualsExample_ ()
}

/* It is possible to define algebraic data types, especially inductive data types.
 * The reserved word for that definition is `inductive` with the name of the data type.
 * The constructors are defined after the line with the `inductive` reserved word.
 * Each constructor takes a possibly empty number of parameters and returns an element of the type being defined.
 * Each parameter is separated by the type arrow (`->`) and the same is used as a result of the construction.
 * It is mandatory to give a name to each parameter.
 */

sealed trait NaturalNumber

case object Zero  extends NaturalNumber

case class Successor (n : NaturalNumber) extends NaturalNumber


/* A class can extend other classes by using the reserved word `extends`.
 * Abstract classes contain a block with the reserved word `abstract`, and they are the only
 * classes that can be extended.
 * By contrast, concrete classes are declared with parentheses `(` and `)` and cannot be
 * extended. Algebraic data types cannot be extended either. */

trait RegisteredPerson
{

  /* The block starting with `abstract` denotes constants or functions that need to be defined
   * in extending classes. Only one `abstract` block should be defined per class, without
   * leaving lines between the declared attributes. */

  def   first_name : String
  def   last_name : String

  /* If a constant or function is not meant to be exported, its name should start with an
   * underscore. */

  private lazy val _separator = " "

  /* Strings can be concatenated by using the plus sign (`+`). */

  lazy val full_name = first_name + _separator + last_name

}

case class RegisteredPerson_ (first_name : String, last_name : String) extends RegisteredPerson

object RegisteredPerson {
  def mk (first_name : String) (last_name : String) : RegisteredPerson =
    RegisteredPerson_ (first_name, last_name)
}

trait Agent
{

  def   identifier : String

}

case class Agent_ (identifier : String) extends Agent

object Agent {
  def mk (identifier : String) : Agent =
    Agent_ (identifier)
}

/* A concrete class needs as parameters all the constants and functions that have not been
 * defined in its super classes. Observe that `extends` has to be in the same block as
 * `class`, which need to be in a different block from `abstract`. */

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

object RegisteredPersonAgent {
  def mk (identifier : String) (first_name : String) (last_name : String) : RegisteredPersonAgent =
    RegisteredPersonAgent_ (identifier, first_name, last_name)
}

trait Element
{

  /* In this class, `accept` is a function that takes an object of type `Visitor` and returns
   * an object of type `Boolean`. This is indicated with the type arrow `->`. */

  def   accept : Visitor => Boolean

}

case class Element_ (accept : Visitor => Boolean) extends Element

object Element {
  def mk (accept : Visitor => Boolean) : Element =
    Element_ (accept)
}

trait Visitor
{

  /* In this class, `visit` is a function defined from `Element` to `Boolean`. */

  def   visit : Element => Boolean

}

case class Visitor_ (visit : Element => Boolean) extends Visitor

object Visitor {
  def mk (visit : Element => Boolean) : Visitor =
    Visitor_ (visit)
}

trait Item
  extends Element
{

  def   identifier : Int

  /* It is possible to refer to an object instance by using reserved word `this`.
   * The dot (`.`) notation is the standard way of accessing attributes and methods of an
   * object. The space before the dot is to improve readability and it is not necessary.
   * Lambda functions are declared with the reserved word `lambda` and a long arrow (`-->`).
   * Notice the difference between the type arrow (`->`) and the lambda arrow (`-->`). */

  lazy val accept : Visitor => Boolean =
     visitor =>
      visitor .visit (this)

}

case class Item_ (identifier : Int) extends Item

object Item {
  def mk (identifier : Int) : Item =
    Item_ (identifier)
}

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

object PersonName {
  def mk (name : String) : PersonName =
    PersonName_ (name)
}

/* A class can be parameterized using square brackets ('[' and ']').
 * The class parameter needs to be of type Type. */

trait MyList [A ]
{



}

case class MyList_ [A] () extends MyList [A]

object MyList {
  def mk [A] : MyList [A] =
    MyList_ [A] ()
}

/* It is possible to have multiple type parameters. */

trait MyPair [A , B ]
{

  def   fst : A
  def   snd : B

}

case class MyPair_ [A, B] (fst : A, snd : B) extends MyPair [A, B]

object MyPair {
  def mk [A, B] (fst : A) (snd : B) : MyPair [A, B] =
    MyPair_ [A, B] (fst, snd)
}

/* It is possible to define a pair as a data type. */

sealed trait AnotherPair [A , B]

case class AnotherPair_ [A , B] (fst : A , snd : B) extends AnotherPair [A , B]


/* It is possible to define a triple as a data type. */

sealed trait Triple [A , B , C]

case class Triple_ [A , B , C] (fst : A , snd : B , trd : C) extends Triple [A , B , C]


/* The parameter type can be constrained using `subtype` and `supertype`.
 * In that case, it is not necessary to declare the parameter to be of type Type.
 * For example, `A subtype B` means that `A` is of type Type and it is a subtype of `B`. */

trait ShapePainter [A <: Shape]
{



}

case class ShapePainter_ [A <: Shape] () extends ShapePainter [A]

object ShapePainter {
  def mk [A <: Shape] : ShapePainter [A] =
    ShapePainter_ [A] ()
}

/* The following class contains examples given as pieces of code. */

trait Manual
{



  /* The first line in this file is the package declaration. It contains the reserved word
   * `package` followed by the package name. The recommended package naming convention is to
   * start with `soda.`, which helps to avoid name conflicts when it is translated to Scala.
   * The package declaration usually goes in a separate file called `Package.soda`. */

  /* It is possible to import classes by listing them under the reserved word `import`.
   * Imported classes can also be declared in the `Package.soda` file, when they are global
   * for the whole package. The list of imported classes can be used to detect and control which
   * classes may be producing side effects. */

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

  /* Scala sequences (`Seq`) can be used, as well as other basic Scala classes. */

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

  /* It is possible to use pattern matching with the reserved words `match` and `case`.
   * The result of the matching case is put after a long double arrow `==>`.
   * The order matters, so the remaining cases are capture by the last variable.
   * Please notice the difference between the case arrow (`==>`), the lambda arrow (`-->`), and
   * the type arrow (`->`). */

  def if_then_else [A ] (condition : Boolean) (if_true : A) (if_false : A) : A =
    condition match  {
      case true => if_true
      case false => if_false
    }

  /* It is possible to use pattern matching to obtain the content of an object.
   * In the case of pattern matching, the constructor name is the class name with an underscore
   * as suffix.
   * Unlike with `mk`, the parameters need to be retrieved all together in a tuple. */

  def get_left [A , B ] (pair : MyPair [A, B]) : A =
    pair match  {
      case MyPair_ (left , right) => left
    }

  /* A constant or function name starting with underscore indicates that the constant or
   * function is private, and therefore is not visible outside the class. */

   private def _my_private_function (x : Float) : Float =
     x * x + x + 1

  /* A tail recursive function cannot be declared inside another function, and its name should
   * start with underscore. The annotation `@tailrec` helps to ensure that the tail recursion
   * is detected and optimized when it is translated to Scala. */

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

object Manual {
  def mk : Manual =
    Manual_ ()
}

/* The class `Fold` has a 'left fold', which is a functional approach to iterations. Starting
 * with an initial value (`initial`), it traverses a sequence (`sequence`) applying a function
 * (`next`) using the current element in the sequence and the result of the previous
 * computation. */

trait Fold
{



  /* Note that the type parameters must be specified in the function call:
   * `_tailrec_foldl [A] [B]`...
   * The sequence constructor `+:` is defined by `Seq`.
   * This is equivalent to the more common constructor `::`, when it is used for instances of
   * `List`. `Nil` is the constructor for an empty `Seq`. */

  private def _tailrec_foldl [A , B ] (sequence : Seq [A] ) (current : B)
      (next : B => A => B) : B =
    sequence match  {
      case Nil => current
      case (head) +: (tail) =>
        _tailrec_foldl [A, B] (tail) (next (current) (head) ) (next)
    }

  /* Ideally, each object should have one responsibility or purpose. The function `apply`
   * defines the main responsibility of an object. */

  def apply [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B) : B =
    _tailrec_foldl [A, B] (sequence) (initial) (next)

}

case class Fold_ () extends Fold

object Fold {
  def mk : Fold =
    Fold_ ()
}

/* A piece of code of the destination language can be included with the reserved word
 * `directive`. In this example, we can define the concept of successor for integers for the
 * translation to Scala. */

object Succ_ {
  def unapply (n : Int) : Option [Int] =
    if (n <= 0) None else Some (n - 1)
}

/* The class `Range` generates a sequence of length `n` of consecutive natural numbers starting
 * from 0. */

trait Range
{



  /* Note that `Range` processes any negative number as 0. This is done by `Succ_`, which only
   * returns values for positive values of `n`. */

  private def _tailrec_range (non_negative_number : Int) (sequence : Seq [Int] ) : Seq [Int] =
    non_negative_number match  {
      case Succ_ (k) =>
        _tailrec_range (k) ( (k) +: (sequence) )
      case _otherwise => sequence
    }

  def apply (length : Int) : Seq [Int] =
    _tailrec_range (length) (Nil)

}

case class Range_ () extends Range

object Range {
  def mk : Range =
    Range_ ()
}

/* `Factorial` shows an example of how to compute the function factorial using a left fold. */

trait Factorial
{



  lazy val fold = Fold .mk

  lazy val range = Range .mk

  def apply (n : Int) : Int =
    fold .apply [Int, Int] (range .apply (n) ) (1) (
       accum =>
         k => (accum * (k + 1) ) )

}

case class Factorial_ () extends Factorial

object Factorial {
  def mk : Factorial =
    Factorial_ ()
}

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

object Main {
  def mk : Main =
    Main_ ()
}

/* The main class has an extending concrete class. The class that needs to be invoked in a
 * translation to Scala is `EntryPoint`. */
