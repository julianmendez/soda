package soda.example.algorithms

/*
 * This package contains examples in Soda.
 * These examples focus on simple algorithms.
 */

trait Package

trait FizzBuzz
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val range = soda.lib.Range_ ()

  def is_div (n : Int) (k : Int) : Boolean =
    n % k == 0

  def get_term (n : Int) : String =
    if ( is_div (n) (15) ) fizz + buzz
    else if ( is_div (n) (3) ) fizz
    else if ( is_div (n) (5) ) buzz
    else n .toString

  lazy val apply : Seq [String] =
    range .apply (100)
      .map ( x => x + 1)
      .map (get_term)

}

case class FizzBuzz_ () extends FizzBuzz

object FizzBuzz {
  def mk : FizzBuzz =
    FizzBuzz_ ()
}


trait FizzBuzzPatternMatching
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val range = soda.lib.Range_ ()

  def get_term (n : Int) : String =
    Tuple2 (n % 3 , n % 5) match  {
      case Tuple2 (0 , 0) => fizz + buzz
      case Tuple2 (0 , x) => fizz
      case Tuple2 (x , 0) => buzz
      case otherwise => n .toString
    }

  lazy val apply : Seq [String] =
    range .apply (100)
      .map ( x => x + 1)
      .map (get_term)

}

case class FizzBuzzPatternMatching_ () extends FizzBuzzPatternMatching

object FizzBuzzPatternMatching {
  def mk : FizzBuzzPatternMatching =
    FizzBuzzPatternMatching_ ()
}


trait FizzBuzzPatternUnicode
{

  lazy val fizz = "Fizz"

  lazy val buzz = "Buzz"

  lazy val range = soda.lib.Range_ ()

  def get_term (n : Int) : String =
    Tuple2 (n % 3 , n % 5) match  {
      case Tuple2 (0 , 0) => fizz + buzz
      case Tuple2 (0 , x) => fizz
      case Tuple2 (x , 0) => buzz
      case otherwise => n .toString
    }

  lazy val apply : Seq [String] =
    range.apply (100)
      .map ( x => x + 1)
      .map (get_term)

}

case class FizzBuzzPatternUnicode_ () extends FizzBuzzPatternUnicode

object FizzBuzzPatternUnicode {
  def mk : FizzBuzzPatternUnicode =
    FizzBuzzPatternUnicode_ ()
}


trait Main
{

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


trait Singleton
  extends
    Parameter
{

  def   x : Int

  lazy val name = "singleton"

}

case class Singleton_ (x : Int) extends Singleton

object Singleton {
  def mk (x : Int) : Singleton =
    Singleton_ (x)
}

trait Pair
  extends
    Parameter
{

  def   x : Int
  def   y : Int

  lazy val name = "pair"

}

case class Pair_ (x : Int, y : Int) extends Pair

object Pair {
  def mk (x : Int) (y : Int) : Pair =
    Pair_ (x, y)
}

trait Triplet
  extends
    Parameter
{

  def   x : Int
  def   y : Int
  def   z : Int

  lazy val name = "triplet"

}

case class Triplet_ (x : Int, y : Int, z : Int) extends Triplet

object Triplet {
  def mk (x : Int) (y : Int) (z : Int) : Triplet =
    Triplet_ (x, y, z)
}

trait Parameter
{

  def   name : String

}

case class Parameter_ (name : String) extends Parameter

object Parameter {
  def mk (name : String) : Parameter =
    Parameter_ (name)
}

trait PatternMatching
{

  def get_value (p : Parameter) : Int =
    p match  {
      case Singleton_ (x) => x
      case Pair_ (x, y) => (x + y) / 2
      case Triplet_ (x, y, z) => (x + y + z) / 3
      case otherwise => 0
    }

  def get_type_name (p : Parameter) : String =
    p match  {
      case Singleton_ (x) => (Singleton_ (x) ) .name + " (x)"
      case Pair_ (x, y) => (Pair_ (x, y) ) .name + " (x) (y)"
      case Triplet_ (x, y, z) => (Triplet_ (x, y, z) ) .name + " (x) (y) (z)"
      case otherwise => ""
    }

}

case class PatternMatching_ () extends PatternMatching

object PatternMatching {
  def mk : PatternMatching =
    PatternMatching_ ()
}


trait SaladMaker
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_prepare_salad [Ingredient , Salad ]
    (ingredients : Seq [Ingredient] )
    (salad : Salad)
    (next_ingredient : Salad => Ingredient => Salad)
    (condition : Salad => Ingredient => Boolean)
      : Salad =
    if ( ingredients .isEmpty ||
      (! condition (salad) (ingredients .head) )
    ) salad
    else _tailrec_prepare_salad (ingredients .tail) (
      next_ingredient (salad) (ingredients .head) ) (next_ingredient) (condition)

  def apply [Ingredient , Salad ]
    (list_of_ingredients : Seq [Ingredient] )
    (initial_bowl : Salad)
    (next_ingredient : Salad => Ingredient => Salad)
    (condition : Salad => Ingredient => Boolean)
      : Salad =
    _tailrec_prepare_salad (list_of_ingredients) (initial_bowl) (next_ingredient) (condition)

}

case class SaladMaker_ () extends SaladMaker

object SaladMaker {
  def mk : SaladMaker =
    SaladMaker_ ()
}


trait MyPair [A , B ]
{

  def   key : A
  def   value : B

}

case class MyPair_ [A, B] (key : A, value : B) extends MyPair [A, B]

object MyPair {
  def mk [A, B] (key : A) (value : B) : MyPair [A, B] =
    MyPair_ [A, B] (key, value)
}

trait ScalaReservedWordEscaping
{

  private lazy val __soda__var = "var"

  private lazy val __soda__val = 1

  def g [A , B ] (key : A) (value : B) : MyPair [A, B] = MyPair_ (key , value)

  private def __soda__while [A , B ] (seq : Seq [A] ) (cond : A => Boolean) (func : A => B) : Seq [B] =
    seq .takeWhile (cond) .map (func)

  private lazy val __soda__protected = "protected"

  private lazy val __soda__private = "private"

  def f (x : Int) (y : Int) : Int = x + y

  lazy val cons : Int => Int => Int =
     x =>
       y =>
        f (x) (y)

}

case class ScalaReservedWordEscaping_ () extends ScalaReservedWordEscaping

object ScalaReservedWordEscaping {
  def mk : ScalaReservedWordEscaping =
    ScalaReservedWordEscaping_ ()
}


trait SortExample
{

  def   is_sorted : Seq [Int] => Boolean

}

case class SortExample_ (is_sorted : Seq [Int] => Boolean) extends SortExample

object SortExample {
  def mk (is_sorted : Seq [Int] => Boolean) : SortExample =
    SortExample_ (is_sorted)
}

trait SortExampleWithAt
  extends
    SortExample
{

  def is_sorted_for (sequence : Seq [Int] ) : Boolean =
    sequence
      .indices
      .filter ( index => index > 0)
      .forall ( index => sequence .apply (index - 1) <= sequence .apply (index) )

  lazy val is_sorted : Seq [Int] => Boolean =
     sequence => is_sorted_for (sequence)

}

case class SortExampleWithAt_ () extends SortExampleWithAt

object SortExampleWithAt {
  def mk : SortExampleWithAt =
    SortExampleWithAt_ ()
}

trait SortExampleWithZip
  extends
    SortExample
{

  def is_sorted_for (sequence : Seq [Int] ) : Boolean =
    sequence
      .zip (sequence .tail)
      .forall ( pair => (pair ._1 <= pair ._2) )

  lazy val is_sorted : Seq [Int] => Boolean =
     sequence => is_sorted_for (sequence)

}

case class SortExampleWithZip_ () extends SortExampleWithZip

object SortExampleWithZip {
  def mk : SortExampleWithZip =
    SortExampleWithZip_ ()
}

trait SortAlgorithmExample
{

  def   sort : Seq [Int] => Seq [Int]

}

case class SortAlgorithmExample_ (sort : Seq [Int] => Seq [Int]) extends SortAlgorithmExample

object SortAlgorithmExample {
  def mk (sort : Seq [Int] => Seq [Int]) : SortAlgorithmExample =
    SortAlgorithmExample_ (sort)
}

trait SortAlgorithmExampleWithFold
  extends
    SortAlgorithmExample
{

  import   soda.lib.Fold_

  private lazy val _fold = Fold_ ()

  private lazy val _initial_value = Seq [Int] ()

  private def _next_value_function (current_sequence : Seq [Int] ) (elem : Int) : Seq [Int] =
    insert_sorted (current_sequence) (elem)

  def sort_for (sequence : Seq [Int] ) : Seq [Int] =
    if ( sequence .length < 2
    ) sequence
    else _fold .apply (sequence) (_initial_value) (_next_value_function)

  lazy val sort : Seq [Int] => Seq [Int] =
     sequence => sort_for (sequence)

  def concatenate (first_part : Seq [Int] ) (middle : Seq [Int] ) (last_part : Seq [Int] )
      : Seq [Int] =
    first_part .++ (middle .++ (last_part) )

  def insert_sorted (sequence : Seq [Int] ) (element : Int) : Seq [Int] =
    concatenate (
      first_part = sequence .takeWhile ( x => x < element) ) (
      middle = Seq (element) ) (
      last_part = sequence .dropWhile ( x => x < element)
    )

}

case class SortAlgorithmExampleWithFold_ () extends SortAlgorithmExampleWithFold

object SortAlgorithmExampleWithFold {
  def mk : SortAlgorithmExampleWithFold =
    SortAlgorithmExampleWithFold_ ()
}

trait ConstrainedSortAlgorithm
{

  import   soda.lib.OptionSD
  import   soda.lib.SomeSD_
  import   soda.lib.NoneSD_

  def sort_with (sorted_sequence : Seq [Int] ) : OptionSD [Seq [Int] ] =
    if ( SortExampleWithZip_ () .is_sorted (sorted_sequence)
    ) SomeSD_ (sorted_sequence)
    else NoneSD_ [Seq [Int] ] ()

  def sort (sequence : Seq [Int] ) : OptionSD [Seq [Int] ] =
    sort_with (SortAlgorithmExampleWithFold_ () .sort (sequence) )

}

case class ConstrainedSortAlgorithm_ () extends ConstrainedSortAlgorithm

object ConstrainedSortAlgorithm {
  def mk : ConstrainedSortAlgorithm =
    ConstrainedSortAlgorithm_ ()
}

trait SortedSequence [A <: Comparable [A] ]
{

  def   sequence : Seq [A]
  def   add : A => SortedSequence [A]
  def   invariant : Boolean

}

case class SortedSequence_ [A <: Comparable [A]] (sequence : Seq [A], add : A => SortedSequence [A], invariant : Boolean) extends SortedSequence [A]

object SortedSequence {
  def mk [A <: Comparable [A]] (sequence : Seq [A]) (add : A => SortedSequence [A]) (invariant : Boolean) : SortedSequence [A] =
    SortedSequence_ [A] (sequence, add, invariant)
}

trait EmptySortedSequence [A <: Comparable [A] ]
  extends
    SortedSequence [A]
{

  lazy val sequence = Seq ()

  def add_for (element : A) : SortedSequence [A] =
    NonEmptySortedSequence_ (Seq (element) )

  lazy val add : A => SortedSequence [A] =
     element => add_for (element)

  lazy val invariant : Boolean = true

}

case class EmptySortedSequence_ [A <: Comparable [A]] () extends EmptySortedSequence [A]

object EmptySortedSequence {
  def mk [A <: Comparable [A]] : EmptySortedSequence [A] =
    EmptySortedSequence_ [A] ()
}

trait SortedSequenceWithElements [A <: Comparable [A] ]
  extends
    SortedSequence [A]
{

  def   sequence : Seq [A]
  def   add : A => SortedSequence [A]
  def   invariant : Boolean

}

case class SortedSequenceWithElements_ [A <: Comparable [A]] (sequence : Seq [A], add : A => SortedSequence [A], invariant : Boolean) extends SortedSequenceWithElements [A]

object SortedSequenceWithElements {
  def mk [A <: Comparable [A]] (sequence : Seq [A]) (add : A => SortedSequence [A]) (invariant : Boolean) : SortedSequenceWithElements [A] =
    SortedSequenceWithElements_ [A] (sequence, add, invariant)
}

trait NonEmptySortedSequence [A <: Comparable [A] ]
  extends
    SortedSequenceWithElements [A]
{

  def   sequence : Seq [A]

  lazy val aux = NonEmptySortedSequenceAux_ [A] ()

  def add_for (element : A) : SortedSequence [A] =
    NonEmptySortedSequence_ (aux .insert_sorted (sequence) (element) )

  lazy val add : A => SortedSequence [A] =
     element => add_for (element)

  lazy val invariant : Boolean = aux .is_sorted (sequence)

}

case class NonEmptySortedSequence_ [A <: Comparable [A]] (sequence : Seq [A]) extends NonEmptySortedSequence [A]

object NonEmptySortedSequence {
  def mk [A <: Comparable [A]] (sequence : Seq [A]) : NonEmptySortedSequence [A] =
    NonEmptySortedSequence_ [A] (sequence)
}

trait NonEmptySortedSequenceAux [A <: Comparable [A] ]
{

  def is_less_than (x : A) (y : A) : Boolean =
    x .compareTo (y) < 0

  def is_sorted (other_sequence : Seq [A] ) : Boolean =
    other_sequence
      .zip (other_sequence .tail)
      .forall ( pair => is_less_than (pair ._1) (pair ._2) )

  def concatenate (first_part : Seq [A] ) (middle : Seq [A] ) (last_part : Seq [A] ) : Seq [A] =
    first_part .++ (middle .++ (last_part) )

  def insert_sorted (original_sequence : Seq [A] ) (element : A) : Seq [A] =
    concatenate (
      first_part = original_sequence.takeWhile ( x => is_less_than (x) (element) ) ) (
      middle = Seq (element) ) (
      last_part = original_sequence.dropWhile ( x => is_less_than (x) (element) )
    )

}

case class NonEmptySortedSequenceAux_ [A <: Comparable [A]] () extends NonEmptySortedSequenceAux [A]

object NonEmptySortedSequenceAux {
  def mk [A <: Comparable [A]] : NonEmptySortedSequenceAux [A] =
    NonEmptySortedSequenceAux_ [A] ()
}

trait SortedSequenceBuilder [A <: Comparable [A] ]
{

  import   soda.lib.Fold_

  private lazy val _fold = Fold_ ()

  private lazy val _initial_value : SortedSequence [A] = EmptySortedSequence_ [A] ()

  private def _next_value_function (sorted_sequence : SortedSequence [A] ) (element : A)
      : SortedSequence [A] =
    sorted_sequence .add (element)

  def build (sequence : Seq [A] ) : SortedSequence [A] =
    _fold .apply (sequence) (_initial_value) (_next_value_function)

}

case class SortedSequenceBuilder_ [A <: Comparable [A]] () extends SortedSequenceBuilder [A]

object SortedSequenceBuilder {
  def mk [A <: Comparable [A]] : SortedSequenceBuilder [A] =
    SortedSequenceBuilder_ [A] ()
}

