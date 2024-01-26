package soda.tiles.fairness.tool

/*
 * This package contains test for the classes to model a fairness scenario.
 */

import   org.scalatest.funsuite.AnyFunSuite

trait Package

trait RandomNumberGenerator
{

  def   get_next_long : Long => Long
  def   get_next_seq : Long => Int => Seq [Long]

}

case class RandomNumberGenerator_ (get_next_long : Long => Long, get_next_seq : Long => Int => Seq [Long]) extends RandomNumberGenerator

object RandomNumberGenerator {
  def mk (get_next_long : Long => Long) (get_next_seq : Long => Int => Seq [Long]) : RandomNumberGenerator =
    RandomNumberGenerator_ (get_next_long, get_next_seq)
}

trait LinearCongruentialGenerator
  extends
    RandomNumberGenerator
{

  def   modulus : Long
  def   multiplier : Long
  def   increment : Long

  lazy val get_next_long : Long => Long =
     seed =>
      (multiplier * seed + increment) % modulus

  private def _get_next_seq_initial_value (seed : Long) : Seq [Long] =
    Seq (seed)

  private def _get_next_seq_next_value_function (list : Seq [Long] ) (x : Int) : Seq [Long] =
    list .+: (get_next_long (list .head) )

  lazy val get_next_seq : Long => Int => Seq [Long] =
     seed =>
       length =>
        Recursion_ ()
          .fold (Recursion_ () .range (length - 1) .map ( x => x + 1) ) (
            _get_next_seq_initial_value (seed) ) (_get_next_seq_next_value_function)
          .reverse

}

case class LinearCongruentialGenerator_ (modulus : Long, multiplier : Long, increment : Long) extends LinearCongruentialGenerator

object LinearCongruentialGenerator {
  def mk (modulus : Long) (multiplier : Long) (increment : Long) : LinearCongruentialGenerator =
    LinearCongruentialGenerator_ (modulus, multiplier, increment)
}

trait Random
  extends
    RandomNumberGenerator
{

  lazy val generator = LinearCongruentialGenerator_ (0x1000000000000L , 0x5DEECE66DL , 0x0BL)

  lazy val get_next_long : Long => Long =
     seed =>
      generator .get_next_long (seed)

  lazy val get_next_seq : Long => Int => Seq [Long] =
     seed =>
       length =>
        generator .get_next_seq (seed) (length)

}

case class Random_ () extends Random

object Random {
  def mk : Random =
    Random_ ()
}


/*
 * This file is automatically generated. Do not edit.
 */

/**
 * This class contains tail recursive auxiliary functions.
 */

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

object FoldWhile {
  def mk : FoldWhile =
    FoldWhile_ ()
}

trait Fold
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold [A , B ] (sequence : Seq [A] ) (current : B) (next : B => A => B)
      : B =
    if ( sequence .isEmpty
    ) current
    else _tailrec_fold (sequence .tail) (next (current) (sequence .head) ) (next)

  def apply [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B) : B =
    _tailrec_fold (sequence) (initial) (next)

}

case class Fold_ () extends Fold

object Fold {
  def mk : Fold =
    Fold_ ()
}

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

object Range {
  def mk : Range =
    Range_ ()
}

trait Recursion
{

  private lazy val _fold_while = FoldWhile_ ()

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  def fold_while [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B)
      (condition : B => A => Boolean) : B =
    _fold_while .apply (sequence) (initial) (next) (condition)

  def fold [A , B ] (sequence : Seq [A] ) (initial : B) (next : B => A => B) : B =
    _fold .apply (sequence) (initial) (next)

  def range (length : Int) : Seq [Int] =
    _range .apply (length)

}

case class Recursion_ () extends Recursion

object Recursion {
  def mk : Recursion =
    Recursion_ ()
}


case class ScoringToolSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_xlist_1 : Seq [Double] = Seq (1 , 3 , 5 , 8)

  lazy val example_ylist_1 : Seq [Double] = Seq (1 , 3 , 5 , 8)

  lazy val instance_1 : Pearson = Pearson_ (example_xlist_1, example_ylist_1)

  private lazy val _mt : MathTool = MathTool_ ()

  private lazy val _mc : ScoringCategory = ScoringCategory_ ()

  test ("sum") (
    check (
      obtained = _mt .sum (example_xlist_1)
    ) (
      expected = 17
    )
  )

  test ("average") (
    check (
      obtained = _mt .average (example_xlist_1)
    ) (
      expected = 4.25
    )
  )

  test ("categorize") (
    check (
      obtained = _mc .categorize (0.2)
    ) (
      expected = _mc .weak_positive_correlation
    )
  )

  lazy val example_xlist_2 : Seq [Double] = Seq (43 , 21 , 25 , 42 , 57 , 59)

  lazy val example_ylist_2 : Seq [Double] = Seq (99 , 65 , 79 , 75 , 87 , 81)

  lazy val instance_2 : Pearson = Pearson_ (example_xlist_2, example_ylist_2)

  test ("coefficient") (
    check (
      obtained = (instance_2 .coefficient >= 0.529808) && (instance_2 .coefficient < 0.529809)
    ) (
      expected = true
    )
  )

}

