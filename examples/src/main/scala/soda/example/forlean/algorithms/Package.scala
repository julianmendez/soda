package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Coq.
 */

trait Package

/**
 * This class contains tail recursive auxiliary functions.
 */

trait RecursionForLean
{

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold4 [A , B ] (sequence : Seq [A] ) (current : B)
      (next_value : B => A => B) (condition : B => A => Boolean) : B =
    sequence match  {
      case (head) :: (tail) =>
         if ( (! (condition (current) (head) ) )
         ) current
         else _tailrec_fold4 (tail) (next_value (current) (head) ) (next_value) (condition)
      case otherwise => current
    }

  def fold4 [A , B ] (sequence : Seq [A] ) (initial_value : B)
      (next_value : B => A => B) (condition : B => A => Boolean) : B =
    _tailrec_fold4 (sequence) (initial_value) (next_value) (condition)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fold3 [A , B ] (sequence : Seq [A] ) (current : B)
      (next_value : B => A => B) : B =
    sequence match  {
      case (head) :: (tail) =>
        _tailrec_fold3 (tail) (next_value (current) (head) ) (next_value)
      case otherwise => current
    }

  def fold3 [A , B ] (sequence : Seq [A] ) (initial_value : B)
      (next_value : B => A => B) : B =
    _tailrec_fold3 (sequence) (initial_value) (next_value)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_range (n : Int) (sequence : Seq [Int] ) : Seq [Int] =
    if ( n <= 0
    ) sequence
    else _tailrec_range (n - 1) (sequence .+: (n - 1) )

  def range (length : Int) : Seq [Int] =
    _tailrec_range (length) (Nil)

}

case class RecursionForLean_ () extends RecursionForLean

