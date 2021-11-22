package soda.example.forcoq.algorithms

/**
 * This class contains tail recursive auxiliary functions.
 */
trait RecursionForCoq {

  import scala.annotation.tailrec
        @tailrec  final
  def _rec_fold4 [A, B] (sequence: Seq [A]  ) (current_value: B ) (next_value_function: B => A => B ) (condition: B => A => Boolean ): B =
    sequence  match {
      case Nil => current_value
      case (head ):: (tail ) =>
         if ((! (condition (current_value ) (head ) ) )
         ) current_value
         else _rec_fold4 (tail ) (next_value_function (current_value ) (head )  ) (next_value_function ) (condition ) ;
    } ;

  def fold4 [A, B] (sequence: Seq [A]  ) (initial_value: B ) (next_value_function: B => A => B ) (condition: B => A => Boolean ): B =
    _rec_fold4 (sequence ) (initial_value ) (next_value_function ) (condition ) ;

  import scala.annotation.tailrec
        @tailrec  final
  def _rec_fold3 [A, B] (sequence: Seq [A]  ) (current_value: B ) (next_value_function: B => A => B ): B =
    sequence  match {
      case Nil => current_value
      case (head ):: (tail ) => _rec_fold3 (tail ) (next_value_function (current_value ) (head )  ) (next_value_function )
    } ;

  def fold3 [A, B] (sequence: Seq [A]  ) (initial_value: B ) (next_value_function: B => A => B ): B =
    _rec_fold3 (sequence ) (initial_value ) (next_value_function ) ;

  import scala.annotation.tailrec
        @tailrec  final
  def _rec_range (n: Int ) (sequence: Seq [Int]  ): Seq [Int] =
    if (n <= 0
    ) sequence
    else _rec_range (n - 1 ) (sequence .+: (n - 1 )  ) ;

  def range (length: Int ): Seq [Int] =
    _rec_range (length ) (Nil ) ;

}

case class RecursionForCoq_ ()  extends RecursionForCoq
