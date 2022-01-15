/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/**
 * This class contains tail recursive auxiliary functions.
 */
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
  def _tailrec_fold3 [A, B, C <: B] (sequence: Seq [A], current_value: C, next_value_function: (B, A ) => C ): C =
    if (sequence.isEmpty
    ) current_value
    else _tailrec_fold3 (sequence.tail, next_value_function (current_value, sequence.head ), next_value_function )

  def fold [A, B, C <: B] (sequence: Seq [A], initial_value: C, next_value_function: (B, A ) => C ): C =
    _tailrec_fold3 (sequence, initial_value, next_value_function )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_range (n: Int, sequence: Seq [Int]  ): Seq [Int] =
    if (n <= 0
    ) sequence
    else _tailrec_range (n - 1, sequence.+: (n - 1 )  )

  def range (length: Int ): Seq [Int] =
    _tailrec_range (length, Seq [Int] ()  )

}

case class Recursion_ ()  extends Recursion
