/*
 * This file is automatically generated. Do not edit.
 */

package soda.lib

/*  ********  */

/**
 * This is a constant to be used in enumerations.
 */
trait EnumConstant {

  def ordinal: Int

  def name: String

  override
  lazy val toString: String = "" + ordinal + "-" + name
}

/*  ********  */

/**
 * This is an Option implemented without exceptions.
 */
trait OptionSD [T] {

  def isEmpty: Boolean

  def open [B]  (ifEmpty: B, ifNonEmpty: T => B ): B

  def map [B]  (mapping: T => B ): OptionSD [B]
}

case class NoneSD [T] () extends OptionSD [T] {

  lazy val isEmpty = true

  def open [B]  (ifEmpty: B, ifNonEmpty: T => B ): B = ifEmpty

  def map [B]  (mapping: T => B ): NoneSD [B] = NoneSD [B]  ()
}

case class SomeSD [T] (element: T ) extends OptionSD [T] {

  lazy val isEmpty = false

  lazy val get: T = element

  def open [B]  (ifEmpty: B, ifNonEmpty: T => B ): B = ifNonEmpty (element )

  def map [B]  (mapping: T => B ): SomeSD [B] = SomeSD [B]  (mapping (element )  )
}

/*  ********  */

/**
 * This class contains auxiliary functions for combinations.
 */
case class Comb () {

  def cartesianProduct [T]  (sequences: Seq [Seq [T]]  ): Seq [Seq [T]] = {
    lazy val result =
      if (sequences.isEmpty
      ) sequences
      else {
        lazy val rev_sequences = sequences.reverse
        Rec () .foldLeft (rev_sequences.tail, initial_value (rev_sequences.head ), next_value )
      }

    def initial_value (seq: Seq [T]  ): Seq [Seq [T]] = seq.map (elem => Seq (elem )  )

    def next_value (accum: Seq [Seq [T]], seq_a: Seq [T]  ): Seq [Seq [T]] =
      seq_a.flatMap (elem_a =>
        accum.map (seq_b => seq_b.+: (elem_a ) ) )

    result
  }

}

/*  ********  */

/**
 * This class contains tail recursive auxiliary functions.
 */
case class Rec () {


  def foldLeftWhile [A, B, C <: B]  (s: Seq [A], initial_value: C, next_value: (B, A ) => C, cond: (B, A ) => Boolean ): C = {

    lazy val result = rec (s, initial_value, next_value, cond )

    import scala.annotation.tailrec
        @tailrec
    def rec (seq: Seq [A], acc: C, next_value: (B, A ) => C, cond: (B, A ) => Boolean ): C =
      if (seq.isEmpty
      ) acc
      else
        if (! cond (acc, seq.head )
        ) acc
        else rec (seq.tail, next_value (acc, seq.head ), next_value, cond )

    result
  }


  def foldLeft [A, B, C <: B]  (seq: Seq [A], initial_value: C, next_value: (B, A ) => C ): C = {

    lazy val result = rec (seq, initial_value, next_value )

    import scala.annotation.tailrec
        @tailrec
    def rec (seq: Seq [A], acc: C, next_value: (B, A ) => C ): C =
      if (seq.isEmpty
      ) acc
      else rec (seq.tail, next_value (acc, seq.head ), next_value )

    result
  }


  def range (n: Int ): Seq [Int] = {

    lazy val result = rec (n, Seq [Int]  ()  )

    import scala.annotation.tailrec
        @tailrec
    def rec (n: Int, seq: Seq [Int]  ): Seq [Int] =
      if (n <= 0
      ) seq
      else rec (n - 1, seq.+: (n - 1 )  )

    result
  }

}
