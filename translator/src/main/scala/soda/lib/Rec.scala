package soda.lib

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
      else {
        lazy val (elem, rest ) = (seq.head, seq.tail )
        if (! cond (acc, elem )
        ) acc
        else rec (rest, next_value (acc, elem ), next_value, cond )
      }

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
      else {
        lazy val next = n - 1
        rec (next, seq.+: (next )  )
      }

    result
  }

}
