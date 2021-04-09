package soda.lib

/**
 * This class contains tail recursive auxiliary functions.
 */
case class Rec () {

  def foldLeftWhile [A, B]  (s: Seq [A], initial_value: B, next_value: (B, A ) => B, cond: (B, A ) => Boolean ): B = {
    lazy val result = rec (s, initial_value, next_value, cond )

    import scala.annotation.tailrec
        @tailrec
    def rec [A, B]  (seq: Seq [A], acc: B, next_value: (B, A ) => B, cond: (B, A ) => Boolean ): B =
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


  def foldLeft [A, B]  (seq: Seq [A], initial_value: B, next_value: (B, A ) => B ): B = {
    lazy val result = rec (seq, initial_value, next_value )

    import scala.annotation.tailrec
        @tailrec
    def rec [A, B]  (seq: Seq [A], acc: B, next_value: (B, A ) => B ): B =
      if (seq.isEmpty
      ) acc
      else rec (seq.tail, next_value (acc, seq.head ), next_value )

    result
  }

}
