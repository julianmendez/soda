package scopus.lib

/**
 * This class contains tail recursive auxiliary functions.
 */
case class Rec() {

  def foldLeftWhile[A, B](s: Seq[A], initval: B, op: (B, A) => B, cond: (B, A) => Boolean): B = {
    lazy val result = rec(s, initval, op, cond)

    import scala.annotation.tailrec
        @tailrec
    def rec[A, B](seq: Seq[A], acc: B, op: (B, A) => B, cond: (B, A) => Boolean): B =
      if ( seq.isEmpty
      ) acc
      else {
        lazy val (elem, rest) = (seq.head, seq.tail)
        if ( ! cond(acc, elem)
        ) acc
        else rec(rest, op(acc, elem), op, cond)
      }

    result
  }


  def foldLeft[A, B](seq: Seq[A], initval: B, op: (B, A) => B): B = {
    lazy val result = rec(seq, initval, op)

    import scala.annotation.tailrec
        @tailrec
    def rec[A, B](seq: Seq[A], acc: B, op: (B, A) => B): B =
      if ( seq.isEmpty
      ) acc
      else rec(seq.tail, op(acc, seq.head), op)

    result
  }

}
