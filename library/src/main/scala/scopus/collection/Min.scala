package scopus.collection


import scala.annotation.tailrec


case class MSeqTranslator[T]() {

  def foldLeftSeq[B](seq: Seq[T], initval: B, op: (B, T) => B): B = {
    lazy val result = rec(seq, initval, op)

    import scala.annotation.tailrec
        @tailrec
    def rec[B](seq: Seq[T], acc: B, op: (B, T) => B): B =
      if ( seq.isEmpty
      ) acc
      else rec(seq.tail, op(acc, seq.head), op)

    result
  }

  def asMSeq(seq: Seq[T]): MSeq[T] = {
    lazy val result = Min().reverse(
      foldLeftSeq[MSeq[T]](seq, initval, op) )

    lazy val initval: MSeq[T] = Min().empty

    def op(acc: MSeq[T], elem: T): MSeq[T] = Min().prepended(acc, elem)

    result
  }

  def foldLeftMSeq[B](mseq: MSeq[T], initval: B, op: (B, T) => B): B = {
    lazy val result = rec(mseq, initval, op)

    import scala.annotation.tailrec
        @tailrec
    def rec[B](mseq: MSeq[T], acc: B, op: (B, T) => B): B =
      if ( mseq.isEmpty
      ) acc
      else rec(mseq.tail(), op(acc, mseq.head()), op)

    result
  }

  def asSeq(mseq: MSeq[T]): Seq[T] = {
    lazy val result = foldLeftMSeq(mseq, initval, op).reverse

    lazy val initval: Seq[T] = Seq()

    def op(acc: Seq[T], elem: T): Seq[T] = acc.+:(elem)

    result
  }

}


case class Min[T]() {

  lazy val empty: MSeq[T] = ESeq()

  def prepended(s: MSeq[T], e: T): MSeq[T] = NESeq(e, s)

  def head(s: MSeq[T]): T = s.head()

  def tail(s: MSeq[T]): MSeq[T] = s.tail()

  def nonEmpty(s: MSeq[T]): Boolean = ! isEmpty(s)

  def isEmpty(s: MSeq[T]): Boolean = s.isEmpty

  //

  def reverse(s: MSeq[T]): MSeq[T] = reverseRec(s, empty)

  import scala.annotation.tailrec
        @tailrec
  private
  def reverseRec(s0: MSeq[T], s1: MSeq[T]): MSeq[T] =
    if ( isEmpty(s0)
    ) s1
    else reverseRec(tail(s0), prepended(s1, head(s0)))


  def length(s: MSeq[T]): Int = {
    lazy val result = rec(s, 0)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], n: Int): Int =
      if ( isEmpty(s)
      ) n
      else rec(tail(s), n + 1)

    result
  }

  def indexOf(s: MSeq[T], e: T): Int = {
    lazy val result = rec(s, e, 0)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], e: T, n: Int): Int =
      if ( isEmpty(s)
      ) -1
      else if ( head(s) == e
      ) n
      else rec(tail(s), e, n + 1)

    result
  }

  def contains(s: MSeq[T], e: T): Boolean = {
    lazy val result = rec(s, e)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], e: T): Boolean =
      if ( isEmpty(s)
      ) false
      else if ( head(s) == e
      ) true
      else rec(tail(s), e)

    result
  }

  def at(s: MSeq[T], n: Int): T = {
    lazy val result = rec(s, n)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], n: Int): T =
      if ( isEmpty(s) || n < 0
      ) None.get
      else if ( n == 0
      ) head(s)
      else rec(tail(s), n - 1)

    result
  }

  //

  def take(s: MSeq[T], n: Int): MSeq[T] = {
    lazy val result = reverse(rec(s, n, empty))

    import scala.annotation.tailrec
        @tailrec
    def rec(s0: MSeq[T], n: Int, s1: MSeq[T]): MSeq[T] =
      if ( isEmpty(s0) || n <= 0
      ) s1
      else rec(tail(s0), n - 1, prepended(s1, head(s0)))

    result
  }

  def drop(s: MSeq[T], n: Int): MSeq[T] = {
    lazy val result = rec(s, n)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], n: Int): MSeq[T] =
      if ( isEmpty(s) || n <= 0
      ) s
      else rec(tail(s), n - 1)

    result
  }

  def takeWhile(s: MSeq[T], p: (T => Boolean)): MSeq[T] = reverse(spanRevRec(s, p, taking = true, empty)._1)

  def dropWhile(s: MSeq[T], p: (T => Boolean)): MSeq[T] = spanRevRec(s, p, taking = true, empty)._2

  def splitAt(s: MSeq[T], n: Int): (MSeq[T], MSeq[T]) = (take(s, n), drop(s, n))

  def span(s: MSeq[T], p: (T => Boolean)): (MSeq[T], MSeq[T]) = {
    lazy val pair = spanRevRec(s, p, taking = true, empty)
    (reverse(pair._1), pair._2)
  }

  //

  def appended(s: MSeq[T], e: T): MSeq[T] = reverse(prepended(reverse(s), e))

  def last(s: MSeq[T]): T = head(reverse(s))

  def concat(s0: MSeq[T], s1: MSeq[T]): MSeq[T] = reverseRec(reverse(s0), s1)

  def slice(s: MSeq[T], from: Int, until: Int): MSeq[T] = take(drop(s, from), until - from)

  //

  def forall(s: MSeq[T], p: (T => Boolean)): Boolean = {
    lazy val result = rec(s, p)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], p: T => Boolean): Boolean =
      if ( isEmpty(s)
      ) true
      else if ( ! p(head(s))
      ) false
      else rec(tail(s), p)

    result
  }

  def exists(s: MSeq[T], p: (T => Boolean)): Boolean = {
    lazy val result = rec(s, p)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], p: T => Boolean): Boolean =
      if ( isEmpty(s)
      ) false
      else if ( p(head(s))
      ) true
      else rec(tail(s), p)

    result
  }

  def find(s: MSeq[T], p: (T => Boolean)): Option[T] = {
    lazy val result = rec(s, p)

    import scala.annotation.tailrec
        @tailrec
    def rec(s: MSeq[T], p: T => Boolean): Option[T] =
      if ( isEmpty(s)
      ) None
      else if ( p(head(s))
      ) Some(head(s))
      else rec(tail(s), p)

    result
  }

  def filter(s: MSeq[T], p: (T => Boolean)): MSeq[T] = {
    lazy val result = reverse(rec(s, p, empty))

    import scala.annotation.tailrec
        @tailrec
    def rec(s0: MSeq[T], f: T => Boolean, s1: MSeq[T]): MSeq[T] =
      if ( isEmpty(s0)
      ) s1
      else {
        lazy val newS1 =
          if ( f(head(s0))
          ) prepended(s1, head(s0))
          else s1
        rec(tail(s0), f, newS1)
      }

    result
  }

  def map0(s: MSeq[T], f: (T => T)): MSeq[T] = {
    lazy val result = reverse(rec(s, f, empty))

    import scala.annotation.tailrec
        @tailrec
    def rec(s0: MSeq[T], f: T => T, s1: MSeq[T]): MSeq[T] =
      if ( isEmpty(s0)
      ) s1
      else rec(tail(s0), f, prepended(s1, f(head(s0))))

    result
  }

  /**
   * <pre>
   * def foldLeft[B](z: B)(op: (B, A) -> B): B = {
   * . var result = z
   * . it = iterator
   * . while (it.hasNext) {
   * . . result = op(result, it.next())
   * . }
   * . result
   * }
   * </pre>
   */
  def foldLeft0(mseq: MSeq[T]): (MSeq[T], ((MSeq[T], T) => MSeq[T])) => MSeq[T] = {
    lazy val result = (acc: MSeq[T], op: ((MSeq[T], T) => MSeq[T])) => rec(mseq, acc, op)

    import scala.annotation.tailrec
        @tailrec
    def rec(mseq: MSeq[T], acc: MSeq[T], op: (MSeq[T], T) => MSeq[T]): MSeq[T] =
      if ( isEmpty(mseq)
      ) acc
      else rec(tail(mseq), op(acc, head(mseq)), op)

    result
   }

  //

  import scala.annotation.tailrec
        @tailrec
  private
  def spanRevRec(s0: MSeq[T], p: T => Boolean, taking: Boolean, s1: MSeq[T]): (MSeq[T], MSeq[T]) =
    if ( isEmpty(s0) || ! taking
    ) (s1, s0)
    else {
      lazy val newTaking = p(head(s0))
      lazy val (newS1, newS0) =
        if ( newTaking
        ) (prepended(s1, head(s0)), tail(s0))
        else (s1, s0)
      spanRevRec(newS0, p, newTaking, newS1)
    }

}
