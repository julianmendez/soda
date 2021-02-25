package scopus.se.umu.cs.rai.scopus.collection


import scala.annotation.tailrec


case class MSeqTranslator[T]() {

  def asMSeq(s: Seq[T]): MSeq[T] = Min().reverse(asMSeqRec(s, Min().empty))

  def asSeq(ms: MSeq[T]): Seq[T] = asSeqRec(ms, Seq()).reverse

  @tailrec final
  def asMSeqRec(seq: Seq[T], ms: MSeq[T]): MSeq[T] =
    if ( seq.isEmpty
    ) ms
    else asMSeqRec(seq.tail, Min().prepended(ms, seq.head))

  @tailrec final
  def asSeqRec(ms: MSeq[T], seq: Seq[T]): Seq[T] =
    if ( Min().isEmpty(ms)
    ) seq
    else asSeqRec(Min().tail(ms), seq.+:(Min().head(ms)))

}


case class Min[T]() {

  val empty: MSeq[T] = ESeq()

  def prepended(s: MSeq[T], e: T): MSeq[T] = NESeq(e, s)

  def head(s: MSeq[T]): T = s.head()

  def tail(s: MSeq[T]): MSeq[T] = s.tail()

  def nonEmpty(s: MSeq[T]): Boolean = ! isEmpty(s)

  def isEmpty(s: MSeq[T]): Boolean = s.isEmpty

  //

  def reverse(s: MSeq[T]): MSeq[T] = reverseRec(s, empty)

  def length(s: MSeq[T]): Int = lengthRec(s, 0)

  def indexOf(s: MSeq[T], e: T): Int = indexOfRec(s, e, 0)

  def contains(s: MSeq[T], e: T): Boolean = containsRec(s, e)

  def at(s: MSeq[T], n: Int): T = atRec(s, n)

  //

  def take(s: MSeq[T], n: Int): MSeq[T] = reverse(takeRevRec(s, n, empty))

  def drop(s: MSeq[T], n: Int): MSeq[T] = dropRec(s, n)

  def takeWhile(s: MSeq[T], p: (T => Boolean)): MSeq[T] = reverse(spanRevRec(s, p, taking = true, empty)._1)

  def dropWhile(s: MSeq[T], p: (T => Boolean)): MSeq[T] = spanRevRec(s, p, taking = true, empty)._2

  def splitAt(s: MSeq[T], n: Int): (MSeq[T], MSeq[T]) = (take(s, n), drop(s, n))

  def span(s: MSeq[T], p: (T => Boolean)): (MSeq[T], MSeq[T]) = {
    val pair = spanRevRec(s, p, taking = true, empty)
    (reverse(pair._1), pair._2)
  }

  //

  def appended(s: MSeq[T], e: T): MSeq[T] = reverse(prepended(reverse(s), e))

  def last(s: MSeq[T]): T = head(reverse(s))

  def concat(s0: MSeq[T], s1: MSeq[T]): MSeq[T] = reverseRec(reverse(s0), s1)

  def slice(s: MSeq[T], from: Int, until: Int): MSeq[T] = take(drop(s, from), until - from)

  //

  def forall(s: MSeq[T], p: (T => Boolean)): Boolean = forallRec(s, p)

  def exists(s: MSeq[T], p: (T => Boolean)): Boolean = existsRec(s, p)

  def find(s: MSeq[T], p: (T => Boolean)): Option[T] = findRec(s, p)

  def filter(s: MSeq[T], p: (T => Boolean)): MSeq[T] = reverse(filterRevRec(s, p, empty))

  def map0(s: MSeq[T], f: (T => T)): MSeq[T] = reverse(mapRevRec(s, f, empty))

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
  def foldLeft0(s0: MSeq[T]): (MSeq[T], ((MSeq[T], T) => MSeq[T])) => MSeq[T] =
    (s1: MSeq[T], f: ((MSeq[T], T) => MSeq[T])) =>
      foldLeftRec(s0, f, s1)

  //

  @tailrec final
  def reverseRec(s0: MSeq[T], s1: MSeq[T]): MSeq[T] =
    if ( isEmpty(s0)
    ) s1
    else reverseRec(tail(s0), prepended(s1, head(s0)))

  @tailrec final
  def lengthRec(s: MSeq[T], n: Int): Int =
    if ( isEmpty(s)
    ) n
    else lengthRec(tail(s), n + 1)

  @tailrec final
  def indexOfRec(s: MSeq[T], e: T, n: Int): Int =
    if ( isEmpty(s)
    ) -1
    else if ( head(s) == e
    ) n
    else indexOfRec(tail(s), e, n + 1)

  @tailrec final
  def containsRec(s: MSeq[T], e: T): Boolean =
    if ( isEmpty(s)
    ) false
    else if ( head(s) == e
    ) true
    else containsRec(tail(s), e)

  @tailrec final
  def atRec(s: MSeq[T], n: Int): T =
    if ( isEmpty(s) || n < 0
    ) None.get
    else if ( n == 0
    ) head(s)
    else atRec(tail(s), n - 1)

  //

  @tailrec final
  def takeRevRec(s0: MSeq[T], n: Int, s1: MSeq[T]): MSeq[T] =
    if ( isEmpty(s0) || n <= 0
    ) s1
    else takeRevRec(tail(s0), n - 1, prepended(s1, head(s0)))

  @tailrec final
  def dropRec(s: MSeq[T], n: Int): MSeq[T] =
    if ( isEmpty(s) || n <= 0
    ) s
    else dropRec(tail(s), n - 1)

  @tailrec final
  def spanRevRec(s0: MSeq[T], p: T => Boolean, taking: Boolean, s1: MSeq[T]): (MSeq[T], MSeq[T]) =
    if ( isEmpty(s0) || ! taking
    ) (s1, s0)
    else {
      val newTaking = p(head(s0))
      val (newS1, newS0) =
        if ( newTaking
        ) (prepended(s1, head(s0)), tail(s0))
        else (s1, s0)
      spanRevRec(newS0, p, newTaking, newS1)
    }

  //

  @tailrec final
  def forallRec(s: MSeq[T], p: T => Boolean): Boolean =
    if ( isEmpty(s)
    ) true
    else if ( ! p(head(s))
    ) false
    else forallRec(tail(s), p)

  @tailrec final
  def existsRec(s: MSeq[T], p: T => Boolean): Boolean =
    if ( isEmpty(s)
    ) false
    else if ( p(head(s))
    ) true
    else existsRec(tail(s), p)

  @tailrec final
  def findRec(s: MSeq[T], p: T => Boolean): Option[T] =
    if ( isEmpty(s)
    ) None
    else if ( p(head(s))
    ) Some(head(s))
    else findRec(tail(s), p)

  @tailrec final
  def filterRevRec(s0: MSeq[T], f: T => Boolean, s1: MSeq[T]): MSeq[T] =
    if ( isEmpty(s0)
    ) s1
    else {
      val newS1 =
        if ( f(head(s0))
        ) prepended(s1, head(s0))
        else s1
      filterRevRec(tail(s0), f, newS1)
    }

  @tailrec final
  def mapRevRec(s0: MSeq[T], f: T => T, s1: MSeq[T]): MSeq[T] =
    if ( isEmpty(s0)
    ) s1
    else mapRevRec(tail(s0), f, prepended(s1, f(head(s0))))

  @tailrec final
  def foldLeftRec(s0: MSeq[T], f: (MSeq[T], T) => MSeq[T], s1: MSeq[T]): MSeq[T] =
    if ( isEmpty(s0)
    ) s1
    else foldLeftRec(tail(s0), f, f(s1, head(s0)))

}
