package se.umu.cs.rai.scopus.collection


import scala.annotation.tailrec


case class MSeqTranslator() {

  def asMSeq(s: Seq[Elem]): MSeq = Min().reverse(asMSeqRec(s, Min().empty))

  def asSeq(ms: MSeq): Seq[Elem] = asSeqRec(ms, Seq()).reverse

  @tailrec
  final def asMSeqRec(seq: Seq[Elem], ms: MSeq): MSeq =
    if (seq.isEmpty) {
      ms
    } else {
      asMSeqRec(seq.tail, Min().prepended(ms, seq.head))
    }

  @tailrec
  final def asSeqRec(ms: MSeq, seq: Seq[Elem]): Seq[Elem] =
    if (Min().isEmpty(ms)) {
      seq
    } else {
      asSeqRec(Min().tail(ms), seq.prepended(Min().head(ms)))
    }

}

object MSeqTranslator {}


case class Min() {

  def empty: MSeq = ESeq()

  def prepended(s: MSeq, e: Elem): MSeq = NESeq(e, s)

  def head(s: MSeq): Elem = s.head

  def tail(s: MSeq): MSeq = s.tail

  def nonEmpty(s: MSeq): Boolean = !isEmpty(s)

  def isEmpty(s: MSeq): Boolean = s.isEmpty

  //

  def reverse(s: MSeq): MSeq = reverseRec(s, empty)

  def length(s: MSeq): Int = lengthRec(s, 0)

  def indexOf(s: MSeq, e: Elem): Int = indexOfRec(s, e, 0)

  def contains(s: MSeq, e: Elem): Boolean = containsRec(s, e)

  def at(s: MSeq, n: Int): Elem = atRec(s, n)

  //

  def take(s: MSeq, n: Int): MSeq = reverse(takeRevRec(s, n, empty))

  def drop(s: MSeq, n: Int): MSeq = dropRec(s, n)

  def takeWhile(s: MSeq, p: (Elem => Boolean)): MSeq = reverse(spanRevRec(s, p, taking = true, empty)._1)

  def dropWhile(s: MSeq, p: (Elem => Boolean)): MSeq = spanRevRec(s, p, taking = true, empty)._2

  def splitAt(s: MSeq, n: Int): (MSeq, MSeq) = (take(s, n), drop(s, n))

  def span(s: MSeq, p: (Elem => Boolean)): (MSeq, MSeq) = {
    val pair = spanRevRec(s, p, taking = true, empty)
    (reverse(pair._1), pair._2)
  }

  //

  def appended(s: MSeq, e: Elem): MSeq = reverse(prepended(reverse(s), e))

  def last(s: MSeq): Elem = head(reverse(s))

  def concat(s0: MSeq, s1: MSeq): MSeq = reverseRec(reverse(s0), s1)

  def slice(s: MSeq, from: Int, until: Int): MSeq = take(drop(s, from), until - from)

  //

  def forall(s: MSeq, p: (Elem => Boolean)): Boolean = forallRec(s, p)

  def exists(s: MSeq, p: (Elem => Boolean)): Boolean = existsRec(s, p)

  def find(s: MSeq, p: (Elem => Boolean)): Option[Elem] = findRec(s, p)

  def filter(s: MSeq, p: (Elem => Boolean)): MSeq = reverse(filterRevRec(s, p, empty))

  def map0(s: MSeq, f: (Elem => Elem)): MSeq = reverse(mapRevRec(s, f, empty))

  /**
   * <pre>
   * def foldLeft[B](z: B)(op: (B, A) => B): B = {
   * . var result = z
   * . val it = iterator
   * . while (it.hasNext) {
   * . . result = op(result, it.next())
   * . }
   * . result
   * }
   * </pre>
   */
  def foldLeft0(s0: MSeq, s1: MSeq, f: ((MSeq, Elem) => MSeq)): MSeq = foldLeftRec(s0, f, s1)

  //

  @tailrec
  final def reverseRec(s0: MSeq, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      reverseRec(tail(s0), prepended(s1, head(s0)))
    }
  }

  @tailrec
  final def lengthRec(s: MSeq, n: Int): Int = {
    if (isEmpty(s)) {
      n
    } else {
      lengthRec(tail(s), n + 1)
    }
  }

  @tailrec
  final def indexOfRec(s: MSeq, e: Elem, n: Int): Int = {
    if (isEmpty(s)) {
      -1
    } else if (head(s) == e) {
      n
    } else {
      indexOfRec(tail(s), e, n + 1)
    }
  }

  @tailrec
  final def containsRec(s: MSeq, e: Elem): Boolean = {
    if (isEmpty(s)) {
      false
    } else if (head(s) == e) {
      true
    } else {
      containsRec(tail(s), e)
    }
  }

  @tailrec
  final def atRec(s: MSeq, n: Int): Elem = {
    if (isEmpty(s) || n < 0) {
      throw new IndexOutOfBoundsException()
    } else if (n == 0) {
      head(s)
    } else {
      atRec(tail(s), n - 1)
    }
  }

  //

  @tailrec
  final def takeRevRec(s0: MSeq, n: Int, s1: MSeq): MSeq = {
    if (isEmpty(s0) || n <= 0) {
      s1
    } else {
      takeRevRec(tail(s0), n - 1, prepended(s1, head(s0)))
    }
  }

  @tailrec
  final def dropRec(s: MSeq, n: Int): MSeq = {
    if (isEmpty(s) || n <= 0) {
      s
    } else {
      dropRec(tail(s), n - 1)
    }
  }

  @tailrec
  final def spanRevRec(s0: MSeq, p: Elem => Boolean, taking: Boolean, s1: MSeq): (MSeq, MSeq) = {
    if (isEmpty(s0) || !taking) {
      (s1, s0)
    } else {
      val newTaking = p(head(s0))
      val (newS1, newS0) = if (newTaking) {
        (prepended(s1, head(s0)), tail(s0))
      } else {
        (s1, s0)
      }
      spanRevRec(newS0, p, newTaking, newS1)
    }
  }

  //

  @tailrec
  final def forallRec(s: MSeq, p: Elem => Boolean): Boolean = {
    if (isEmpty(s)) {
      true
    } else if (!p(head(s))) {
      false
    } else {
      forallRec(tail(s), p)
    }
  }

  @tailrec
  final def existsRec(s: MSeq, p: Elem => Boolean): Boolean = {
    if (isEmpty(s)) {
      false
    } else if (p(head(s))) {
      true
    } else {
      existsRec(tail(s), p)
    }
  }

  @tailrec
  final def findRec(s: MSeq, p: Elem => Boolean): Option[Elem] = {
    if (isEmpty(s)) {
      None
    } else if (p(head(s))) {
      Some(head(s))
    } else {
      findRec(tail(s), p)
    }
  }

  @tailrec
  final def filterRevRec(s0: MSeq, f: Elem => Boolean, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      val newS1 = if (f(head(s0))) {
        prepended(s1, head(s0))
      } else {
        s1
      }
      filterRevRec(tail(s0), f, newS1)
    }
  }

  @tailrec
  final def mapRevRec(s0: MSeq, f: Elem => Elem, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      mapRevRec(tail(s0), f, prepended(s1, f(head(s0))))
    }
  }

  @tailrec
  final def foldLeftRec(s0: MSeq, f: (MSeq, Elem) => MSeq, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      foldLeftRec(tail(s0), f, f(s1, head(s0)))
    }
  }

}

object Min {}

