package se.umu.cs.rai.scopus.collection


import scala.annotation.tailrec


case class MSeqTranslator() {

  def asMSeq: Seq[Elem] => MSeq = s => Min().reverse(asMSeqRec(s, Min().empty))

  def asSeq: MSeq => Seq[Elem] = ms => asSeqRec(ms, Seq()).reverse

  @tailrec
  private def asMSeqRec(seq: Seq[Elem], ms: MSeq): MSeq =
    if (seq.isEmpty) {
      ms
    } else {
      asMSeqRec(seq.tail, Min().prepended(ms)(seq.head))
    }

  @tailrec
  private def asSeqRec(ms: MSeq, seq: Seq[Elem]): Seq[Elem] =
    if (Min().isEmpty(ms)) {
      seq
    } else {
      asSeqRec(Min().tail(ms), seq.prepended(Min().head(ms)))
    }

}

object MSeqTranslator {}


case class Min() {

  def empty: MSeq = ESeq()

  def prepended: MSeq => Elem => MSeq = s => e => NESeq(e, s)

  def head: MSeq => Elem = s => s.head

  def tail: MSeq => MSeq = s => s.tail

  def nonEmpty: MSeq => Boolean = s => !isEmpty(s)

  def isEmpty: MSeq => Boolean = s => s.isEmpty

  //

  def reverse: MSeq => MSeq = s => reverseRec(s, empty)

  def length: MSeq => Int = s => lengthRec(s, 0)

  def indexOf: MSeq => Elem => Int = s => e => indexOfRec(s, e, 0)

  def contains: MSeq => Elem => Boolean = s => e => containsRec(s, e)

  def at: MSeq => Int => Elem = s => n => atRec(s, n)

  //

  def take: MSeq => Int => MSeq = s => n => reverse(takeRevRec(s, n, empty))

  def drop: MSeq => Int => MSeq = s => n => dropRec(s, n)

  def takeWhile: MSeq => (Elem => Boolean) => MSeq = s => p => reverse(spanRevRec(s, p, taking = true, empty)._1)

  def dropWhile: MSeq => (Elem => Boolean) => MSeq = s => p => spanRevRec(s, p, taking = true, empty)._2

  def splitAt: MSeq => Int => (MSeq, MSeq) = s => n => (take(s)(n), drop(s)(n))

  def span: MSeq => (Elem => Boolean) => (MSeq, MSeq) = s => p => {
    val pair = spanRevRec(s, p, taking = true, empty)
    (reverse(pair._1), pair._2)
  }

  //

  def appended: MSeq => Elem => MSeq = s => e => reverse(prepended(reverse(s))(e))

  def last: MSeq => Elem = s => head(reverse(s))

  def concat: MSeq => MSeq => MSeq = s0 => s1 => reverseRec(reverse(s0), s1)

  def slice: MSeq => Int => Int => MSeq = s => from => until => take(drop(s)(from))(until - from)

  //

  def forall: MSeq => (Elem => Boolean) => Boolean = s => p => forallRec(s, p)

  def exists: MSeq => (Elem => Boolean) => Boolean = s => p => existsRec(s, p)

  def find: MSeq => (Elem => Boolean) => Option[Elem] = s => p => findRec(s, p)

  def filter: MSeq => (Elem => Boolean) => MSeq = s => p => reverse(filterRevRec(s, p, empty))

  def map0: MSeq => (Elem => Elem) => MSeq = s => f => reverse(mapRevRec(s, f, empty))

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
  def foldLeft0: MSeq => MSeq => ((MSeq, Elem) => MSeq) => MSeq = s0 => s1 => f => foldLeftRec(s0, f, s1)

  //

  @tailrec
  private def reverseRec(s0: MSeq, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      reverseRec(tail(s0), prepended(s1)(head(s0)))
    }
  }

  @tailrec
  private def lengthRec(s: MSeq, n: Int): Int = {
    if (isEmpty(s)) {
      n
    } else {
      lengthRec(tail(s), n + 1)
    }
  }

  @tailrec
  private def indexOfRec(s: MSeq, e: Elem, n: Int): Int = {
    if (isEmpty(s)) {
      -1
    } else if (head(s) == e) {
      n
    } else {
      indexOfRec(tail(s), e, n + 1)
    }
  }

  @tailrec
  private def containsRec(s: MSeq, e: Elem): Boolean = {
    if (isEmpty(s)) {
      false
    } else if (head(s) == e) {
      true
    } else {
      containsRec(tail(s), e)
    }
  }

  @tailrec
  private def atRec(s: MSeq, n: Int): Elem = {
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
  private def takeRevRec(s0: MSeq, n: Int, s1: MSeq): MSeq = {
    if (isEmpty(s0) || n <= 0) {
      s1
    } else {
      takeRevRec(tail(s0), n - 1, prepended(s1)(head(s0)))
    }
  }

  @tailrec
  private def dropRec(s: MSeq, n: Int): MSeq = {
    if (isEmpty(s) || n <= 0) {
      s
    } else {
      dropRec(tail(s), n - 1)
    }
  }

  @tailrec
  private def spanRevRec(s0: MSeq, p: Elem => Boolean, taking: Boolean, s1: MSeq): (MSeq, MSeq) = {
    if (isEmpty(s0) || !taking) {
      (s1, s0)
    } else {
      val newTaking = p(head(s0))
      val (newS1, newS0) = if (newTaking) {
        (prepended(s1)(head(s0)), tail(s0))
      } else {
        (s1, s0)
      }
      spanRevRec(newS0, p, newTaking, newS1)
    }
  }

  //

  @tailrec
  private def forallRec(s: MSeq, p: Elem => Boolean): Boolean = {
    if (isEmpty(s)) {
      true
    } else if (!p(head(s))) {
      false
    } else {
      forallRec(tail(s), p)
    }
  }

  @tailrec
  private def existsRec(s: MSeq, p: Elem => Boolean): Boolean = {
    if (isEmpty(s)) {
      false
    } else if (p(head(s))) {
      true
    } else {
      existsRec(tail(s), p)
    }
  }

  @tailrec
  private def findRec(s: MSeq, p: Elem => Boolean): Option[Elem] = {
    if (isEmpty(s)) {
      None
    } else if (p(head(s))) {
      Some(head(s))
    } else {
      findRec(tail(s), p)
    }
  }

  @tailrec
  private def filterRevRec(s0: MSeq, f: Elem => Boolean, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      val newS1 = if (f(head(s0))) {
        prepended(s1)(head(s0))
      } else {
        s1
      }
      filterRevRec(tail(s0), f, newS1)
    }
  }

  @tailrec
  private def mapRevRec(s0: MSeq, f: Elem => Elem, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      mapRevRec(tail(s0), f, prepended(s1)(f(head(s0))))
    }
  }

  @tailrec
  private def foldLeftRec(s0: MSeq, f: (MSeq, Elem) => MSeq, s1: MSeq): MSeq = {
    if (isEmpty(s0)) {
      s1
    } else {
      foldLeftRec(tail(s0), f, f(s1, head(s0)))
    }
  }

}

object Min {}

