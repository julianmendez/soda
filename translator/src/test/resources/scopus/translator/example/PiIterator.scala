package scopus.translator.example

import scala.annotation.tailrec

case class Status(
  r: BigInt,
  n: BigInt,
  q: BigInt,
  t: BigInt,
  l: BigInt,
  k: BigInt
) {

  override
  val toString = " r=" + r + " n=" + n + " q=" + q + " t=" + t + " l=" + l + " k=" + k

}


case class PiIterator() {

  val initial_status = Status (r=0, n=3, q=1, t=1, l=3, k=1)

  @tailrec final
  def compute_new_status (s: Status): Status =
    if ( (4 * s.q + s.r - s.t) < (s.n * s.t)
    ) s
    else {
      val r = (2 * s.q + s.r) * s.l
      val n = (s.q * (7 * s.k) + 2 + (s.r * s.l)) / (s.t * s.l)
      val q = s.q * s.k
      val t = s.t * s.l
      val l = s.l + 2
      val k = s.k + 1
      val new_status = Status (r, n, q, t, l, k)
      compute_new_status (new_status)
    }

  def next (s: Status): (BigInt, Status) = {
    val ns = compute_new_status (s)
    val ret = ns.n
    val r = 10 * (ns.r - ns.n * ns.t)
    val n = ((10 * (3 * ns.q + ns.r)) / ns.t) - (10 * ns.n)
    val q = ns.q * 10
    val t = ns.t
    val l = ns.l
    val k = ns.k
    val new_status = Status (r, n, q, t, l, k)
    (ret, new_status)
  }

  def take (n: Int): Seq[BigInt] =
    take_rec (n, Seq(), initial_status)

  @tailrec final
  def take_rec (n: Int, rev_seq: Seq[BigInt], s: Status): Seq[BigInt] =
    if ( n == 0
    ) rev_seq.reverse
    else {
      val (digit, new_status) = next (s)
      take_rec (n - 1, rev_seq.+: (digit), new_status)
    }

}
