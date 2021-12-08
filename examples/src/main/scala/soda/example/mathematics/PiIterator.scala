package soda.example.mathematics

trait Status {

  def r: BigInt

  def n: Int

  def q: BigInt

  def t: BigInt

  def l: Int

  def k: Int

  override
  lazy val toString = " r=" + r + " n=" + n + " q=" + q + " t=" + t + " l=" + l + " k=" + k

}

case class Status_ (r: BigInt, n: Int, q: BigInt, t: BigInt, l: Int, k: Int  ) extends Status

trait PiIterator {

  lazy val initial_status =
    Status_ (r = 0, n = 3, q = 1, t = 1, l = 3, k = 1 )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_compute_new_status (s: Status ): Status =
    if ((4 * s.q + s.r - s.t ) < (s.n * s.t )
    ) s
    else
      {
        lazy val r = (2 * s.q + s.r ) * s.l
        lazy val n = ((s.q * (7 * s.k ) + 2 + (s.r * s.l )  ) / (s.t * s.l )  ) .toInt
        lazy val q = s.q * s.k
        lazy val t = s.t * s.l
        lazy val l = s.l + 2
        lazy val k = s.k + 1
        lazy val new_status = Status_ (r, n, q, t, l, k )
        _tailrec_compute_new_status (new_status ) }

  def compute_new_status (s: Status ): Status =
    _tailrec_compute_new_status (s )

  import scala.annotation.tailrec
        @tailrec  final
  def _tailrec_take (n: Int, rev_seq: Seq [Int], s: Status ): Seq [Int] =
    if (n == 0
    ) rev_seq.reverse
    else
      {
        lazy val t = _get_next (s )
        _tailrec_take (n - 1, rev_seq.+: (t.digit ), t.new_status ) }

  def take (n: Int ): Seq [Int] =
    _tailrec_take (n, Seq (), initial_status )

  def _get_next (s: Status ): IntAndStatus =
    {
      lazy val result = IntAndStatus_ (ret, new_status )
      lazy val ns = compute_new_status (s )
      lazy val ret = ns.n
      lazy val r = 10 * (ns.r - ns.n * ns.t )
      lazy val n = (((10 * (3 * ns.q + ns.r )  ) / ns.t ) - (10 * ns.n )  ) .toInt
      lazy val q = ns.q * 10
      lazy val t = ns.t
      lazy val l = ns.l
      lazy val k = ns.k
      lazy val new_status = Status_ (r, n, q, t, l, k )
      result }

}

case class PiIterator_ () extends PiIterator

trait IntAndStatus {

  def digit: Int

  def new_status: Status

}

case class IntAndStatus_ (digit: Int, new_status: Status ) extends IntAndStatus
