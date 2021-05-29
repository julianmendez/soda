package soda.translator.example


case class Status (r: BigInt, n: BigInt, q: BigInt, t: BigInt, l: BigInt, k: BigInt ) {

  override
  lazy val toString = " r=" + r + " n=" + n + " q=" + q + " t=" + t + " l=" + l + " k=" + k
}

case class PiIterator () {

  lazy val initial_status =
    Status (r = 0, n = 3, q = 1, t = 1, l = 3, k = 1 )

  def compute_new_status (s: Status ): Status =
    {
      lazy val result = rec (s )

      import scala.annotation.tailrec
        @tailrec
      def rec (s: Status ): Status =
        if ((4 * s.q + s.r - s.t ) < (s.n * s.t )
        ) s
        else
          {
            lazy val r = (2 * s.q + s.r ) * s.l
            lazy val n = (s.q * (7 * s.k ) + 2 + (s.r * s.l )  ) / (s.t * s.l )
            lazy val q = s.q * s.k
            lazy val t = s.t * s.l
            lazy val l = s.l + 2
            lazy val k = s.k + 1
            lazy val new_status = Status (r, n, q, t, l, k )
            rec (new_status ) }

      result }

  def take (n: Int ): Seq [BigInt] =
    {
      lazy val result = rec (n, Seq (), initial_status )

      import scala.annotation.tailrec
        @tailrec
      def rec (n: Int, rev_seq: Seq [BigInt], s: Status ): Seq [BigInt] =
        if (n == 0
        ) rev_seq.reverse
        else
          {
            lazy val t = _next (s )
            rec (n - 1, rev_seq.+: (t.digit ), t.new_status ) }

      result }

  def _next (s: Status ): BigIntAndStatus =
    {
      lazy val result = BigIntAndStatus (ret, new_status )
      lazy val ns = compute_new_status (s )
      lazy val ret = ns.n
      lazy val r = 10 * (ns.r - ns.n * ns.t )
      lazy val n = ((10 * (3 * ns.q + ns.r )  ) / ns.t ) - (10 * ns.n )
      lazy val q = ns.q * 10
      lazy val t = ns.t
      lazy val l = ns.l
      lazy val k = ns.k
      lazy val new_status = Status (r, n, q, t, l, k )
      result }

  case class BigIntAndStatus (digit: BigInt, new_status: Status )
}
