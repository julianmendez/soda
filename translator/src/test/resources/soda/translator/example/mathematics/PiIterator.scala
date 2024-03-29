trait Status
{

  def   r : BigInt
  def   n : Int
  def   q : BigInt
  def   t : BigInt
  def   l : Int
  def   k : Int

  override
  lazy val toString = " r=" + r + " n=" + n + " q=" + q + " t=" + t + " l=" + l + " k=" + k

}

case class Status_ (r : BigInt, n : Int, q : BigInt, t : BigInt, l : Int, k : Int) extends Status

object Status {
  def mk (r : BigInt) (n : Int) (q : BigInt) (t : BigInt) (l : Int) (k : Int) : Status =
    Status_ (r, n, q, t, l, k)
}

trait IntAndStatus
{

  def   digit : Int
  def   new_status : Status

}

case class IntAndStatus_ (digit : Int, new_status : Status) extends IntAndStatus

object IntAndStatus {
  def mk (digit : Int) (new_status : Status) : IntAndStatus =
    IntAndStatus_ (digit, new_status)
}

trait PiIterator
{



  private def _mk_Status (r : BigInt) (n : Int) (q : BigInt) (t : BigInt) (l : Int) (k : Int) : Status =
    Status_ (r, n, q, t, l, k)

  private def _mk_IntAndStatus (digit : Int) (new_status : Status) : IntAndStatus =
    IntAndStatus_ (digit, new_status)

  private lazy val _initial_status =
    _mk_Status (r = 0) (n = 3) (q = 1) (t = 1) (l = 3) (k = 1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_compute_new_status (s : Status) : Status =
    if ( (4 * s .q + s .r - s .t) < (s .n * s .t)
    ) s
    else
      _tailrec_compute_new_status (
        _mk_Status (
          r = (2 * s .q + s .r) * s .l) (
          n = ( (s .q * (7 * s .k) + 2 + (s .r * s .l) ) / (s .t * s .l) ) .toInt) (
          q = s .q * s .k) (
          t = s .t * s .l) (
          l = s .l + 2) (
          k = s .k + 1
        )
      )

  private def _compute_new_status (s : Status) : Status =
    _tailrec_compute_new_status (s)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_take (n : Int) (rev_seq : Seq [Int] ) (s : Status) (t : IntAndStatus) : Seq [Int] =
    if ( n == 0
    ) rev_seq .reverse
    else _tailrec_take (n - 1) (rev_seq .+: (t .digit) ) (t .new_status) (
      _get_next (t .new_status) )

  private def _get_next_with_new_status (s : Status) : IntAndStatus =
    _mk_IntAndStatus (s .n) (
      _mk_Status (
        r = 10 * (s .r - s .n * s .t)) (
        n = ( ( (10 * (3 * s .q + s .r) ) / s .t) - (10 * s .n) ) .toInt) (
        q = s .q * 10) (
        t = s .t) (
        l = s .l) (
        k = s .k
      )
    )

  private def _get_next (s : Status) : IntAndStatus =
    _get_next_with_new_status (_compute_new_status (s) )

  def apply (n : Int) : Seq [Int] =
    _tailrec_take (n) (Seq () ) (_initial_status) (_get_next (_initial_status) )

}

case class PiIterator_ () extends PiIterator

object PiIterator {
  def mk : PiIterator =
    PiIterator_ ()
}
