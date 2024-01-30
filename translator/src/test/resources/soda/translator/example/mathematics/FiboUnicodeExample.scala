trait FiboUnicodeExample
{



  private def _rec (m : Int) (a : Int) (b : Int) : Int =
    if ( m == 0 ) a
     else if ( m == 1 ) b
        else _rec (m - 1) (b) (a + b)

  def apply (n : Int) : Int =
    _rec (n) (0) (1)

}

case class FiboUnicodeExample_ () extends FiboUnicodeExample

object FiboUnicodeExample {
  def mk : FiboUnicodeExample =
    FiboUnicodeExample_ ()
}
