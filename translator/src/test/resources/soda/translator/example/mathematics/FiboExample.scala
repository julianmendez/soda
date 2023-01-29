package soda.example.mathematics

trait FiboExampleInSoda
{

  def apply (n : Int) =
    _rec (n) (0) (1)

  private def _rec (m : Int) (a : Int) (b : Int) : Int =
    if ( m == 0 ) a
    else if ( m == 1 ) b
    else _rec (m - 1) (b) (a + b)

}

case class FiboExampleInSoda_ () extends FiboExampleInSoda
