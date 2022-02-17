package soda.example.mathematics

trait FiboExample
{

  def   fib : Int => Int

}

case class FiboExample_ (fib : Int => Int) extends FiboExample

trait FiboExampleInSoda
  extends
    FiboExample
{

  def _rec (m : Int) (a : Int) (b : Int) : Int =
    if ( m == 0 ) a
    else if ( m == 1 ) b
    else _rec (m - 1) (b) (a + b)

  lazy val fib : Int => Int =
     n => fib_for (n)

  def fib_for (n : Int) =
    _rec (n) (0) (1)

}

case class FiboExampleInSoda_ () extends FiboExampleInSoda
