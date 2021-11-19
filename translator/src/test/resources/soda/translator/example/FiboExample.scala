package soda.example


trait FiboExample {

  def fib (n: Int ): Int
}

trait FiboExampleInSoda  extends FiboExample {

  def _rec (m: Int, a: Int, b: Int ): Int =
    if (m == 0 ) a
    else if (m == 1 ) b
    else _rec (m - 1, b, a + b )

  def fib (n: Int ) =
    {
      lazy val result = _rec (n, 0, 1 )
      result }
}

case class FiboExampleInSoda_ ()  extends FiboExampleInSoda
