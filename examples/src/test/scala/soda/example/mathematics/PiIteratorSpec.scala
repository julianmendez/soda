package soda.example.mathematics

case class PiIteratorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val pi_start = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446"

  lazy val pi_sequence = PiIterator_ ().take (128)

  test ("first digits of Pi") (
    check (
      obtained = "" + pi_sequence.head + "." + pi_sequence.tail.mkString ("") ) (
      expected = pi_start
    )
  )

}
