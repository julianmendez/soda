package soda.example.mathematics

case class PiIteratorSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val piStart = "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446"

  test ("first digits of Pi")
    {
      lazy val piSequence = PiIterator_ () .take (128 )
      lazy val obtained = "" + piSequence.head + "." + piSequence.tail.mkString ("")
      assert (obtained == piStart ) }

}
