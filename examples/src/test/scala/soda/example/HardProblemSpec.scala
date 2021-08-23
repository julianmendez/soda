package soda.translator.example


case class HardProblemSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  test ("some calculations 13") {
    lazy val instance = HardProblemBuilder_ () .build
    lazy val value = 13
    lazy val expected = 11
    lazy val computed = instance.compute (value )
    lazy val obtained = computed.get (value ) .get

    assert (obtained == expected )
  }

  test ("some calculations 14") {
    lazy val instance = HardProblemBuilder_ () .build
    lazy val value = 14
    lazy val expected = 19
    lazy val computed = instance.compute (value )
    lazy val obtained = computed.get (value ) .get

    assert (obtained == expected )
  }

  test ("some calculations 16") {
    lazy val instance = HardProblemBuilder_ () .build
    lazy val value = 16
    lazy val expected = 6
    lazy val computed = instance.compute (value )
    lazy val obtained = computed.get (value ) .get

    assert (obtained == expected )
  }

  test ("some calculations 20") {
    lazy val instance = HardProblemBuilder_ () .build
    lazy val value = 20
    lazy val expected = 9
    lazy val computed = instance.compute (value )
    lazy val obtained = computed.get (value ) .get

    assert (obtained == expected )
  }
}
