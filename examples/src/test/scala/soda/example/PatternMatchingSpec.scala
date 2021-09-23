package soda.example

case class PatternMatchingSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val instance = PatternMatching_ ()

  test ("get value and name of singleton") {
    lazy val input = Singleton (5 )
    lazy val expected = 5
    lazy val obtained = instance.get_value (input )

    assert (obtained == expected )

    lazy val expected_name = "singleton(x)"
    lazy val obtained_name = instance.get_type_name (input )

    assert (obtained_name == expected_name )
  }

  test ("get value and name of pair") {
    lazy val input = Pair (10, 100 )
    lazy val expected = 55
    lazy val obtained = instance.get_value (input )

    assert (obtained == expected )

    lazy val expected_name = "pair(x, y)"
    lazy val obtained_name = instance.get_type_name (input )

    assert (obtained_name == expected_name )
  }

  test ("get value and name of triplet") {
    lazy val input = Triplet (9, 100, 890 )
    lazy val expected = 333
    lazy val obtained = instance.get_value (input )

    assert (obtained == expected )

    lazy val expected_name = "triplet(x, y, z)"
    lazy val obtained_name = instance.get_type_name (input )

    assert (obtained_name == expected_name )
  }
}
