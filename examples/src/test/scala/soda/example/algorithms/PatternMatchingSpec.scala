package soda.example.algorithms

case class PatternMatchingSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  lazy val instance = PatternMatching_ ()

  test ("get value and name of singleton - 1")
    {
      lazy val input = Singleton_ (5)
      lazy val expected = 5
      lazy val obtained = instance.get_value (input)
     assert (obtained == expected) }

  test ("get value and name of singleton - 2")
    {
      lazy val input = Singleton_ (5)
      lazy val expected_name = "singleton(x)"
      lazy val obtained_name = instance.get_type_name (input)
     assert (obtained_name == expected_name) }

  test ("get value and name of pair - 1")
    {
      lazy val input = Pair_ (10, 100)
      lazy val expected = 55
      lazy val obtained = instance.get_value (input)
     assert (obtained == expected) }

  test ("get value and name of pair - 2")
    {
      lazy val input = Pair_ (10, 100)
      lazy val expected_name = "pair(x, y)"
      lazy val obtained_name = instance.get_type_name (input)
     assert (obtained_name == expected_name) }

  test ("get value and name of triplet - 1")
    {
      lazy val input = Triplet_ (9, 100, 890)
      lazy val expected = 333
      lazy val obtained = instance.get_value (input)
     assert (obtained == expected) }

  test ("get value and name of triplet - 2")
    {
      lazy val input = Triplet_ (9, 100, 890)
      lazy val expected_name = "triplet(x, y, z)"
      lazy val obtained_name = instance.get_type_name (input)
     assert (obtained_name == expected_name) }

}
