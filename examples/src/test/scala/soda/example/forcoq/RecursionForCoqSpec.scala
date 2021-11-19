package soda.example.forcoq

case class RecursionForCoqSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val exampleSeq: Seq [Int] = Seq (0, 1, 1, 2, 3, 5, 8 )

  test ("fold left while with Seq")
    {
      lazy val initial_value = Seq [String]  ()
      lazy val next_value_function: Seq [String] => Int => Seq [String] = (s: Seq [String]  ) => (e: Int ) => s .+: ("" + (e + 100 )  )
      lazy val condition: Seq [String] => Int => Boolean = (s: Seq [String]  ) => (e: Int ) => e < 5
      lazy val expected = Seq ("103", "102", "101", "101", "100")
      lazy val obtained = RecursionForCoq_ () .fold4 (exampleSeq ) (initial_value ) (next_value_function ) (condition )
      assert (obtained == expected ) }

  test ("fold left with Seq")
    {
      lazy val initial_value = Seq [String]  ()
      lazy val next_value_function: Seq [String] => Int => Seq [String] = (s: Seq [String]  ) => (e: Int ) => s .+: ("" + (e + 100 )  )
      lazy val expected = Seq ("108", "105", "103", "102", "101", "101", "100")
      lazy val obtained = RecursionForCoq_ () .fold3 (exampleSeq ) (initial_value ) (next_value_function )
      assert (obtained == expected ) }

  test ("range with positive number")
    {
      lazy val expected = Seq [Int]  (0, 1, 2, 3, 4, 5, 6, 7 )
      lazy val obtained = RecursionForCoq_ () .range (8 )
      assert (obtained == expected ) }

  test ("range with zero size")
    {
      lazy val expected = Seq [Int]  ()
      lazy val obtained = RecursionForCoq_ () .range (-1 )
      assert (obtained == expected ) }

  test ("range with negative number")
    {
      lazy val expected = Seq [Int]  ()
      lazy val obtained = RecursionForCoq_ () .range (-1 )
      assert (obtained == expected ) }

}
