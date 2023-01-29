package soda.lib

case class RecursionSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_seq : Seq [Int] = Seq (0, 1, 1, 2, 3, 5, 8)

  private lazy val _fold_while = FoldWhile_ ()

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  test ("fold left while with Seq") (
    check (
      obtained = _fold_while.apply (example_seq) (_fold0_initial_value) (_fold0_next_value_function) (_fold0_condition)
    ) (
      expected = Seq ("103", "102", "101", "101", "100")
    )
  )

  private lazy val _fold0_initial_value = Seq [String] ()

  private lazy val _fold0_next_value_function : Seq [String] => Int => Seq [String] =
     (s : Seq [String]) =>  (e : Int) => (s.+: ("" + (e + 100) ) )

  private lazy val _fold0_condition : Seq [String] => Int => Boolean =
     (s : Seq [String] ) =>  (e : Int) => (e < 5)

  test ("fold left with Seq") (
    check (
      obtained = _fold.apply (example_seq) (_fold1_initial_value) (_fold1_next_value_function)
    ) (
      expected = Seq ("108", "105", "103", "102", "101", "101", "100")
    )
  )

  private lazy val _fold1_initial_value = Seq [String] ()

  private lazy val _fold1_next_value_function : Seq [String] => Int => Seq [String] =
     (s : Seq [String] ) =>  (e : Int) => (s.+: ("" + (e + 100) ) )

  test ("range with positive number") (
    check (
      obtained = _range.apply (8)
    ) (
      expected = Seq (0, 1, 2, 3, 4, 5, 6, 7)
    )
  )

  test ("range with zero size") (
    check (
      obtained = _range.apply (-1)
    ) (
      expected = Seq ()
    )
  )

  test ("range with negative number") (
    check (
      obtained = _range.apply (-1)
    ) (
      expected = Seq ()
    )
  )

}
