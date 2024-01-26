package soda.example.forlean.algorithms

/*
 * This package contains examples using recursion for Lean.
 */



trait Package

case class RecursionForLeanSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_seq : List [Int] = List (0 , 1 , 1 , 2 , 3 , 5 , 8)

  private lazy val _fold_left_while_initial_value = List [String] ()

  private lazy val _fold_left_while_next_value_function : List [String] => Int => List [String] =
     (s : List [String] ) =>  (e : Int) => s  .+: ("" +  (e + 100))

  private lazy val _fold_left_while_condition : List [String] => Int => Boolean =
     (s : List [String] ) =>  (e : Int) => e < 5

  private lazy val _fold_while = FoldWhile_ ()

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  test ("fold left while with Seq") (
    check (
      obtained =
        _fold_while
          .apply (example_seq) (_fold_left_while_initial_value) (
            _fold_left_while_next_value_function) (_fold_left_while_condition)
    ) (
      expected = List ("103" , "102" , "101" , "101" , "100")
    )
  )

  private lazy val _fold_left_initial_value = List [String] ()

  private lazy val _fold_left_next_value_function : List [String] => Int => List [String] =
     (s : List [String]) =>  (e : Int) => s .+: ("" + (e + 100))

  test ("fold left with Seq") (
    check (
      obtained =
        _fold
          .apply (example_seq) (_fold_left_initial_value) (
            _fold_left_next_value_function)
    ) (
      expected = List ("108" , "105" , "103" , "102" , "101" , "101" , "100")
    )
  )

  test ("range with positive number") (
    check (
      obtained = _range .apply ((8) )
    ) (
      expected = List [Int] (0 , 1 , 2 , 3 , 4 , 5 , 6 , 7)
    )
  )

  test ("range with zero size") (
    check (
      obtained = _range .apply (0)
    ) (
      expected = List [Int] ()
    )
  )

}


case class SwapExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val zero = 0

  lazy val three = 3

  lazy val five = 5

  lazy val pair_1 = PairExample_ (three, five)

  lazy val pair_2 = PairExample_ (five, three)

  lazy val instance = SwapExample_ ()

  test ("one swap") (
    check (
      obtained = instance .swap (pair_1)
    ) (
      expected = pair_2
    )
  )

  test ("two swaps") (
    check (
      obtained = instance .swap (instance .swap (pair_1) )
    ) (
      expected = pair_1
    )
  )

}

