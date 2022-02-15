package soda.example.mathematics

case class HardProblemSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained: A ) (expected: A ): org.scalatest.compatible.Assertion =
    assert (obtained == expected )

  lazy val empty_map = Map [Int, Int] ()

  lazy val instance = HardProblem_ ()

  test ("hard problem 9") (
    check (
      obtained = instance.compute (InputPair_ (9, empty_map ) ) ) (
      expected = OutputPair_ (19, Seq ((9, 19 ), (28, 18 ), (14, 17 ), (7, 16 ), (22, 15 ),
        (11, 14 ), (34, 13 ), (17, 12 ), (52, 11 ), (26, 10 ),
        (13, 9 ), (40, 8 ), (20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
    )
  )

  test ("hard problem 12") (
    check (
      obtained = instance.compute (InputPair_ (12, empty_map ) ) ) (
      expected = OutputPair_ (9, Seq ((12, 9 ), (6, 8 ), (3, 7 ), (10, 6 ), (5, 5 ),
        (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
    )
  )

  test ("hard problem 13") (
    check (
      obtained = instance.compute (InputPair_ (13, empty_map ) ) ) (
      expected = OutputPair_ (9, Seq ((13, 9 ), (40, 8 ), (20, 7 ), (10, 6 ), (5, 5 ),
        (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
    )
  )

  test ("hard problem 14") (
    check (
      obtained = instance.compute (InputPair_ (14, empty_map ) ) ) (
      expected = OutputPair_ (17, Seq ((14, 17 ), (7, 16 ), (22, 15 ), (11, 14 ), (34, 13 ), (17, 12 ), (52, 11 ), (26, 10 ),
        (13, 9 ), (40, 8 ), (20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
    )
  )

  test ("hard problem 16") (
    check (
      obtained = instance.compute (InputPair_ (16, empty_map ) ) ) (
      expected = OutputPair_ (4, Seq ((16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
    )
  )

  test ("hard problem 20") (
    check (
      obtained = instance.compute (InputPair_ (20, empty_map ) ) ) (
      expected = OutputPair_ (7, Seq ((20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
    )
  )

  test ("memoized fibonacci 20") (
    check (
      obtained = MemoizedFibonacci_ () .compute (InputPair_ (20, empty_map ) ) ) (
      expected = OutputPair_ (6765, Seq ((20, 6765 ), (19, 4181 ), (18, 2584 ), (17, 1597 ), (16, 987 ), (15, 610 ), (14, 377 ),
        (13, 233 ), (12, 144 ), (11, 89 ), (10, 55 ), (9, 34 ), (8, 21 ), (7, 13 ),
        (6, 8 ), (5, 5 ), (4, 3 ), (3, 2 ), (2, 1 ), (1, 1 ), (0, 0 ) ) .toMap )
    )
  )

}
