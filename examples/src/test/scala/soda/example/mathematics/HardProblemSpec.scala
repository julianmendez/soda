package soda.example.mathematics

case class HardProblemSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val empty_map = Map [Int, Int] ()

  test ("hard problem 9")
    {
      lazy val instance = HardProblem_ ()
      lazy val value = 9
      lazy val expected = (19, Seq ((9, 19 ), (28, 18 ), (14, 17 ), (7, 16 ), (22, 15 ), (11, 14 ), (34, 13 ), (17, 12 ), (52, 11 ), (26, 10 ), (13, 9 ), (40, 8 ), (20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

  test ("hard problem 12")
    {
      lazy val instance = HardProblem_ ()
      lazy val value = 12
      lazy val expected = (9, Seq ((12, 9 ), (6, 8 ), (3, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

  test ("hard problem 13")
    {
      lazy val instance = HardProblem_ ()
      lazy val value = 13
      lazy val expected = (9, Seq ((13, 9 ), (40, 8 ), (20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

  test ("hard problem 14")
    {
      lazy val instance = HardProblem_ ()
      lazy val value = 14
      lazy val expected = (17, Seq ((14, 17 ), (7, 16 ), (22, 15 ), (11, 14 ), (34, 13 ), (17, 12 ), (52, 11 ), (26, 10 ), (13, 9 ), (40, 8 ), (20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

  test ("hard problem 16")
    {
      lazy val instance = HardProblem_ ()
      lazy val value = 16
      lazy val expected = (4, Seq ((16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

  test ("hard problem 20")
    {
      lazy val instance = HardProblem_ ()
      lazy val value = 20
      lazy val expected = (7, Seq ((20, 7 ), (10, 6 ), (5, 5 ), (16, 4 ), (8, 3 ), (4, 2 ), (2, 1 ), (1, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

  test ("memoized fibonacci 20")
    {
      lazy val instance = MemoizedFibonacci_ ()
      lazy val value = 20
      lazy val expected = (6765, Seq ((20, 6765 ), (19, 4181 ), (18, 2584 ), (17, 1597 ), (16, 987 ), (15, 610 ), (14, 377 ), (13, 233 ), (12, 144 ), (11, 89 ), (10, 55 ), (9, 34 ), (8, 21 ), (7, 13 ), (6, 8 ), (5, 5 ), (4, 3 ), (3, 2 ), (2, 1 ), (1, 1 ), (0, 0 ) ) .toMap )
      lazy val obtained = instance.compute (value, empty_map )
      assert (obtained == expected ) }

}
