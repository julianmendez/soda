package soda.example.mathematics

/*
 * This package contains tests for examples in Soda that use mathematical properties.
 */

trait Package

case class FactorialSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val factorial_values = Seq (
    (-1 , 1) , (0 , 1) , (1 , 1) , (2 , 2) , (3 , 6) , (4 , 24) , (5 , 120) , (6 , 720) ,
    (7 , 5040) , (8 , 40320) , (9 , 362880) , (10 , 3628800)
  )

  private lazy val _factorial_concise = FactorialConcise_ ()

  private lazy val _factorial_simple = FactorialSimple_ ()

  private lazy val _factorial_patten_matching = FactorialPatternMatching_ ()

  private lazy val _factorial_with_fold = FactorialWithFold_ ()

  test ("should test the factorial - concise version") (
    check (
      obtained = factorial_values
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , _factorial_concise .apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

  test ("should test the factorial - simple version") (
    check (
      obtained = factorial_values
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , _factorial_simple .apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

  test ("should test the factorial - with pattern matching") (
    check (
      obtained = factorial_values
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , _factorial_patten_matching .apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

  test ("should test the factorial - with fold") (
    check (
      obtained = factorial_values
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , _factorial_with_fold .apply (n) ) )
    ) (
      expected = factorial_values
    )
  )

}


case class FiboExampleSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fibonacci_values = Seq (
   (0 , 0) , (1 , 1) , (2 , 1) , (3 , 2) , (4 , 3) , (5 , 5) , (6 , 8) , (7 , 13) , (8 , 21) ,
   (9 , 34) , (10 , 55)
  )

  private lazy val _fibo_example_in_soda = FiboExampleInSoda_ ()

  test ("should test the fibonacci function") (
    check (
      obtained = fibonacci_values
        .map ( pair => pair ._1)
        .map ( n => Tuple2 (n , _fibo_example_in_soda .apply (n) ) )
    ) (
      expected = fibonacci_values
    )
  )

}


case class HardProblemSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val empty_map = Map [Int, Int] ()

  lazy val instance = HardProblem_ ()

  test ("hard problem 9") (
    check (
      obtained = instance.compute (InputPair_ (9 , empty_map) )
    ) (
      expected = OutputPair_ (19 , Seq ( (9 , 19) , (28 , 18) , (14 , 17) , (7 , 16) ,
        (22 , 15), (11 , 14) , (34 , 13) ,
        (17 , 12) , (52 , 11) , (26 , 10), (13 , 9) , (40 , 8) , (20 , 7) , (10 , 6) ,
        (5 , 5) , (16 , 4) , (8 , 3) , (4 , 2) , (2 , 1) , (1 , 0) ) .toMap)
    )
  )

  test ("hard problem 12") (
    check (
      obtained = instance .compute (InputPair_ (12 , empty_map) )
    ) (
      expected = OutputPair_ (9 , Seq ( (12 , 9) , (6 , 8) , (3 , 7) , (10 , 6) , (5 , 5),
        (16 , 4) , (8 , 3) , (4 , 2) , (2 , 1) , (1 , 0) ) .toMap)
    )
  )

  test ("hard problem 13") (
    check (
      obtained = instance .compute (InputPair_ (13 , empty_map) )
    ) (
      expected = OutputPair_ (9 , Seq ( (13 , 9) , (40 , 8) , (20 , 7) , (10 , 6) , (5 , 5),
        (16 , 4) , (8 , 3) , (4 , 2) , (2 , 1) , (1 , 0) ) .toMap)
    )
  )

  test ("hard problem 14") (
    check (
      obtained = instance .compute (InputPair_ (14 , empty_map) )
    ) (
      expected = OutputPair_ (17 , Seq ( (14 , 17) , (7 , 16) , (22 , 15) , (11 , 14) ,
        (34 , 13) , (17 , 12) , (52 , 11) , (26 , 10), (13 , 9) , (40 , 8) , (20 , 7) ,
        (10 , 6) , (5 , 5) , (16 , 4) , (8 , 3) , (4 , 2) , (2 , 1) , (1 , 0) ) .toMap)
    )
  )

  test ("hard problem 16") (
    check (
      obtained = instance .compute (InputPair_ (16 , empty_map) )
    ) (
      expected = OutputPair_ (4 , Seq ( (16 , 4) , (8 , 3) , (4 , 2) , (2 , 1) , (1 , 0) )
        .toMap)
    )
  )

  test ("hard problem 20") (
    check (
      obtained = instance .compute (InputPair_ (20 , empty_map) )
    ) (
      expected = OutputPair_ (7 , Seq ( (20 , 7) , (10 , 6) , (5 , 5) , (16 , 4) , (8 , 3) ,
        (4 , 2) , (2 , 1) , (1 , 0) ) .toMap)
    )
  )

  test ("memoized fibonacci 20") (
    check (
      obtained = MemoizedFibonacci_ () .compute (InputPair_ (20 , empty_map) )
    ) (
      expected = OutputPair_ (6765 , Seq ( (20 , 6765) , (19 , 4181) , (18 , 2584) ,
        (17 , 1597) , (16 , 987) , (15 , 610) , (14 , 377) ,
        (13 , 233) , (12 , 144) , (11 , 89) , (10 , 55) , (9 , 34) , (8 , 21) , (7 , 13),
        (6 , 8) , (5 , 5) , (4 , 3) , (3 , 2) , (2 , 1) , (1 , 1) , (0 , 0) ) .toMap)
    )
  )

}


case class PiIteratorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val pi_start =
    "3.14159265358979323846264338327950288419716939937510582097494459" +
    "23078164062862089986280348253421170679821480865132823066470938446"

  private lazy val _pi_iterator = PiIterator_ ()

  lazy val pi_sequence = _pi_iterator .apply (128)

  test ("first digits of Pi") (
    check (
      obtained = "" + pi_sequence .head + "." + pi_sequence .tail .mkString ("")
    ) (
      expected = pi_start
    )
  )

}

