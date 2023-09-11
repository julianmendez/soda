package soda.tiles.fairness.tool

/*
 * This package contains test for the classes to model a fairness scenario.
 */

import   org.scalatest.funsuite.AnyFunSuite

trait Package

case class ScoringToolSpec ()
  extends
    AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_xlist_1 : Seq [Double] = Seq (1 , 3 , 5 , 8)

  lazy val example_ylist_1 : Seq [Double] = Seq (1 , 3 , 5 , 8)

  lazy val instance_1 : Pearson = Pearson_ (example_xlist_1, example_ylist_1)

  private lazy val _mt : MathTool = MathTool_ ()

  private lazy val _mc : ScoringCategory = ScoringCategory_ ()

  test ("sum") (
    check (
      obtained = _mt .sum (example_xlist_1)
    ) (
      expected = 17
    )
  )

  test ("average") (
    check (
      obtained = _mt .average (example_xlist_1)
    ) (
      expected = 4.25
    )
  )

  test ("categorize") (
    check (
      obtained = _mc .categorize (0.2)
    ) (
      expected = _mc .weak_positive_correlation
    )
  )

  lazy val example_xlist_2 : Seq [Double] = Seq (43 , 21 , 25 , 42 , 57 , 59)

  lazy val example_ylist_2 : Seq [Double] = Seq (99 , 65 , 79 , 75 , 87 , 81)

  lazy val instance_2 : Pearson = Pearson_ (example_xlist_2, example_ylist_2)

  test ("coefficient") (
    check (
      obtained = (instance_2 .coefficient >= 0.529808) && (instance_2 .coefficient < 0.529809)
    ) (
      expected = true
    )
  )

}

