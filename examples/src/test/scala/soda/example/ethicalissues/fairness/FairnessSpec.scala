package soda.example.ethicalissues.fairness

case class FairnessSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val rank_1: Applicant => Double =
     applicant =>
      applicant.background_score * 2

  lazy val rank_2: Applicant => Double =
     applicant =>
      (1 - applicant.background_score) * 2

  test ("simple test for fairness (true)") (
    check (
      obtained = Fairness_ (score_difference_tolerance = 2, ranking_difference_tolerance = 5, rank_1)
        .is_fair (Applicant_ (78) ) (Applicant_ (77) )
    ) (
      expected = true
    )
  )

  test ("simple test for fairness (false)") (
    check (
      Fairness_ (score_difference_tolerance = 2, ranking_difference_tolerance = 1, rank_2)
        .is_fair (Applicant_ (78) ) (Applicant_ (77) )
    ) (
      expected = false
    )
  )

}
