package soda.example.ethicalissues.fairness

case class FairnessSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  lazy val rank_1: Applicant => Double =
     applicant =>
      applicant.background_score * 2

  lazy val rank_2: Applicant => Double =
     applicant =>
      (1 - applicant.background_score ) * 2

  test ("simple test for fairness (true)")
    {
      lazy val instance =
        Fairness_ (score_difference_tolerance = 2, ranking_difference_tolerance = 5, rank_1 )
      lazy val obtained = instance.is_fair (Applicant_ (78 ), Applicant_ (77 )  )
      lazy val expected = true
      assert (obtained == expected ) }

  test ("simple test for fairness (false)")
    {
      lazy val instance =
        Fairness_ (score_difference_tolerance = 2, ranking_difference_tolerance = 1, rank_2 )
      lazy val obtained = instance.is_fair (Applicant_ (78 ), Applicant_ (77 )  )
      lazy val expected = false
      assert (obtained == expected ) }

}
