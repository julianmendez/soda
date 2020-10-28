package se.umu.cs.rai.scopus.translator.example.fairness

case class  Fairness(
  ranking_function: Applicant => Double,
  score_difference_tolerance: Double,
  ranking_difference_tolerance: Double,
  measure_time: Any => Double,
  maximum_execution_time: Double
) {

  def is_fair (alice: Applicant, bob: Applicant) =
    if (have_similar_score (alice.background_score, bob.background_score)
   ) have_similar_ranking (ranking_function(alice), ranking_function(bob))
    else true

  def have_similar_score (score1: Double, score2: Double) =
    difference_between (score1, score2) < score_difference_tolerance

  def have_similar_ranking (result1: Double, result2: Double) =
    difference_between (result1, result2) < ranking_difference_tolerance

  def difference_between (value: Double, another_value: Double) =
    Math.abs(value - another_value)

  def is_response_time_acceptable (applicant: Applicant) =
    if (measure_time (ranking_function (applicant)) < maximum_execution_time
   ) true
    else false

}
