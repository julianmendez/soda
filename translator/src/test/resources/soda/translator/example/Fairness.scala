package soda.example

trait Applicant {

  def background_score: Double

}

trait AbstractFairness {

  def rank: Applicant => Double

  def score_difference_tolerance: Double

  def ranking_difference_tolerance: Double

  def measure_time: Any => Double

  def maximum_execution_time: Double

}

trait Fairness
  extends AbstractFairness {

  def is_fair (alice: Applicant, bob: Applicant ) =
    if (have_similar_score (alice.background_score, bob.background_score )
    ) have_similar_ranking (rank (alice ), rank (bob )  )
    else true

  def have_similar_score (score1: Double, score2: Double ) =
    difference_between (score1, score2 ) < score_difference_tolerance

  def have_similar_ranking (result1: Double, result2: Double ) =
    difference_between (result1, result2 ) < ranking_difference_tolerance

  def difference_between (value: Double, another_value: Double ) =
    Math.abs (value - another_value )

  def is_response_time_acceptable (applicant: Applicant ) =
    if (measure_time (rank (applicant )  ) < maximum_execution_time
    ) true
    else false

}
