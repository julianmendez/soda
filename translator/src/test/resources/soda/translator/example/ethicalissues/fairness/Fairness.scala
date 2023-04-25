trait Applicant
{

  def   background_score : Double

}

case class Applicant_ (background_score : Double) extends Applicant

trait Fairness
{

  def   score_difference_tolerance : Double
  def   ranking_difference_tolerance : Double
  def   rank : Applicant => Double

  def difference_between (value : Double) (another_value : Double) : Double =
    Math.abs (value - another_value)

  def have_similar_ranking (result1 : Double) (result2 : Double) : Boolean =
    difference_between (result1) (result2) < ranking_difference_tolerance

  def have_similar_score (score1 : Double) (score2 : Double) : Boolean =
    difference_between (score1) (score2) < score_difference_tolerance

  def is_fair (alice : Applicant) (bob : Applicant) : Boolean =
    if ( have_similar_score (alice .background_score) (bob .background_score)
    ) have_similar_ranking (rank (alice) ) (rank (bob) )
    else true

}

case class Fairness_ (score_difference_tolerance : Double, ranking_difference_tolerance : Double, rank : Applicant => Double) extends Fairness
