package soda.example.pricemonitor

case class Report1 (compliant: Boolean, price_for_c1: Int, price_for_c2: Int, similarity: Double )

trait Requirement1Monitor
 extends RequirementMonitor {

  lazy val minimum_acceptable_similarity = 0.95

  def get_report (c1: Customer, c2: Customer, flight: Flight, date_in_days: Int ): Report1 =
    {
      lazy val price_for_c1 = get_price (c1, flight, date_in_days )
      lazy val price_for_c2 = get_price (c2, flight, date_in_days )
      lazy val similarity = 1.0 * min (price_for_c1, price_for_c2 ) / max (price_for_c1, price_for_c2 )
      Report1 (minimum_acceptable_similarity <= similarity, price_for_c1, price_for_c2, similarity ) }

  def min (x: Int, y: Int ): Int =
    if (x < y ) x else y

  def max (x: Int, y: Int ): Int =
    if (x < y ) y else x

}

case class Requirement1Monitor_ (pricing_agent: PricingAgent )
  extends Requirement1Monitor
