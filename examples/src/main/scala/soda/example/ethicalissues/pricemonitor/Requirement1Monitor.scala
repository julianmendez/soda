package soda.example.ethicalissues.pricemonitor

case class Report1 (compliant: Boolean, price_c1: Int, price_c2: Int, similarity: Double )

trait Requirement1Monitor extends RequirementMonitor {

  lazy val minimum_acceptable_similarity = 0.95

  def get_report (c1: Customer, c2: Customer, flight: Flight, date_in_days: Int ): Report1 =
    get_report_with (get_price (c1, flight, date_in_days ), get_price (c2, flight, date_in_days )    )

  def get_report_with (price_c1: Int, price_c2: Int ): Report1 =
    get_report_with (price_c1, price_c2, get_similarity (price_c1, price_c2 )  )

  def get_report_with (price_c1: Int, price_c2: Int, similarity: Double ): Report1 =
    Report1 (minimum_acceptable_similarity <= similarity, price_c1, price_c2, similarity )

  def get_similarity (x: Int, y: Int ): Double =
    1.0 * min (x, y ) / max (x, y )

  def min (x: Int, y: Int ): Int =
    if (x < y ) x else y

  def max (x: Int, y: Int ): Int =
    if (x < y ) y else x

}

case class Requirement1Monitor_ (pricing_agent: PricingAgent )  extends Requirement1Monitor
