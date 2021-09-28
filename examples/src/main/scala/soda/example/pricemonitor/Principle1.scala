package soda.example.pricemonitor


case class Report1 (compliant: Boolean, price_for_c1: Int, price_for_c2: Int, similarity: Double )

trait Principle1 extends Principle {

  lazy val minimum_acceptable_similarity = 0.95

  def min (x: Int, y: Int ): Int = if (x < y ) x else y

  def max (x: Int, y: Int ): Int = if (x > y ) x else y

  def complies (c1: Customer, c2: Customer, flight: Flight, date_in_days: Int ): Report1 =
    {
      lazy val price_for_c1 = get_price (c1, flight, date_in_days )
      lazy val price_for_c2 = get_price (c2, flight, date_in_days )
      lazy val similarity = 1.0 * min (price_for_c1, price_for_c2 ) / max (price_for_c1, price_for_c2 )
      Report1 (similarity >= minimum_acceptable_similarity, price_for_c1, price_for_c2, similarity ) }
}

case class Principle1_ (pricing_agent: PricingAgent )  extends Principle1
