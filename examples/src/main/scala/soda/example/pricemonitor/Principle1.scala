package soda.example.pricemonitor


trait Principle1 extends Principle {

  lazy val minimum_acceptable_similarity = 0.95

  def min (x: Int, y: Int ): Int = if (x < y ) x else y

  def max (x: Int, y: Int ): Int = if (x > y ) x else y

  def complies (c1: Customer, c2: Customer, flight: Flight, date_in_days: Int ): Boolean =
    {
      lazy val price_for_c1 = get_price (c1, flight, date_in_days )
      lazy val price_for_c2 = get_price (c2, flight, date_in_days )
      lazy val similarity = min (price_for_c1, price_for_c2 ) / max (price_for_c1, price_for_c2 )
      similarity >= minimum_acceptable_similarity }
}

case class Principle1_ (pricing_agent: PricingAgent )  extends Principle1
