package soda.example.pricemonitor


trait Principle2  extends Principle {

  lazy val acceptable_yearly_increase = 1.25

  def get_a_year_before (date_in_days: Int ): Int =
    date_in_days - 365

  def complies (customer: Customer, flight: Flight, date_in_days: Int ): Boolean =
    {
      lazy val old_price = get_price (customer, flight, get_a_year_before (date_in_days ) )
      lazy val new_price = get_price (customer, flight, date_in_days )
      new_price <= old_price * acceptable_yearly_increase }
}

case class Principle2_ (pricing_agent: PricingAgent )  extends Principle2
