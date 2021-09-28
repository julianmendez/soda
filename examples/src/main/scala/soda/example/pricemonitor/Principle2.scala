package soda.example.pricemonitor


trait Report2 {

  def compliant: Boolean

  def old_price: Int

  def new_price: Int
}

case class Report2_ (compliant: Boolean, old_price: Int, new_price: Int )  extends Report2

trait Principle2  extends Principle {

  lazy val acceptable_yearly_increase = 1.25

  def get_a_year_before (date_in_days: Int ): Int =
    date_in_days - 365

  def complies (customer: Customer, flight: Flight, date_in_days: Int ): Report2 =
    {
      lazy val old_price = get_price (customer, flight, get_a_year_before (date_in_days ) )
      lazy val new_price = get_price (customer, flight, date_in_days )
      Report2_ (new_price <= old_price * acceptable_yearly_increase, old_price, new_price ) }
}

case class Principle2_ (pricing_agent: PricingAgent )  extends Principle2
