package soda.example.pricemonitor

case class Report2 (compliant: Boolean, old_price: Int, new_price: Int )

trait Requirement2Monitor  extends RequirementMonitor {

  lazy val acceptable_yearly_increase = 1.25

  def get_report (customer: Customer, flight: Flight, date_in_days: Int ): Report2 =
    {
      lazy val old_price = get_price (customer, flight, get_a_year_before (date_in_days ) )
      lazy val new_price = get_price (customer, flight, date_in_days )
      Report2 (new_price <= old_price * acceptable_yearly_increase, old_price, new_price ) }

  def get_a_year_before (date_in_days: Int ): Int =
    date_in_days - 365

}

case class Requirement2Monitor_ (pricing_agent: PricingAgent )  extends Requirement2Monitor
