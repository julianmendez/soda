package soda.example.ethicalissues.pricemonitor

trait Report2
{

  def   compliant : Boolean
  def   old_price : Int
  def   new_price : Int

}

case class Report2_ (compliant : Boolean, old_price : Int, new_price : Int) extends Report2

trait Requirement2Monitor
  extends
    RequirementMonitor
{

  def   pricing_agent : PricingAgent

  lazy val acceptable_increase = 1.25

  def get_report (customer : Customer) (flight : Flight) (date_in_days : Int) : Report2 =
    get_report_with (
      old_price = get_price (customer) (flight) (get_a_year_before (date_in_days) ) ) (
      new_price = get_price (customer) (flight) (date_in_days)
    )

  def get_report_with (old_price : Int) (new_price : Int) : Report2 =
    Report2_ (new_price <= old_price * acceptable_increase, old_price, new_price)

  def get_a_year_before (date_in_days : Int) : Int =
    date_in_days - 365

}

case class Requirement2Monitor_ (pricing_agent : PricingAgent) extends Requirement2Monitor
