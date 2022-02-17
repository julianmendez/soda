package soda.example.ethicalissues.pricemonitor

trait Report1
{

  def   compliant : Boolean
  def   price1 : Int
  def   price2 : Int
  def   similarity : Double

}

case class Report1_ (compliant : Boolean, price1 : Int, price2 : Int, similarity : Double) extends Report1

trait Requirement1Monitor
  extends
    RequirementMonitor
{

  def   pricing_agent : PricingAgent

  lazy val minimum_similarity = 0.95

  def get_report (c1 : Customer) (c2 : Customer) (flight : Flight) (date : Int) : Report1 =
    get_report_with (
      price1 = get_price (c1) (flight) (date) ) (
      price2 = get_price (c2) (flight) (date)
    )

  def get_report_with (price1 : Int) (price2 : Int) : Report1 =
    get_report_with_similarity (price1) (price2) (get_similarity (price1) (price2) )

  def get_report_with_similarity (price1 : Int) (price2 : Int) (similarity : Double) : Report1 =
    Report1_ (minimum_similarity <= similarity, price1, price2, similarity)

  def get_similarity (x : Int) (y : Int) : Double =
    1.0 * (min (x) (y) ) / (max (x) (y) )

  def min (x : Int) (y : Int) : Int =
    if ( x < y ) x else y

  def max (x : Int) (y : Int) : Int =
    if ( x < y ) y else x

}

case class Requirement1Monitor_ (pricing_agent : PricingAgent) extends Requirement1Monitor
