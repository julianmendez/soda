package soda.example.ethicalissues.pricemonitor

/*
 * This package contains example classes for a price monitor.
 */

trait Customer
{

  def   name : String
  def   ip_address : String

}

case class Customer_ (name : String, ip_address : String) extends Customer

object Customer {
  def mk (name : String) (ip_address : String) : Customer =
    Customer_ (name, ip_address)
}

trait Flight
{

  def   start_airport : String
  def   intermediate_airports : Seq [String]
  def   end_airport : String

}

case class Flight_ (start_airport : String, intermediate_airports : Seq [String], end_airport : String) extends Flight

object Flight {
  def mk (start_airport : String) (intermediate_airports : Seq [String]) (end_airport : String) : Flight =
    Flight_ (start_airport, intermediate_airports, end_airport)
}

trait PricingAgent
{

    /** get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int */
  def   abs_get_price : Customer => Flight => Int => Int

  import   java.util.Date

  def get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    abs_get_price (customer) (flight) (date_in_days)

  lazy val milliseconds_per_day : Long = 24 * 60 * 60 * 1000

  def get_days_for (date : Date) : Int =
    (date .getTime / milliseconds_per_day) .toInt

}

case class PricingAgent_ (abs_get_price : Customer => Flight => Int => Int) extends PricingAgent

object PricingAgent {
  def mk (abs_get_price : Customer => Flight => Int => Int) : PricingAgent =
    PricingAgent_ (abs_get_price)
}

trait RequirementMonitor
{

  def   pricing_agent : PricingAgent

  def get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    pricing_agent .get_price (customer) (flight) (date_in_days)

}

case class RequirementMonitor_ (pricing_agent : PricingAgent) extends RequirementMonitor

object RequirementMonitor {
  def mk (pricing_agent : PricingAgent) : RequirementMonitor =
    RequirementMonitor_ (pricing_agent)
}


trait Report1
{

  def   compliant : Boolean
  def   price1 : Int
  def   price2 : Int
  def   similarity : Double

}

case class Report1_ (compliant : Boolean, price1 : Int, price2 : Int, similarity : Double) extends Report1

object Report1 {
  def mk (compliant : Boolean) (price1 : Int) (price2 : Int) (similarity : Double) : Report1 =
    Report1_ (compliant, price1, price2, similarity)
}

trait Requirement1Monitor
  extends
    RequirementMonitor
{

  def   pricing_agent : PricingAgent

  lazy val minimum_similarity = 0.95

  def min (x : Int) (y : Int) : Int =
    if ( x < y
    ) x
    else y

  def max (x : Int) (y : Int) : Int =
    if ( x < y
    ) y
    else x

  def get_similarity (x : Int) (y : Int) : Double =
    1.0 * (min (x) (y) ) / (max (x) (y) )

  def get_report_with_similarity (price1 : Int) (price2 : Int) (similarity : Double) : Report1 =
    Report1_ (minimum_similarity <= similarity, price1, price2, similarity)

  def get_report_with (price1 : Int) (price2 : Int) : Report1 =
    get_report_with_similarity (price1) (price2) (get_similarity (price1) (price2) )

  def get_report (c1 : Customer) (c2 : Customer) (flight : Flight) (date : Int) : Report1 =
    get_report_with (
      price1 = get_price (c1) (flight) (date) ) (
      price2 = get_price (c2) (flight) (date)
    )

}

case class Requirement1Monitor_ (pricing_agent : PricingAgent) extends Requirement1Monitor

object Requirement1Monitor {
  def mk (pricing_agent : PricingAgent) : Requirement1Monitor =
    Requirement1Monitor_ (pricing_agent)
}


trait Report2
{

  def   compliant : Boolean
  def   old_price : Int
  def   new_price : Int

}

case class Report2_ (compliant : Boolean, old_price : Int, new_price : Int) extends Report2

object Report2 {
  def mk (compliant : Boolean) (old_price : Int) (new_price : Int) : Report2 =
    Report2_ (compliant, old_price, new_price)
}

trait Requirement2Monitor
  extends
    RequirementMonitor
{

  def   pricing_agent : PricingAgent

  lazy val acceptable_increase = 1.25

  def get_a_year_before (date_in_days : Int) : Int =
    date_in_days - 365

  def get_report_with (old_price : Int) (new_price : Int) : Report2 =
    Report2_ (new_price <= old_price * acceptable_increase, old_price, new_price)

  def get_report (customer : Customer) (flight : Flight) (date_in_days : Int) : Report2 =
    get_report_with (
      old_price = get_price (customer) (flight) (get_a_year_before (date_in_days) ) ) (
      new_price = get_price (customer) (flight) (date_in_days)
    )

}

case class Requirement2Monitor_ (pricing_agent : PricingAgent) extends Requirement2Monitor

object Requirement2Monitor {
  def mk (pricing_agent : PricingAgent) : Requirement2Monitor =
    Requirement2Monitor_ (pricing_agent)
}


trait Report3
{

  def   compliant : Boolean
  def   price_of_flight : Int
  def   price_of_flight_by_segments : Int

}

case class Report3_ (compliant : Boolean, price_of_flight : Int, price_of_flight_by_segments : Int) extends Report3

object Report3 {
  def mk (compliant : Boolean) (price_of_flight : Int) (price_of_flight_by_segments : Int) : Report3 =
    Report3_ (compliant, price_of_flight, price_of_flight_by_segments)
}

trait Segment
  extends
    Flight
{

  def   start_airport : String
  def   end_airport : String

  lazy val intermediate_airports = Seq [String] ()

}

case class Segment_ (start_airport : String, end_airport : String) extends Segment

object Segment {
  def mk (start_airport : String) (end_airport : String) : Segment =
    Segment_ (start_airport, end_airport)
}

trait SegmentsForFlight
{

  def   flight : Flight

  def rec_segments_multi (first_airport : String) (intermediate_stops : Seq [String] )
      (last_airport : String) : Seq [Segment] =
    intermediate_stops match  {
      case Nil => Nil .+: (Segment_ (first_airport , last_airport) )
      case head +: tail => (rec_segments_multi (head) (tail) (last_airport) ) .+: (
        Segment_ (first_airport , head) )
    }

  lazy val segments : Seq [Segment] =
    rec_segments_multi (flight .start_airport) (flight .intermediate_airports) (
      flight .end_airport)

}

case class SegmentsForFlight_ (flight : Flight) extends SegmentsForFlight

object SegmentsForFlight {
  def mk (flight : Flight) : SegmentsForFlight =
    SegmentsForFlight_ (flight)
}

trait Requirement3Monitor
  extends
    RequirementMonitor
{

  def   pricing_agent : PricingAgent

  def get_report_with (price : Int) (price_by_segments : Int) : Report3 =
    Report3_ (price <= price_by_segments, price, price_by_segments)

  def sum_prices (prices : Seq [Int] ) : Int =
    prices .sum

  def get_prices_of_segments (customer : Customer) (segments : Seq [Segment] ) (date_in_days : Int)
      : Seq [Int] =
    segments .map ( segment => get_price (customer) (segment) (date_in_days) )

  def get_price_of_flight_by_segments (customer : Customer) (flight : Flight) (date_in_days : Int)
      : Int =
    sum_prices (
      get_prices_of_segments (customer) (SegmentsForFlight_ (flight) .segments) (date_in_days) )

  def get_report (customer : Customer) (flight : Flight) (date_in_days : Int) : Report3 =
    get_report_with (
      get_price (customer) (flight) (date_in_days) ) (
      get_price_of_flight_by_segments (customer) (flight) (date_in_days)
    )

}

case class Requirement3Monitor_ (pricing_agent : PricingAgent) extends Requirement3Monitor

object Requirement3Monitor {
  def mk (pricing_agent : PricingAgent) : Requirement3Monitor =
    Requirement3Monitor_ (pricing_agent)
}

