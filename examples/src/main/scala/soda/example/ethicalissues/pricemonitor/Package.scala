package soda.example.ethicalissues.pricemonitor

/*
 * This package contains example classes for a price monitor.
 */

trait Package

trait Customer
{

  def   name : String
  def   ip_address : String

}

case class Customer_ (name : String, ip_address : String) extends Customer

trait PricingAgent
{

  import   java.util.Date

    /** get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int */
  def   abs_get_price : Customer => Flight => Int => Int

  def get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    abs_get_price (customer) (flight) (date_in_days)

  lazy val milliseconds_per_day : Long = 24 * 60 * 60 * 1000

  def get_days_for (date : Date) : Int =
    (date.getTime / milliseconds_per_day).toInt

}

case class PricingAgent_ (abs_get_price : Customer => Flight => Int => Int) extends PricingAgent

trait Flight
{

  def   start_airport : String
  def   intermediate_airports : Seq [String]
  def   end_airport : String

}

case class Flight_ (start_airport : String, intermediate_airports : Seq [String], end_airport : String) extends Flight

trait RequirementMonitor
{

  def   pricing_agent : PricingAgent

  def get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    pricing_agent.get_price (customer) (flight) (date_in_days)

}

case class RequirementMonitor_ (pricing_agent : PricingAgent) extends RequirementMonitor


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
    if ( x < y
    ) x
    else y

  def max (x : Int) (y : Int) : Int =
    if ( x < y
    ) y
    else x

}

case class Requirement1Monitor_ (pricing_agent : PricingAgent) extends Requirement1Monitor


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


trait Report3
{

  def   compliant : Boolean
  def   price_of_flight : Int
  def   price_of_flight_by_segments : Int

}

case class Report3_ (compliant : Boolean, price_of_flight : Int, price_of_flight_by_segments : Int) extends Report3

trait Requirement3Monitor
  extends
    RequirementMonitor
{

  def   pricing_agent : PricingAgent

  def get_report (customer : Customer) (flight : Flight) (date_in_days : Int) : Report3 =
    get_report_with (
      get_price (customer) (flight) (date_in_days) ) (
      get_price_of_flight_by_segments (customer) (flight) (date_in_days)
    )

  def get_report_with (price_of_flight : Int) (price_of_flight_by_segments : Int) : Report3 =
    Report3_ (price_of_flight <= price_of_flight_by_segments, price_of_flight, price_of_flight_by_segments)

  def get_price_of_flight_by_segments (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    sum_prices (get_prices_of_segments (customer) (SegmentsForFlight_ (flight).segments) (date_in_days) )

  def get_prices_of_segments (customer : Customer) (segments : Seq [Segment] ) (date_in_days : Int) : Seq [Int] =
    segments.map ( segment => get_price (customer) (segment) (date_in_days) )

  def sum_prices (prices : Seq [Int] ) : Int =
    prices.sum

}

case class Requirement3Monitor_ (pricing_agent : PricingAgent) extends Requirement3Monitor

trait Segment
  extends
    Flight
{

  def   start_airport : String
  def   end_airport : String

  lazy val intermediate_airports = Seq [String] ()

}

case class Segment_ (start_airport : String, end_airport : String) extends Segment

trait SegmentsForFlight
{

  def   flight : Flight

  lazy val segments : Seq [Segment] =
    rec_segments_multi (flight.start_airport) (flight.intermediate_airports) (flight.end_airport)

  def rec_segments_multi (first_airport : String) (intermediate_stops : Seq [String] ) (last_airport : String) : Seq [Segment] =
    intermediate_stops match  {
      case head :: tail => (rec_segments_multi (head) (tail) (last_airport) ).+: (Segment_ (first_airport, head) )
      case x => Nil.+: (Segment_ (first_airport, last_airport) )
    }

}

case class SegmentsForFlight_ (flight : Flight) extends SegmentsForFlight

