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
      case head :: tail => (rec_segments_multi (head) (tail) (last_airport) ) .+: (
        Segment_ (first_airport , head) )
      case _otherwise => Nil .+: (Segment_ (first_airport , last_airport) )
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
