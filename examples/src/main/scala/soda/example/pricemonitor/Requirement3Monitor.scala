package soda.example.pricemonitor


case class Report3 (compliant: Boolean, price_of_flight: Int, price_of_flight_by_segments: Int )

trait Requirement3Monitor  extends RequirementMonitor {

  def get_report (customer: Customer, flight: Flight, date_in_days: Int ): Report3 =
    {
      lazy val price_of_flight = get_price (customer, flight, date_in_days )
      lazy val price_of_flight_by_segments = get_price_of_flight_by_segments (customer, flight, date_in_days )
      Report3 (price_of_flight <= price_of_flight_by_segments, price_of_flight, price_of_flight_by_segments ) }

  def get_price_of_flight_by_segments (customer: Customer, flight: Flight, date_in_days: Int ): Int =
    sum_prices (get_prices_of_segments (customer, SegmentsForFlight_ (flight ) .segments, date_in_days )  )

  def get_prices_of_segments (customer: Customer, segments: Seq [Segment], date_in_days: Int ): Seq [Int] =
    segments.map (segment => get_price (customer, segment, date_in_days ) )

  def sum_prices (prices: Seq [Int]  ): Int =
    prices.sum
}

case class Requirement3Monitor_ (pricing_agent: PricingAgent )  extends Requirement3Monitor

trait Segment  extends Flight {

  lazy val intermediate_airports = Seq [String]  ()
}

case class Segment_ (start_airport: String, end_airport: String )  extends Segment

trait SegmentsForFlight {

  def flight: Flight

  lazy val segments: Seq [Segment] =
    rec_segments_multi (flight.start_airport, flight.intermediate_airports, flight.end_airport )

  def rec_segments_multi (first_airport: String, intermediate_stops: Seq [String], last_airport: String ): Seq [Segment] =
    intermediate_stops  match {
      case Nil => Nil.+: (Segment_ (first_airport, last_airport )  )
      case x:: xs => rec_segments_multi (x, xs, last_airport ) .+: (Segment_ (first_airport, x )  )
    }
}

case class SegmentsForFlight_ (flight: Flight )  extends SegmentsForFlight
