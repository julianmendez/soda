package soda.example.pricemonitor


trait Principle3  extends Principle {

  def complies (customer: Customer, flight: Flight, date_in_days: Int ): Boolean =
    get_price (customer, flight, date_in_days ) <= price_of_flight_by_segments (customer, flight, date_in_days )

  def price_of_flight_by_segments (customer: Customer, flight: Flight, date_in_days: Int ): Int =
    sum_of_prices (prices_of_segments (customer, SegmentsForFlight_ (flight ) .segments, date_in_days )  )

  def prices_of_segments (customer: Customer, segments: Seq [Segment], date_in_days: Int ): Seq [Int] =
    segments.map (segment => get_price (customer, segment, date_in_days ) )

  def sum_of_prices (prices: Seq [Int]  ): Int =
    prices.sum
}

case class Principle3_ (pricing_agent: PricingAgent )  extends Principle3

trait Segment  extends Flight {

  lazy val intermediate_airports = Seq [String]  ()
}

case class Segment_ (start_airport: String, end_airport: String )  extends Segment

trait SegmentsForFlight {

  def flight: Flight

  lazy val segments: Seq [Segment] =
    _segments_multi (flight.start_airport, flight.intermediate_airports, flight.end_airport )

  def _segments_multi (first_airport: String, intermediate_stops: Seq [String], last_airport: String ): Seq [Segment] =
    intermediate_stops  match {
      case Nil => Nil.+: (Segment_ (first_airport, last_airport )  )
      case x:: xs => _segments_multi (x, xs, last_airport ) .+: (Segment_ (first_airport, x )  )
    }
}

case class SegmentsForFlight_ (flight: Flight )  extends SegmentsForFlight
