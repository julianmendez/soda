package soda.example.pricemonitor


trait Customer {

  def name: String

  def ip_address: String
}

case class Customer_ (name: String, ip_address: String )  extends Customer

trait PricingAgent {
  import java.util.Date

  def get_price (customer: Customer, flight: Flight, date_in_days: Int ): Int

  def get_days_for (date: Date ): Int =
    (date.getTime / (1000 * 60 * 60 * 24 )  ) .toInt
}

trait Flight {

  def start_airport: String

  def intermediate_airports: Seq [String]

  def end_airport: String
}

case class Flight_ (start_airport: String, intermediate_airports: Seq [String], end_airport: String )  extends Flight

trait Principle {

  def pricing_agent: PricingAgent

  def get_price (customer: Customer, flight: Flight, date_in_days: Int ): Int =
    pricing_agent.get_price (customer, flight, date_in_days )
}

trait Principle1 extends Principle {

  lazy val minimum_acceptable_similarity = 0.95

  def min (x: Int, y: Int ): Int = if (x < y ) x else y

  def max (x: Int, y: Int ): Int = if (x > y ) x else y

  def complies (c1: Customer, c2: Customer, flight: Flight, date_in_days: Int ): Boolean =
    {
      lazy val price_for_c1 = get_price (c1, flight, date_in_days )
      lazy val price_for_c2 = get_price (c2, flight, date_in_days )
      lazy val similarity = min (price_for_c1, price_for_c2 ) / max (price_for_c1, price_for_c2 )
      similarity >= minimum_acceptable_similarity }
}

case class Principle1_ (pricing_agent: PricingAgent )  extends Principle1


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
