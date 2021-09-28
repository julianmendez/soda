package soda.example.pricemonitor


trait Customer {

  def name: String

  def ip_address: String
}

case class Customer_ (name: String, ip_address: String )  extends Customer

trait PricingAgent {

  def get_price: (Customer, Flight, Int ) => Int
}

case class PricingAgent_ (get_price: (Customer, Flight, Int ) => Int )  extends PricingAgent

trait Airport {
  import soda.lib.OptionSD
  import soda.lib.SomeSD_
  import soda.lib.NoneSD_

  def name: String

  lazy val Airport_invariant =
    name.length == 3 &&
    name.forall (ch => 'A' <= ch && ch <= 'Z')

  lazy val Airport_checked: OptionSD [Airport] =
    if (Airport_invariant ) SomeSD_ (this ) else NoneSD_ ()
}

case class _Airport_ (name: String )  extends Airport

case class AirportBuilder () {
  import soda.lib.OptionSD

  def build (name: String ): OptionSD [Airport] =
    _Airport_ (name ) .Airport_checked
}

trait Trip {

  def airport_start: Airport

  def airport_end: Airport
}

trait Segment  extends Trip {

  lazy val invariant_Segment =
    ! (airport_start == airport_end )
}

case class Segment_ (airport_start: Airport, airport_end: Airport )  extends Segment

trait Flight  extends Trip {

  def intermediate_stops: Seq [Airport]

  lazy val segments =
    _segments_multi (airport_start, intermediate_stops, airport_end )

  def _segments_multi (airport_start: Airport, intermediate_stops: Seq [Airport], airport_end: Airport ): Seq [Segment] =
    intermediate_stops  match {
      case Nil => Nil.+: (Segment_ (airport_start, airport_end )  )
      case x:: xs => _segments_multi (x, xs, airport_end ) .+: (Segment_ (airport_start, x )  )
    }

  lazy val invariant_Flight =
    ! (intermediate_stops.contains (airport_start )  ) &&
      ! (intermediate_stops.contains (airport_end )  )
}

trait FlightSocietalPrinciple {

  def pricing_agent: PricingAgent

  def get_price (customer: Customer, flight: Flight, date_in_days: Int ): Int =
    pricing_agent.get_price (customer, flight, date_in_days )
}

trait FlightSocietalPrinciple1 extends FlightSocietalPrinciple {

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

case class FlightSocietalPrinciple1_ (get_price: (Customer, Flight, Int ) => Int )

trait FlightSocietalPrinciple2  extends FlightSocietalPrinciple {

  lazy val acceptable_yearly_increase = 1.25

  def get_a_year_before (date_in_days: Int ): Int =
    date_in_days - 365

  def complies (customer: Customer, flight: Flight, date_in_days: Int ): Boolean =
    {
      lazy val old_price = get_price (customer, flight, get_a_year_before (date_in_days ) )
      lazy val new_price = get_price (customer, flight, date_in_days )
      new_price <= old_price * acceptable_yearly_increase }
}

case class FlightSocietalPrinciple2_ (get_price: (Customer, Flight, Int ) => Int )

trait FlightSocietalPrinciple3  extends FlightSocietalPrinciple {

  def complies (customer: Customer, flight: Flight, date_in_days: Int ): Boolean =
    get_price (customer, flight, date_in_days ) <= price_of_flight_by_segments (customer, flight, date_in_days )

  def price_of_flight_by_segments (customer: Customer, flight: Flight, date_in_days: Int ): Int =
    sum_of_prices (prices_of_segments (customer, flight.segments, date_in_days )  )

  def prices_of_segments (customer: Customer, segments: Seq [Segment], date_in_days: Int ): Seq [Int] =
    segments.map (segment => get_price (customer, SingleSegmentFlight_ (segment.airport_start, segment.airport_end ), date_in_days ) )

  def sum_of_prices (prices: Seq [Int]  ): Int =
    prices.sum
}

case class FlightSocietalPrinciple3_ (get_price: (Customer, Flight, Int ) => Int )

trait PriceMonitorAgent {
  import java.util.Date

  def pricing_agent: PricingAgent

  def get_a_year_before (date_in_days: Date ): Date

  def as_flight (segment: Segment ): Flight
}

trait SingleSegmentFlight  extends Flight {

  lazy val intermediate_stops = Seq ()
}

case class SingleSegmentFlight_ (airport_start: Airport, airport_end: Airport )  extends SingleSegmentFlight
