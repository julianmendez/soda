package soda.example


trait Customer {

  def name: String

  def ip_address: String
}

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

trait PricingAgent {
  import java.util.Date

  def get_price (customer: Customer, flight: Flight, date: Date ): Int
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


trait PriceMonitorAgent {
  import java.util.Date

  def pricing_agent: PricingAgent

  def get_a_year_before (date: Date ): Date

  def as_flight (segment: Segment ): Flight

  def get_price (customer: Customer, flight: Flight, date: Date ): Int =
    pricing_agent.get_price (customer, flight, date )

  def min (x: Int, y: Int ): Int = if (x < y ) x else y

  def max (x: Int, y: Int ): Int = if (x > y ) x else y

  lazy val minimum_acceptable_similarity = 0.95

  def complies_with_societal_principle_1 (customer1: Customer, customer2: Customer, flight: Flight, date: Date ): Boolean =
    {
      lazy val price_for_customer1 = get_price (customer1, flight, date )
      lazy val price_for_customer2 = get_price (customer2, flight, date )
      lazy val similarity = min (price_for_customer1, price_for_customer2 ) / max (price_for_customer1, price_for_customer2 )
      similarity  >= minimum_acceptable_similarity }

  lazy val acceptable_yearly_increase = 1.25

  def complies_with_societal_principle_2 (customer: Customer, flight: Flight, date: Date ): Boolean =
    {
      lazy val old_price = get_price (customer, flight, get_a_year_before (date ) )
      lazy val new_price = get_price (customer, flight, date )
      new_price <= old_price * acceptable_yearly_increase }

  def complies_with_societal_principle_3 (customer: Customer, flight: Flight, date: Date ): Boolean =
    get_price (customer, flight, date ) <= price_of_flight_by_segments (customer, flight, date )

  def price_of_flight_by_segments (customer: Customer, flight: Flight, date: Date ): Int =
    sum_of_prices (prices_of_segments (customer, flight.segments, date )  )

  def prices_of_segments (customer: Customer, segments: Seq [Segment], date: Date ): Seq [Int] =
    segments.map (segment => get_price (customer, SingleSegmentFlight_ (segment.airport_start, segment.airport_end ), date ) )

  def sum_of_prices (prices: Seq [Int]  ): Int =
    prices.sum
}

trait SingleSegmentFlight  extends Flight {

  lazy val intermediate_stops = Seq ()
}

case class SingleSegmentFlight_ (airport_start: Airport, airport_end: Airport )  extends SingleSegmentFlight
