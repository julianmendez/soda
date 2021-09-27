package soda.example



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

  def get_price (flight: Flight, date: Date ): Int
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

trait MonitoringAgent {
  import java.util.Date

  def pricing_agent: PricingAgent

  def get_a_year_before (date: Date ): Date

  def as_flight (segment: Segment ): Flight

  def prices_of_segments (segments: Seq [Segment], date: Date ): Seq [Int] =
    segments.map (segment => get_price (SingleSegmentFlight_ (segment.airport_start, segment.airport_end ), date ) )

  def sum_of_prices (prices: Seq [Int]  ): Int =
    prices.sum

  def price_of_flight_by_segments (flight: Flight, date: Date ): Int =
    sum_of_prices (prices_of_segments (flight.segments, date )  )

  def complies_with_ethical_rule_1 (flight: Flight, date: Date ): Boolean =
    get_price (flight, date ) <= price_of_flight_by_segments (flight, date )

  def complies_with_ethical_rule_2 (flight: Flight, date: Date ): Boolean =
    is_price_increase_acceptable (old_price = get_price (flight, date ), new_price = get_price (flight, get_a_year_before (date ) ) )

  def is_price_increase_acceptable (old_price: Int, new_price: Int ): Boolean =
    new_price <= (old_price * 125 ) / 100

  def get_price (flight: Flight, date: Date ): Int = pricing_agent.get_price (flight, date )
}

trait SingleSegmentFlight  extends Flight {

  lazy val intermediate_stops = Seq ()
}

case class SingleSegmentFlight_ (airport_start: Airport, airport_end: Airport )  extends SingleSegmentFlight
