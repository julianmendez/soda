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

trait Flight  extends Trip {

  def segments: Seq [Segment]

  lazy val invariant_Flight =
    has_at_least_one_segment &&
    are_airport_start_and_airport_end_correct &&
    are_segments_consistent

  lazy val has_at_least_one_segment = segments.length > 0

  lazy val are_airport_start_and_airport_end_correct =
    has_at_least_one_segment &&
    (airport_start == segments (0 ) .airport_start ) &&
    (airport_end == segments (segments.length ) .airport_end )

  lazy val are_segments_consistent =
    segments.indices.forall (index =>
      (index > segments.length - 2 ) || (segments (index ) .airport_end == segments (index + 1 ) .airport_start ) )
}

trait MonitoringAgent {
  import java.util.Date

  def pricing_agent: PricingAgent

  def get_a_year_before (date: Date ): Date

  def as_flight (segment: Segment ): Flight

  def complies_with_ethical_rule_1 (flight: Flight, date: Date ): Boolean =
    get_price (flight, date ) <= flight.segments.map (segment => get_price (as_flight (segment ), date ) ) .sum

  def complies_with_ethical_rule_2 (flight: Flight, date: Date ): Boolean =
    is_price_increase_acceptable (old_price = get_price (flight, date ), new_price = get_price (flight, get_a_year_before (date ) ) )

  def is_price_increase_acceptable (old_price: Int, new_price: Int ): Boolean =
    new_price <= (old_price * 125 ) / 100

  def get_price (flight: Flight, date: Date ): Int = pricing_agent.get_price (flight, date )
}

trait SingleSegmentFlight  extends Flight {

  def segment: Segment

  lazy val airport_start = segment.airport_start

  lazy val airport_end = segment.airport_end

  lazy val segments = Seq (segment )

  lazy val invariant_SingleSegmentFlight = invariant_Flight
}

case class SingleSegmentFlight_ (segment: Segment )  extends SingleSegmentFlight
