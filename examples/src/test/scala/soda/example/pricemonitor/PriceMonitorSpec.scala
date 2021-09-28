package soda.example.pricemonitor


trait UnfairPricingAgent  extends PricingAgent {

  def get_price (customer: Customer, flight: Flight, date: Int ): Int =
    customer.name.length * (date % 100 + 100 * flight.intermediate_airports.length + 1 )
}

case class UnfairPricingAgent_ ()  extends UnfairPricingAgent

trait FairPricingAgent  extends PricingAgent {

  def get_price (customer: Customer, flight: Flight, date: Int ): Int =
    100 * (flight.intermediate_airports.length + 1 )
}

case class FairPricingAgent_ ()  extends FairPricingAgent

case class PriceMonitorSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val fair_pricing_agent = FairPricingAgent_ ()

  lazy val unfair_pricing_agent = UnfairPricingAgent_ ()

  lazy val customer_1 = Customer_ (name = "Jon", ip_address = "127.0.0.1")
  lazy val customer_2 = Customer_ (name = "Maria", ip_address = "192.168.1.1")

  lazy val flight_1 = Flight_ ("BER", Seq ("FRA", "ARN"), "UMU")
  lazy val date_1 = 18898

  test ("Test unfair pricing agent - principle 1") {
    lazy val principle = Principle1_ (unfair_pricing_agent )
    lazy val obtained = principle.complies (customer_1, customer_2, flight_1, date_1 )
    lazy val expected = Report1_ (false, 897, 1495, 0.6 )

    assert (obtained == expected )
  }

  test ("Test unfair pricing agent - principle 2") {
    lazy val principle = Principle2_ (unfair_pricing_agent )
    lazy val obtained = principle.complies (customer_1, flight_1, date_1 )
    lazy val expected = Report2_ (false, 702, 897 )

    assert (obtained == expected )
  }

  test ("Test unfair pricing agent - principle 3") {
    lazy val principle = Principle3_ (unfair_pricing_agent )
    lazy val obtained = principle.complies (customer_1, flight_1, date_1 )
    lazy val expected = Report3_ (false, 897, 891 )

    assert (obtained == expected )
  }

  test ("Test fair pricing agent - principle 1") {
    lazy val principle = Principle1_ (fair_pricing_agent )
    lazy val obtained = principle.complies (customer_1, customer_2, flight_1, date_1 )
    lazy val expected = Report1_ (true, 300, 300, 1.0 )

    assert (obtained == expected )
  }

  test ("Test fair pricing agent - principle 2") {
    lazy val principle = Principle2_ (fair_pricing_agent )
    lazy val obtained = principle.complies (customer_1, flight_1, date_1 )
    lazy val expected = Report2_ (true, 300, 300 )

    assert (obtained == expected )
  }

  test ("Test fair pricing agent - principle 3") {
    lazy val principle = Principle3_ (fair_pricing_agent )
    lazy val obtained = principle.complies (customer_1, flight_1, date_1 )
    lazy val expected = Report3_ (true, 300, 300 )

    assert (obtained == expected )
  }
}
