package soda.example.ethicalissues.pricemonitor

trait UnfairPricingAgent
  extends
    PricingAgent
{

  lazy val abs_get_price: Customer => Flight => Int => Int =
     customer =>
       flight =>
         date =>
          get_price_for (customer, flight, date )

  def get_price_for (customer: Customer, flight: Flight, date: Int ): Int =
    customer.name.length * (date % 100 + 100 * flight.intermediate_airports.length + 1 )

}

case class UnfairPricingAgent_ ()
  extends
    UnfairPricingAgent
{

}

trait FairPricingAgent
  extends
    PricingAgent
{

  lazy val abs_get_price: Customer => Flight => Int => Int =
     customer =>
       flight =>
         date =>
          get_price_for (customer, flight, date )

  def get_price_for (customer: Customer, flight: Flight, date: Int ): Int =
    100 * (flight.intermediate_airports.length + 1 )

}

case class FairPricingAgent_ ()
  extends
    FairPricingAgent
{

}

case class PriceMonitorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   java.util.Calendar
  import   java.util.TimeZone

  lazy val fair_pricing_agent = FairPricingAgent_ ()

  lazy val unfair_pricing_agent = UnfairPricingAgent_ ()

  lazy val customer_1 = Customer_ (name = "Jon", ip_address = "127.0.0.1")

  lazy val customer_2 = Customer_ (name = "Maria", ip_address = "192.168.1.1")

  lazy val flight_1 = Flight_ ("BER", Seq ("FRA", "ARN"), "UMU")

  lazy val date_1 = 18898

  test ("unfair pricing agent - requirement_monitor 1")
    {
      lazy val requirement_monitor = Requirement1Monitor_ (unfair_pricing_agent )
      lazy val obtained = requirement_monitor.get_report (customer_1, customer_2, flight_1, date_1 )
      lazy val expected = Report1_ (false, 897, 1495, 0.6 )
      assert (obtained == expected ) }

  test ("unfair pricing agent - requirement_monitor 2")
    {
      lazy val requirement_monitor = Requirement2Monitor_ (unfair_pricing_agent )
      lazy val obtained = requirement_monitor.get_report (customer_1, flight_1, date_1 )
      lazy val expected = Report2_ (false, 702, 897 )
      assert (obtained == expected ) }

  test ("unfair pricing agent - requirement_monitor 3")
    {
      lazy val requirement_monitor = Requirement3Monitor_ (unfair_pricing_agent )
      lazy val obtained = requirement_monitor.get_report (customer_1, flight_1, date_1 )
      lazy val expected = Report3_ (false, 897, 891 )
      assert (obtained == expected ) }

  test ("fair pricing agent - requirement_monitor 1")
    {
      lazy val requirement_monitor = Requirement1Monitor_ (fair_pricing_agent )
      lazy val obtained = requirement_monitor.get_report (customer_1, customer_2, flight_1, date_1 )
      lazy val expected = Report1_ (true, 300, 300, 1.0 )
      assert (obtained == expected ) }

  test ("fair pricing agent - requirement_monitor 2")
    {
      lazy val requirement_monitor = Requirement2Monitor_ (fair_pricing_agent )
      lazy val obtained = requirement_monitor.get_report (customer_1, flight_1, date_1 )
      lazy val expected = Report2_ (true, 300, 300 )
      assert (obtained == expected ) }

  test ("fair pricing agent - requirement_monitor 3")
    {
      lazy val requirement_monitor = Requirement3Monitor_ (fair_pricing_agent )
      lazy val obtained = requirement_monitor.get_report (customer_1, flight_1, date_1 )
      lazy val expected = Report3_ (true, 300, 300 )
      assert (obtained == expected ) }

  test ("get number of days for 1970-01-01")
    {
      lazy val calendar = new Calendar.Builder ()
        .setTimeZone (TimeZone.getTimeZone ("UTC")  )
        .setDate (1970, 0, 1 )
        .build
      lazy val date = calendar.getTime
      lazy val obtained = fair_pricing_agent.get_days_for (date )
      lazy val expected = 0
      assert (obtained == expected ) }

  test ("get number of days for 2021-09-28")
    {
      lazy val calendar = new Calendar.Builder ()
        .setTimeZone (TimeZone.getTimeZone ("UTC")  )
        .setDate (2021, 8, 28 )
        .build
      lazy val date = calendar.getTime
      lazy val obtained = fair_pricing_agent.get_days_for (date )
      lazy val expected = 18898
      assert (obtained == expected ) }

}
