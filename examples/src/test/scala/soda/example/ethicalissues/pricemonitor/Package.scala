package soda.example.ethicalissues.pricemonitor

/*
 * This package contains tests for example classes for a price monitor.
 */

trait UnfairPricingAgent
  extends
    PricingAgent
{



  def get_price_for (customer : Customer) (flight : Flight) (date : Int) : Int =
    customer .name .length * (date % 100 + 100 * flight .intermediate_airports .length + 1)

  lazy val abs_get_price : Customer => Flight => Int => Int =
     customer =>
       flight =>
         date =>
          get_price_for (customer) (flight) (date)

}

case class UnfairPricingAgent_ () extends UnfairPricingAgent

object UnfairPricingAgent {
  def mk : UnfairPricingAgent =
    UnfairPricingAgent_ ()
}

trait FairPricingAgent
  extends
    PricingAgent
{



  def get_price_for (customer : Customer) (flight : Flight) (date : Int) : Int =
    100 * (flight .intermediate_airports .length + 1)

  lazy val abs_get_price : Customer => Flight => Int => Int =
     customer =>
       flight =>
         date =>
          get_price_for (customer) (flight) (date)

}

case class FairPricingAgent_ () extends FairPricingAgent

object FairPricingAgent {
  def mk : FairPricingAgent =
    FairPricingAgent_ ()
}

case class PriceMonitorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   java.util.Calendar
  import   java.util.TimeZone

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val fair_pricing_agent = FairPricingAgent .mk

  lazy val unfair_pricing_agent = UnfairPricingAgent .mk

  lazy val customer_1 = Customer .mk (name = "Jon") (ip_address = "127.0.0.1")

  lazy val customer_2 = Customer .mk (name = "Maria") (ip_address = "192.168.1.1")

  lazy val flight_1 = Flight .mk ("BER") (Seq ("FRA" , "ARN") ) ("UMU")

  lazy val date_1 = 18898

  test ("unfair pricing agent - requirement_monitor 1") (
    check (
      obtained =
        Requirement1Monitor .mk (unfair_pricing_agent)
          .get_report (customer_1) (customer_2) (flight_1) (date_1)
    ) (
      expected = Report1 .mk (false) (897) (1495) (0.6)
    )
  )

  test ("unfair pricing agent - requirement_monitor 2") (
    check (
      obtained =
        Requirement2Monitor .mk (unfair_pricing_agent)
          .get_report (customer_1) (flight_1) (date_1)
    ) (
      expected = Report2 .mk (false) (702) (897)
    )
  )

  test ("unfair pricing agent - requirement_monitor 3") (
    check (
      obtained =
        Requirement3Monitor .mk (unfair_pricing_agent)
          .get_report (customer_1) (flight_1) (date_1)
    ) (
      expected = Report3 .mk (false) (897) (891)
    )
  )

  test ("fair pricing agent - requirement_monitor 1") (
    check (
      obtained =
        Requirement1Monitor .mk (fair_pricing_agent)
          .get_report (customer_1) (customer_2) (flight_1) (date_1)
    ) (
      expected = Report1 .mk (true) (300) (300) (1.0)
    )
  )

  test ("fair pricing agent - requirement_monitor 2") (
    check (
      obtained =
        Requirement2Monitor .mk (fair_pricing_agent) .get_report (customer_1) (flight_1) (date_1)
    ) (
      expected = Report2 .mk (true) (300) (300)
    )
  )

  test ("fair pricing agent - requirement_monitor 3") (
    check (
      obtained =
        Requirement3Monitor .mk (fair_pricing_agent) .get_report (customer_1) (flight_1) (date_1)
    ) (
      expected = Report3 .mk (true) (300) (300)
    )
  )

  private lazy val _calendar_0 = new Calendar .Builder ()
    .setTimeZone (TimeZone .getTimeZone ("UTC"))
    .setDate (1970 , 0 , 1)
    .build

  test ("get number of days for 1970-01-01") (
    check (
      obtained = fair_pricing_agent .get_days_for (_calendar_0 .getTime)
    ) (
      expected = 0
    )
  )

  private lazy val _calendar_1 = new Calendar .Builder ()
    .setTimeZone (TimeZone .getTimeZone ("UTC"))
    .setDate (2021 , 8 , 28)
    .build

  test ("get number of days for 2021-09-28") (
    check (
      obtained = fair_pricing_agent .get_days_for (_calendar_1 .getTime)
    ) (
      expected = 18898
    )
  )

}

