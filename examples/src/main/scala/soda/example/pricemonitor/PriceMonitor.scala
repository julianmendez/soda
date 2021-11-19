package soda.example.pricemonitor


trait Customer {

  def name: String

  def ip_address: String

}

case class Customer_ (name: String, ip_address: String )  extends Customer

trait PricingAgent {
  import java.util.Date

  def get_price (customer: Customer, flight: Flight, date_in_days: Int ): Int

  lazy val milliseconds_per_day: Long = 24 * 60 * 60 * 1000

  def get_days_for (date: Date ): Int =
    (date.getTime / milliseconds_per_day ) .toInt

}

trait Flight {

  def start_airport: String

  def intermediate_airports: Seq [String]

  def end_airport: String

}

case class Flight_ (start_airport: String, intermediate_airports: Seq [String], end_airport: String )  extends Flight

trait RequirementMonitor {

  def pricing_agent: PricingAgent

  def get_price (customer: Customer, flight: Flight, date_in_days: Int ): Int =
    pricing_agent.get_price (customer, flight, date_in_days )

}
