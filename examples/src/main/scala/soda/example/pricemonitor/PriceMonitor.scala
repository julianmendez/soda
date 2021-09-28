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
