trait Customer
{

  def   name : String
  def   ip_address : String

}

case class Customer_ (name : String, ip_address : String) extends Customer

object Customer { def mk  (name : String) (ip_address : String) : Customer  = Customer_  (name, ip_address) }

trait Flight
{

  def   start_airport : String
  def   intermediate_airports : Seq [String]
  def   end_airport : String

}

case class Flight_ (start_airport : String, intermediate_airports : Seq [String], end_airport : String) extends Flight

object Flight { def mk  (start_airport : String) (intermediate_airports : Seq [String]) (end_airport : String) : Flight  = Flight_  (start_airport, intermediate_airports, end_airport) }

trait PricingAgent
{

  import   java.util.Date

    /** get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int */
  def   abs_get_price : Customer => Flight => Int => Int

  def get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    abs_get_price (customer) (flight) (date_in_days)

  lazy val milliseconds_per_day : Long = 24 * 60 * 60 * 1000

  def get_days_for (date : Date) : Int =
    (date .getTime / milliseconds_per_day) .toInt

}

case class PricingAgent_ (abs_get_price : Customer => Flight => Int => Int) extends PricingAgent

object PricingAgent { def mk  (abs_get_price : Customer => Flight => Int => Int) : PricingAgent  = PricingAgent_  (abs_get_price) }

trait RequirementMonitor
{

  def   pricing_agent : PricingAgent

  def get_price (customer : Customer) (flight : Flight) (date_in_days : Int) : Int =
    pricing_agent .get_price (customer) (flight) (date_in_days)

}

case class RequirementMonitor_ (pricing_agent : PricingAgent) extends RequirementMonitor

object RequirementMonitor { def mk  (pricing_agent : PricingAgent) : RequirementMonitor  = RequirementMonitor_  (pricing_agent) }
