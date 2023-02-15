package soda.lib

/*
 * This package contains tests for the Soda library.
 */



trait DayOfTheWeek
  extends
    EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class DayOfTheWeek_ (ordinal : Int, name : String) extends DayOfTheWeek

trait DayOfTheWeekConstant
{

  lazy val sunday = DayOfTheWeek_ (0, "Sunday")

  lazy val monday = DayOfTheWeek_ (1, "Monday")

  lazy val tuesday = DayOfTheWeek_ (2, "Tuesday")

  lazy val wednesday = DayOfTheWeek_ (3, "Wednesday")

  lazy val thursday = DayOfTheWeek_ (4, "Thursday")

  lazy val friday = DayOfTheWeek_ (5, "Friday")

  lazy val saturday = DayOfTheWeek_ (6, "Saturday")

  lazy val DayOfTheWeek_values = Seq (sunday, monday, tuesday, wednesday, thursday, friday, saturday)

}

case class DayOfTheWeekConstant_ () extends DayOfTheWeekConstant

trait DayOfTheWeekEnum
  extends
    DayOfTheWeekConstant
{

  lazy val values = DayOfTheWeek_values

}

case class DayOfTheWeekEnum_ () extends DayOfTheWeekEnum

case class EnumSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("the names of the elements in enumerations") (
    check (
      obtained = DayOfTheWeekEnum_ ().values.map (  x => x.toString)
    ) (
      expected = Seq ("DayOfTheWeek_(0,Sunday)", "DayOfTheWeek_(1,Monday)", "DayOfTheWeek_(2,Tuesday)", "DayOfTheWeek_(3,Wednesday)", "DayOfTheWeek_(4,Thursday)", "DayOfTheWeek_(5,Friday)", "DayOfTheWeek_(6,Saturday)")
    )
  )

}
