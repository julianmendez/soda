package soda.lib


trait DayOfTheWeek extends EnumConstant

case class DayOfTheWeek_ (ordinal: Int, name: String ) extends DayOfTheWeek

trait DayOfTheWeekConstant {

  lazy val sunday = DayOfTheWeek_ (0, "Sunday")
  lazy val monday = DayOfTheWeek_ (1, "Monday")
  lazy val tuesday = DayOfTheWeek_ (2, "Tuesday")
  lazy val wednesday = DayOfTheWeek_ (3, "Wednesday")
  lazy val thursday = DayOfTheWeek_ (4, "Thursday")
  lazy val friday = DayOfTheWeek_ (5, "Friday")
  lazy val saturday = DayOfTheWeek_ (6, "Saturday")

  lazy val DayOfTheWeek_values = Seq (sunday, monday, tuesday, wednesday, thursday, friday, saturday )
}

trait DayOfTheWeekEnum extends DayOfTheWeekConstant {

  lazy val values = DayOfTheWeek_values
}

case class DayOfTheWeekEnum_ () extends DayOfTheWeekEnum

trait EnumSpec  extends org.scalatest.funsuite.AnyFunSuite {

  test ("the names of the elements in enumerations") {
    lazy val expected = Seq ("0-Sunday", "1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", "5-Friday", "6-Saturday")
    lazy val obtained = DayOfTheWeekEnum_ () .values.map (x => x.toString )

    assert (obtained == expected )
  }
}

case class EnumSpec_ () extends EnumSpec
