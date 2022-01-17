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

case class EnumSpec ()
  extends org.scalatest.funsuite.AnyFunSuite {

  test ("the names of the elements in enumerations")
    {
      lazy val expected = Seq ("DayOfTheWeek_(0,Sunday)", "DayOfTheWeek_(1,Monday)", "DayOfTheWeek_(2,Tuesday)", "DayOfTheWeek_(3,Wednesday)", "DayOfTheWeek_(4,Thursday)", "DayOfTheWeek_(5,Friday)", "DayOfTheWeek_(6,Saturday)")
      lazy val obtained = DayOfTheWeekEnum_ () .values.map (x => x.toString )
      assert (obtained == expected ) }

}
