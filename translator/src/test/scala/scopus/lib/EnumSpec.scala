package scopus.lib

import org.scalatest.funsuite.AnyFunSuite


case class DayOfTheWeek (ordinal: Int, name: String) extends EnumConstant

case class DayOfTheWeekEnum () {

  lazy val Sunday = DayOfTheWeek(0, "Sunday")
  lazy val Monday = DayOfTheWeek(1, "Monday")
  lazy val Tuesday = DayOfTheWeek(2, "Tuesday")
  lazy val Wednesday = DayOfTheWeek(3, "Wednesday")
  lazy val Thursday = DayOfTheWeek(4, "Thursday")
  lazy val Friday = DayOfTheWeek(5, "Friday")
  lazy val Saturday = DayOfTheWeek(6, "Saturday")

  lazy val values = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

}

case class EnumSpec() extends AnyFunSuite {

  test("the names of the elements in enumerations") {
    lazy val expected = Seq("0-Sunday", "1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", "5-Friday", "6-Saturday")
    lazy val obtained = DayOfTheWeekEnum().values.map(x => x.toString)
    assert(obtained == expected)
  }

}
