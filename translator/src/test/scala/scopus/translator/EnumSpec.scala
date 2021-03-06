package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions


case class DayOfTheWeek (index: Int, name: String) extends Enum

case class DayOfTheWeekCons () {

  val Sunday = DayOfTheWeek(0, "Sunday")
  val Monday = DayOfTheWeek(1, "Monday")
  val Tuesday = DayOfTheWeek(2, "Tuesday")
  val Wednesday = DayOfTheWeek(3, "Wednesday")
  val Thursday = DayOfTheWeek(4, "Thursday")
  val Friday = DayOfTheWeek(5, "Friday")
  val Saturday = DayOfTheWeek(6, "Saturday")

  val All = Seq(Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday)

}

case class EnumSpec() extends AnyFunSuite {

  test("the names of the elements in enumerations") {
    val expected = Seq("0-Sunday", "1-Monday", "2-Tuesday", "3-Wednesday", "4-Thursday", "5-Friday", "6-Saturday")
    val obtained = DayOfTheWeekCons().All.map(x => x.toString)
    assert(obtained == expected)
  }

}