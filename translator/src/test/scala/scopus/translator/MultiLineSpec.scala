package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import scala.language.implicitConversions


case class MultiLineSpec() extends AnyFunSuite {

  test("should preprocess the comma in multiple lines") {
    val input = "" +
      "  value = 1\n" +
      "  sequence = Seq(1 ,\n" +
      "    2,  \n" +
      "    3)\n" +
      "  f( x: Int,\t\n" +
      "     y: Int,\n" +
      "     z: Int) =" +
      "       x * x + y * y + z * z\n"

    val expected = "" +
      "  value = 1\n" +
      "  sequence = Seq(1 ," +
      "    2,  " +
      "    3)\n" +
      "  f( x: Int,\t" +
      "     y: Int," +
      "     z: Int) =" +
      "       x * x + y * y + z * z\n"

    val mt = MicroTranslator()
    val input_lines = mt.split_lines(input)
    val joined_lines = mt.join_lines_ending_with_comma(input_lines)
    val obtained = mt.join_translated_lines(joined_lines)
    assert (obtained == expected)
  }

}
