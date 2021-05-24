package soda.translator.language


case class MultiLineSpec () extends org.scalatest.funsuite.AnyFunSuite {

  lazy val mt = MicroTranslator ()

  lazy val Original_input = "" +
    "  value = 1\n" +
    "  sequence = Seq(1 ,\n" +
    "    2,  \n" +
    "    3)\n" +
    "  f( x: Int,\t\n" +
    "     y: Int,\n" +
    "     z: Int) =\n" +
    "       x * x + y * y + z * z\n"

  lazy val Original_input_lines = Seq ("  value = 1", "  sequence = Seq(1 ,", "    2,  ", "    3)", "  f( x: Int,\t", "     y: Int,", "     z: Int) =", "       x * x + y * y + z * z")

  lazy val Joined_comma_lines = Seq ("  value = 1", "  sequence = Seq(1 ,    2,      3)", "  f( x: Int,\t     y: Int,     z: Int) =", "       x * x + y * y + z * z")

  lazy val Joined_output = "" +
    "  value = 1\n" +
    "  sequence = Seq(1 ," +
    "    2,  " +
    "    3)\n" +
    "  f( x: Int,\t" +
    "     y: Int," +
    "     z: Int) =\n" +
    "       x * x + y * y + z * z\n"


  test ("should split a program in multiple lines") {
    lazy val obtained = mt.split_lines (Original_input )
    lazy val expected = Original_input_lines
    assert (obtained == expected )
  }

  test ("should preprocess the comma in multiple lines") {
    lazy val obtained = mt.join_lines_ending_with_comma_or_opening_parenthesis (Original_input_lines )
    lazy val expected = Joined_comma_lines
    assert (obtained == expected )
  }

  test ("should join the translated lines of a program") {
    lazy val obtained = mt.join_translated_lines (Joined_comma_lines )
    lazy val expected = Joined_output
    assert (obtained == expected )
  }
}
