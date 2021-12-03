package soda.translator.extension.toscala

case class MultiLineSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.block.Block_
  import soda.translator.block.BlockProcessor_
  import soda.translator.block.DefaultBlockTranslator_

  lazy val bp = BlockProcessor_ (DefaultBlockTranslator_ ()  )

  lazy val mt = MicroTranslatorToScala_ ()

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
    "       x * x + y * y + z * z"


  test ("should split a program in multiple lines")
    {
      lazy val obtained = bp.make_block (Original_input )
      lazy val expected = Block_ (Original_input_lines )
      assert (obtained == expected ) }

  test ("should preprocess the comma in multiple lines")
    {
      lazy val obtained = mt.join_lines_with_forward_join (Block_ (Original_input_lines ) )
      lazy val expected = Block_ (Joined_comma_lines )
      assert (obtained == expected ) }

  test ("should join the translated lines of a program")
    {
      lazy val obtained = Block_ (Joined_comma_lines )
      lazy val expected = bp.make_block (Joined_output )
      assert (obtained == expected ) }

}
