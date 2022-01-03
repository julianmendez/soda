package soda.translator.extension.toscala

case class MultiLineSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.block.DefaultBlockTranslator_
  import soda.translator.blocktr.LineForwardJoinerBlockTranslator_
  import soda.translator.parser.BlockBuilder_
  import soda.translator.parser.BlockProcessor_

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

  def build_block (lines: Seq [String]  ): AnnotatedBlock =
    BlockBuilder_ () .build (lines, BlockAnnotationEnum_ () .undefined )

  test ("should split a program in multiple lines")
    {
      lazy val obtained = bp.make_block (Original_input )
      lazy val expected = build_block (Original_input_lines )
      assert (obtained == expected ) }

  test ("should preprocess the comma in multiple lines")
    {
      lazy val obtained = LineForwardJoinerBlockTranslator_ () .translate (build_block (Original_input_lines ) )
      lazy val expected = build_block (Joined_comma_lines )
      assert (obtained == expected ) }

  test ("should join the translated lines of a program")
    {
      lazy val obtained = build_block (Joined_comma_lines )
      lazy val expected = bp.make_block (Joined_output )
      assert (obtained == expected ) }

}
