package soda.translator.extension.toscala

case class MultiLineSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.BlockProcessor_

  lazy val bp =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  lazy val mt = MicroTranslatorToScala_ ()

  lazy val original_input =
    "  value = 1\n" +
    "  sequence = Seq(1 ,\n" +
    "    2,  \n" +
    "    3)\n" +
    "  f( x: Int,\t\n" +
    "     y: Int,\n" +
    "     z: Int) =\n" +
    "       x * x + y * y + z * z\n"

  lazy val original_input_lines = Seq (
    "  value = 1",
    "  sequence = Seq(1 ,",
    "    2,  ",
    "    3)",
    "  f( x: Int,\t",
    "     y: Int,",
    "     z: Int) =",
    "       x * x + y * y + z * z")

  lazy val joined_comma_lines = Seq (
    "  value = 1",
    "  sequence = Seq(1 ,    2,      3)",
    "  f( x: Int,\t     y: Int,     z: Int) =",
    "       x * x + y * y + z * z")

  lazy val joined_output =
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
      lazy val obtained = bp.make_block (original_input )
      lazy val expected = build_block (original_input_lines )
      assert (obtained == expected ) }

  test ("should join the translated lines of a program")
    {
      lazy val obtained = build_block (joined_comma_lines )
      lazy val expected = bp.make_block (joined_output )
      assert (obtained == expected ) }

}
