package soda.translator.parser

case class BlockSpec ()
  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.block.AnnotatedLine_
  import soda.translator.block.BlockAnnotationEnum_

  /* This is to test how to find commented text. */
  test ("should find commented text")
    {
      lazy val input = ("\n" +
        "\n/** This is an example */" +
        "\n* Example () {" +
        "\n  /* This is a comment */" +
        "\n  a = \"/** this is not a comment */\"" +
        "\n}" +
        "\n")
        .split ("\n")
        .toSeq
      lazy val expected = Seq (
        AnnotatedLine_ ("", is_comment = false ),
        AnnotatedLine_ ("", is_comment = false ),
        AnnotatedLine_ ("/** This is an example */", is_comment = true ),
        AnnotatedLine_ ("* Example () {", is_comment = false ),
        AnnotatedLine_ ("  /* This is a comment */", is_comment = true ),
        AnnotatedLine_ ("  a = \"/** this is not a comment */\"", is_comment = false ),
        AnnotatedLine_ ("}", is_comment = false )
      )
      lazy val obtained = BlockBuilder_ () .build (input, BlockAnnotationEnum_ () .undefined ) .annotated_lines
      assert (obtained == expected ) }

}
