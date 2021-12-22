package soda.translator.block

case class BlockSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

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
      lazy val expected = Seq (AnnotatedLine_ ("", isComment = false ), AnnotatedLine_ ("", isComment = false ), AnnotatedLine_ ("/** This is an example */", isComment = true ), AnnotatedLine_ ("* Example () {", isComment = false ), AnnotatedLine_ ("  /* This is a comment */", isComment = true ), AnnotatedLine_ ("  a = \"/** this is not a comment */\"", isComment = false ), AnnotatedLine_ ("}", isComment = false )      )
      lazy val obtained = Block_ (input ) .annotated_lines
      assert (obtained == expected ) }

}
