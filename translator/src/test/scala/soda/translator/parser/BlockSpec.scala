package soda.translator.parser

/*
 * This package contains tests for the Soda parser.
 */



case class BlockSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.AnnotatedLine_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val input =  ("\n" +
    "\n/** This is an example */" +
    "\n* Example () {" +
    "\n  /* This is a comment */" +
    "\n  a = \"/** this is not a comment */\"" +
    "\n}" +
    "\n")
    .split ("\n")
    .toSeq

  /* This is to test how to find commented text. */
  test ("should find commented text") (
    check (
      obtained = BlockBuilder_ ().build (input).annotated_lines
    ) (
      expected = Seq (
        AnnotatedLine_ ("", is_comment = false),
        AnnotatedLine_ ("", is_comment = false),
        AnnotatedLine_ ("/** This is an example */", is_comment = true),
        AnnotatedLine_ ("* Example () {", is_comment = false),
        AnnotatedLine_ ("  /* This is a comment */", is_comment = true),
        AnnotatedLine_ ("  a = \"/** this is not a comment */\"", is_comment = false),
        AnnotatedLine_ ("}", is_comment = false)
      )
    )
  )

}
