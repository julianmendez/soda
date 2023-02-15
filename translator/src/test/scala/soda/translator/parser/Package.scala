package soda.translator.parser

/*
 * This package contains tests for the Soda parser.
 */

trait Package
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
case class PreprocessorSequenceTranslatorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.block.DefaultBlockTranslator_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_program =
    ("package soda.example.mytest" +
    "\n" +
    "\n/**" +
    "\n * Example class for testing." +
    "\n */" +
    "\n" +
    "\nclass Example" +
    "\n" +
    "\n  import" +
    "\n    soda.lib.Fold_" +
    "\n    soda.lib.Enum" +
    "\n" +
    "\n  abstract" +
    "\n    number: Int" +
    "\n    name: String" +
    "\n" +
    "\n  my_constant: Int = 0" +
    "\n" +
    "\n  my_function (x: Int, y: Int): Int =" +
    "\n    x + y" +
    "\n" +
    "\nend" +
    "\n" +
    "\n")

  lazy val expected_output =
    ("package soda.example.mytest" +
    "\n" +
    "\n/**" +
    "\n * Example class for testing." +
    "\n */" +
    "\n" +
    "\nclass Example" +
    "\n" +
    "\n  import" +
    "\n    soda.lib.Fold_" +
    "\n    soda.lib.Enum" +
    "\n" +
    "\n  abstract" +
    "\n    number: Int" +
    "\n    name: String" +
    "\n" +
    "\n  my_constant: Int = 0" +
    "\n" +
    "\n  my_function (x: Int, y: Int): Int =" +
    "\n    x + y" +
    "\n" +
    "\nend" +
    "\n")

  lazy val block_processor =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        DefaultBlockTranslator_ ()
      )
    )

  test ("should test the preprocessor") (
    check (
      obtained = block_processor.translate (example_program)
    ) (
      expected = expected_output
    )
  )

}
