package soda.translator.parser

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
    "\n    soda.lib.Recursion_" +
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
    "\n    soda.lib.Recursion_" +
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
