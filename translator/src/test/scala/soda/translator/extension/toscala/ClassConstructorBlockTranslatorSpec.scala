package soda.translator.extension.toscala

case class ClassConstructorBlockTranslatorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  lazy val example_program =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example [A, B subtype SuperType]" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n    AnotherSuperClassExample [A]" +
    "\n" +
    "\n  abstract" +
    "\n    value: Int" +
    "\n    a_function: Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\nclass ExampleSpec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n" +
    "\n"

  lazy val expected_program =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example [A, B subtype SuperType]" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n    AnotherSuperClassExample [A]" +
    "\n" +
    "\n  abstract" +
    "\n    value: Int" +
    "\n    a_function: Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\ncase class Example_ [A, B <: SuperType] (value: Int, a_function: Int => Double => String) extends Example [A, B]" +
    "\n" +
    "\nclass ExampleSpec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n"

  lazy val translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        ClassConstructorBlockTranslator_ ()
      )
    )

  test ("should produce the constructors")
    {
      lazy val obtained = translator.translate (example_program )
      lazy val expected = expected_program
      assert (obtained == expected ) }

}
