package soda.translator.extension.toscala

case class ClassConstructorBlockTranslatorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  lazy val example_program_0 =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example0" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n" +
    "\n  abstract" +
    "\n    /** this value is an example */" +
    "\n    value: Int" +
    "\n    /** this function is also an example */" +
    "\n    a_function: Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\nclass Example0Spec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n" +
    "\n"

  lazy val expected_program_0 =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example0" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n" +
    "\n  abstract" +
    "\n    /** this value is an example */" +
    "\n    value: Int" +
    "\n    /** this function is also an example */" +
    "\n    a_function: Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\ncase class Example0_ (value: Int, a_function: Int => Double => String) extends Example0" +
    "\n" +
    "\nclass Example0Spec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n"

  lazy val example_program_1 =
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

  lazy val expected_program_1 =
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

  test ("should produce the constructors for a simple class")
    {
      lazy val obtained = translator.translate (example_program_0)
      lazy val expected = expected_program_0
     assert (obtained == expected) }

  test ("should produce the constructors for a class with type parameters")
    {
      lazy val obtained = translator.translate (example_program_1)
      lazy val expected = expected_program_1
     assert (obtained == expected) }

}
