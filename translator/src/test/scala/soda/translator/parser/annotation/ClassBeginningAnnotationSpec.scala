package soda.translator.parser.annotation

case class ClassBeginningAnnotationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.Block
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_blocks = ExampleProgram_ ().example_blocks

  lazy val default_block_processor = ExampleProgram_ ().default_block_processor

  lazy val example_0 = example_blocks.apply (2)

  def get_as_block (text: String): Block =
    default_block_processor
      .split_blocks (text)
      .head

  lazy val example_1 =
    get_as_block (
      "class Example [A]" +
      "\n  extends" +
      "\n    SuperExample0" +
      "\n    SuperExample1 [A]" +
      "\n"
    )

  lazy val example_2 =
    get_as_block (
      "class Example [A, B]" +
      "\n  extends" +
      "\n    SuperExample0" +
      "\n    SuperExample2 [A, B]" +
      "\n    SuperExample1 [A]" +
      "\n"
    )

  lazy val example_3 =
    get_as_block (
      "class Example [A subtype SuperTypeExample]" +
      "\n  extends" +
      "\n    SuperExample0" +
      "\n    SuperExample1 [A]" +
      "\n"
    )

  lazy val example_4 =
    get_as_block (
      "class Example [A supertype SubTypeExample, B subtype SuperTypeExample]" +
      "\n  extends" +
      "\n    SuperExample2 [A, B]" +
      "\n    SuperExample1 [A]" +
      "\n    SuperExample0" +
      "\n"
    )

  test ("should find the right block for ClassBeginningAnnotation") (
    check (
      obtained = example_blocks.map (  block => ClassBeginningAnnotation_ (block).applies )
    ) (
      expected = Seq [Boolean] (false, false, true, false, false, false, false, false, false, false, false, false)
    )
  )

  test ("should extract the class name") (
    check (
      obtained = ClassBeginningAnnotation_ (example_0).class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 0") (
    check (
      obtained = ClassBeginningAnnotation_ (example_0).type_parameters_and_bounds
    ) (
      expected = Seq [String] ()
    )
  )

  test ("should extract the type parameters in example 0") (
    check (
      obtained = ClassBeginningAnnotation_ (example_0).type_parameters
    ) (
      expected = Seq [String] ()
    )
  )

  test ("should extract the class name in example 1") (
    check (
      obtained = ClassBeginningAnnotation_ (example_1).class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 1") (
    check (
      obtained = ClassBeginningAnnotation_ (example_1).type_parameters_and_bounds
    ) (
      expected = Seq ("A")
    )
  )

  test ("should extract the type parameters in example 1") (
    check (
      obtained = ClassBeginningAnnotation_ (example_1).type_parameters
    ) (
      expected = Seq ("A")
    )
  )

  test ("should extract the class name in example 2") (
    check (
      obtained = ClassBeginningAnnotation_ (example_2).class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 2") (
    check (
      obtained = ClassBeginningAnnotation_ (example_2).type_parameters_and_bounds
    ) (
      expected = Seq ("A", "B")
    )
  )

  test ("should extract the type parameters in example 2") (
    check (
      obtained = ClassBeginningAnnotation_ (example_2).type_parameters
    ) (
      expected = Seq ("A", "B")
    )
  )

  test ("should extract the class name in example 3") (
    check (
      obtained = ClassBeginningAnnotation_ (example_3).class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 3") (
    check (
      obtained = ClassBeginningAnnotation_ (example_3).type_parameters_and_bounds
    ) (
      expected = Seq ("A subtype SuperTypeExample")
    )
  )

  test ("should extract the type parameters in example 3") (
    check (
      obtained = ClassBeginningAnnotation_ (example_3).type_parameters
    ) (
      expected = Seq ("A")
    )
  )

  test ("should extract the class name in example 4") (
    check (
      obtained = ClassBeginningAnnotation_ (example_4).class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 4") (
    check (
      obtained = ClassBeginningAnnotation_ (example_4).type_parameters_and_bounds
    ) (
      expected = Seq ("A supertype SubTypeExample", "B subtype SuperTypeExample")
    )
  )

  test ("should extract the type parameters in example 4") (
    check (
      obtained = ClassBeginningAnnotation_ (example_4).type_parameters
    ) (
      expected = Seq ("A", "B")
    )
  )

}
