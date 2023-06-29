package soda.translator.parser.annotation

/*
 * This package contains tests for block annotations.
 */

trait Package

case class BlockAnnotationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.Block
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_blocks =
    ExampleProgram_ () .example_blocks

  def detectors (block : Block) : Seq [BlockAnnotationParser] =
    Seq (
      FunctionDefinitionAnnotation_ (block) ,
      ClassBeginningAnnotation_ (block) ,
      ClassEndAnnotation_ (block, Seq [BlockAnnotationParser] () ) ,
      AbstractDeclarationAnnotation_ (block, Seq [BlockAnnotationParser] () ) ,
      ImportDeclarationAnnotation_ (block) ,
      PackageDeclarationAnnotation_ (block) ,
      ClassAliasAnnotation_ (block) ,
      TheoremBlockAnnotation_ (block) ,
      DirectiveBlockAnnotation_ (block) ,
      CommentAnnotation_ (block) ,
      TestDeclarationAnnotation_ (block)
    )

  def apply_detectors (block : Block) : Seq [Boolean] =
    detectors (block) .map ( detector => detector .applies)

  test ("should detect a package declaration") (
    check (
      obtained = apply_detectors (example_blocks (0) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , false , true , false , false , false , false , false)
    )
  )

  test ("should detect a comment") (
    check (
      obtained = apply_detectors (example_blocks (1) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , false , false , false , false , false , true , false)
    )
  )

  test ("should detect a class beginning") (
    check (
      obtained = apply_detectors (example_blocks (2) )
    ) (
      expected = Seq [Boolean] (
        false , true , false , false , false , false , false , false , false , false , false)
    )
  )

  test ("should detect an abstract block declaration") (
    check (
      obtained = apply_detectors (example_blocks (3) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , true , false , false , false , false , false , false , false)
    )
  )

  test ("should detect an import declaration") (
    check (
      obtained = apply_detectors (example_blocks (4) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , true , false , false , false , false , false , false)
    )
  )

  test ("should detect a constant declaration") (
    check (
      obtained = apply_detectors (example_blocks (5) )
    ) (
      expected = Seq [Boolean] (
        true , false , false , false , false , false , false , false , false , false , false)
    )
  )

  test ("should detect a function declaration") (
    check (
      obtained = apply_detectors (example_blocks (6) )
    ) (
      expected = Seq [Boolean] (
        true , false , false , false , false , false , false , false , false , false , false)
    )
  )

  test ("should detect another function declaration") (
    check (
      obtained = apply_detectors (example_blocks (7) )
    ) (
      expected = Seq [Boolean] (
        true , false , false , false , false , false , false , false , false , false , false)
    )
  )

  test ("should detect a test declaration") (
    check (
      obtained = apply_detectors (example_blocks (8) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , false , false , false , false , false , false , true)
    )
  )

  test ("should detect a theorem block") (
    check (
      obtained = apply_detectors (example_blocks (9) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , false , false , false , true , false , false , false)
    )
  )

  test ("should detect a directive block") (
    check (
      obtained = apply_detectors (example_blocks (10) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , false , false , false , false , true , false , false)
    )
  )

  test ("should detect a class end") (
    check (
      obtained = apply_detectors (example_blocks (11) )
    ) (
      expected = Seq [Boolean] (
        false , false , true , false , false , false , false , false , false , false , false)
    )
  )

  test ("should detect a class alias") (
    check (
      obtained = apply_detectors (example_blocks (12) )
    ) (
      expected = Seq [Boolean] (
        false , false , false , false , false , false , true , false , false , false , false)
    )
  )

  test ("should find only 13 blocks") (
    check (
      obtained = example_blocks .length
    ) (
      expected = 13
    )
  )

  test ("should be ordered by the identifier ordinal") (
    check (
      obtained = detectors (example_blocks (0) ) .map ( detector =>
        detector .identifier .ordinal)
    ) (
      expected = Seq [Int] (1 , 2 , 3 , 4 , 5 , 6 , 7 , 8 , 9 , 10 , 11)
    )
  )

}


case class ClassBeginningAnnotationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.Block
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_blocks = ExampleProgram_ () .example_blocks

  lazy val default_block_processor = ExampleProgram_ () .default_block_processor

  lazy val example_0 = example_blocks .apply (2)

  def get_as_block (text : String) : Block =
    default_block_processor
      .make_blocks(
        default_block_processor
          .split_blocks (text)
      ) .head

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
      "class Example [A][ B]" +
      "\n  extends" +
      "\n    SuperExample0" +
      "\n    SuperExample2 [A][B]" +
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
      "class Example [A supertype SubTypeExample][B subtype SuperTypeExample]" +
      "\n  extends" +
      "\n    SuperExample2 [A] [B]" +
      "\n    SuperExample1 [A]" +
      "\n    SuperExample0" +
      "\n"
    )

  test ("should find the right block for ClassBeginningAnnotation") (
    check (
      obtained = example_blocks .map ( block => ClassBeginningAnnotation_ (block) .applies )
    ) (
      expected = Seq [Boolean] (false , false , true , false , false , false , false ,
        false , false , false , false , false , false)
    )
  )

  test ("should extract the class name") (
    check (
      obtained = ClassBeginningAnnotation_ (example_0) .class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 0") (
    check (
      obtained = ClassBeginningAnnotation_ (example_0) .type_parameters_and_bounds
    ) (
      expected = Seq [String] ()
    )
  )

  test ("should extract the type parameters in example 0") (
    check (
      obtained = ClassBeginningAnnotation_ (example_0) .type_parameters
    ) (
      expected = Seq [String] ()
    )
  )

  test ("should extract the class name in example 1") (
    check (
      obtained = ClassBeginningAnnotation_ (example_1) .class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 1") (
    check (
      obtained = ClassBeginningAnnotation_ (example_1) .type_parameters_and_bounds
    ) (
      expected = Seq ("A")
    )
  )

  test ("should extract the type parameters in example 1") (
    check (
      obtained = ClassBeginningAnnotation_ (example_1) .type_parameters
    ) (
      expected = Seq ("A")
    )
  )

  test ("should extract the class name in example 2") (
    check (
      obtained = ClassBeginningAnnotation_ (example_2) .class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 2") (
    check (
      obtained = ClassBeginningAnnotation_ (example_2) .type_parameters_and_bounds
    ) (
      expected = Seq ("A" , "B")
    )
  )

  test ("should extract the type parameters in example 2") (
    check (
      obtained = ClassBeginningAnnotation_ (example_2) .type_parameters
    ) (
      expected = Seq ("A" , "B")
    )
  )

  test ("should extract the class name in example 3") (
    check (
      obtained = ClassBeginningAnnotation_ (example_3) .class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 3") (
    check (
      obtained = ClassBeginningAnnotation_ (example_3) .type_parameters_and_bounds
    ) (
      expected = Seq ("A subtype SuperTypeExample")
    )
  )

  test ("should extract the type parameters in example 3") (
    check (
      obtained = ClassBeginningAnnotation_ (example_3) .type_parameters
    ) (
      expected = Seq ("A")
    )
  )

  test ("should extract the class name in example 4") (
    check (
      obtained = ClassBeginningAnnotation_ (example_4) .class_name
    ) (
      expected = "Example"
    )
  )

  test ("should extract the type parameters and bounds in example 4") (
    check (
      obtained = ClassBeginningAnnotation_ (example_4) .type_parameters_and_bounds
    ) (
      expected = Seq ("A supertype SubTypeExample" , "B subtype SuperTypeExample")
    )
  )

  test ("should extract the type parameters in example 4") (
    check (
      obtained = ClassBeginningAnnotation_ (example_4) .type_parameters
    ) (
      expected = Seq ("A" , "B")
    )
  )

}


trait ExampleProgram
{

  import   soda.translator.block.Block
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor
  import   soda.translator.parser.BlockProcessor_

  lazy val example_program =
    ("package soda.example.mytest" +
    "\n" +
    "\n/**" +
    "\n * Example class for testing." +
    "\n */" +
    "\n" +
    "\nclass Example" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n" +
    "\n  abstract" +
    "\n    value : Int" +
    "\n" +
    "\n  import" +
    "\n    soda.lib.Fold_" +
    "\n    soda.lib.Enum" +
    "\n" +
    "\n  my_constant : Int = 0" +
    "\n" +
    "\n  my_function (x : Int) (y : Int) : Int =" +
    "\n    x + y" +
    "\n" +
    "\n  another_function" +
    "\n    (x : Int)" +
    "\n    (y : Int)" +
    "\n      : Int =" +
    "\n    x + y" +
    "\n" +
    "\n  test (\"should test the example\")" +
    "\n    check (" +
    "\n      obtained := ClassBeginningAnnotation_ (example_4).type_parameters" +
    "\n    ) (" +
    "\n      expected := Seq (\"A\", \"B\")" +
    "\n    )" +
    "\n" +
    "\n  theorem" +
    "\n    True = True" +
    "\n  ." +
    "\n  Proof." +
    "\n    auto." +
    "\n  Qed" +
    "\n" +
    "\n  directive" +
    "\n    theorem" +
    "\n      value = value" +
    "\n      := by auto" +
    "\n" +
    "\nend" +
    "\n" +
    "\nclass AnotherExample = Example" +
    "\n" +
    "\n")

  lazy val default_block_processor : BlockProcessor =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        DefaultBlockTranslator_ ()
      )
    )

  lazy val example_blocks : Seq [Block] =
    default_block_processor .make_blocks (
      default_block_processor .split_blocks (example_program)
    )

}

case class ExampleProgram_ () extends ExampleProgram

