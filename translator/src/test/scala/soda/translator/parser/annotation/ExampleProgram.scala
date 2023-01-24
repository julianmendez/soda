package soda.translator.parser.annotation

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
    "\n    soda.lib.Recursion_" +
    "\n    soda.lib.Enum" +
    "\n" +
    "\n  my_constant : Int = 0" +
    "\n" +
    "\n  my_function (x : Int) (y : Int) : Int =" +
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
    "\n    True" +
    "\n" +
    "\n  proof" +
    "\n    auto." +
    "\n" +
    "\nend" +
    "\n" +
    "\nclass AnotherExample = Example" +
    "\n" +
    "\n")

  lazy val default_block_processor : BlockProcessor =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        DefaultBlockTranslator_ ()
      )
    )

  lazy val example_blocks : Seq [Block] =
    default_block_processor.split_blocks (example_program)

}

case class ExampleProgram_ () extends ExampleProgram
