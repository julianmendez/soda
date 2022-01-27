package soda.translator.block

case class BlockAnnotationId (ordinal: Int, name: String )
  extends
    soda.lib.EnumConstant
{

}

trait BlockAnnotationEnum
  extends
    soda.lib.Enum [BlockAnnotationId]
{

  lazy val undefined = BlockAnnotationId (0, "undefined")

  lazy val function_definition = BlockAnnotationId (1, "function_definition")

  lazy val class_beginning = BlockAnnotationId (2, "class_beginning")

  lazy val class_end = BlockAnnotationId (3, "class_end")

  lazy val class_declaration = BlockAnnotationId (4, "class_declaration")

  lazy val abstract_block_declaration = BlockAnnotationId (5, "abstract_block_declaration")

  lazy val abstract_function_declaration = BlockAnnotationId (6, "abstract_function_declaration")

  lazy val import_declaration = BlockAnnotationId (7, "import_declaration")

  lazy val package_declaration = BlockAnnotationId (8, "package_declaration")

  lazy val theorem_block = BlockAnnotationId (9, "theorem_block")

  lazy val proof_block = BlockAnnotationId (10, "proof_block")

  lazy val comment = BlockAnnotationId (11, "comment")

  lazy val test_declaration = BlockAnnotationId (12, "test_declaration")

  lazy val values =
    Seq (
      undefined,
      function_definition,
      abstract_block_declaration,
      abstract_function_declaration,
      class_beginning,
      class_end,
      class_declaration,
      package_declaration,
      import_declaration,
      theorem_block,
      proof_block,
      comment,
      test_declaration
    )

}

case class BlockAnnotationEnum_ ()
  extends
    BlockAnnotationEnum
{

}
