package soda.translator.block

trait BlockAnnotationId
  extends
    soda.lib.EnumConstant
{

  def   ordinal: Int
  def   name: String

}

case class BlockAnnotationId_ (ordinal: Int, name: String )
  extends
    BlockAnnotationId
{

}

trait BlockAnnotationEnum
  extends
    soda.lib.Enum [BlockAnnotationId]
{

  lazy val undefined = BlockAnnotationId_ (0, "undefined")

  lazy val function_definition = BlockAnnotationId_ (1, "function_definition")

  lazy val class_beginning = BlockAnnotationId_ (2, "class_beginning")

  lazy val class_end = BlockAnnotationId_ (3, "class_end")

  lazy val abstract_declaration = BlockAnnotationId_ (4, "abstract_declaration")

  lazy val import_declaration = BlockAnnotationId_ (5, "import_declaration")

  lazy val package_declaration = BlockAnnotationId_ (6, "package_declaration")

  lazy val class_alias = BlockAnnotationId_ (7, "class_alias")

  lazy val theorem_block = BlockAnnotationId_ (8, "theorem_block")

  lazy val proof_block = BlockAnnotationId_ (9, "proof_block")

  lazy val comment = BlockAnnotationId_ (10, "comment")

  lazy val test_declaration = BlockAnnotationId_ (11, "test_declaration")

  lazy val values =
    Seq (
      undefined,
      function_definition,
      class_beginning,
      class_end,
      abstract_declaration,
      import_declaration,
      package_declaration,
      class_alias,
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
