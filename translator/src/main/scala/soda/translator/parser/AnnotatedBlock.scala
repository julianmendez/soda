package soda.translator.parser

case class BlockAnnotation (ordinal: Int, name: String )  extends soda.lib.EnumConstant

trait BlockAnnotationEnum  extends soda.lib.Enum [BlockAnnotation] {

  lazy val undefined = BlockAnnotation (0, "undefined")

  lazy val function_definition = BlockAnnotation (1, "function_definition")

  lazy val class_beginning = BlockAnnotation (2, "class_beginning")

  lazy val class_end = BlockAnnotation (3, "class_end")

  lazy val class_declaration = BlockAnnotation (4, "class_end")

  lazy val abstract_function_declaration = BlockAnnotation (5, "abstract_function_declaration")

  lazy val import_declaration = BlockAnnotation (6, "import_declaration")

  lazy val package_declaration = BlockAnnotation (7, "package_declaration")

  lazy val comment = BlockAnnotation (8, "comment")

  lazy val test_declaration = BlockAnnotation (9, "comment")

  lazy val values =
    Seq (undefined, function_definition, abstract_function_declaration, class_beginning, class_end, class_declaration, package_declaration, import_declaration, comment, test_declaration    )

}

case class BlockAnnotationEnum_ ()  extends BlockAnnotationEnum

trait AnnotatedBlock  extends soda.translator.block.Block {

  def annotation: BlockAnnotation

}

case class AnnotatedBlock_ (lines: Seq [String], annotated_lines: Seq [soda.translator.block.AnnotatedLine], annotation: BlockAnnotation )  extends AnnotatedBlock
