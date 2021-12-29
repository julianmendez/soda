package soda.translator.parser

case class BlockAnnotation (ordinal: Int, name: String )  extends soda.lib.EnumConstant

trait BlockAnnotationEnum  extends soda.lib.Enum [BlockAnnotation] {

  lazy val undefined = BlockAnnotation (0, "undefined")

  lazy val function_definition = BlockAnnotation (1, "function_definition")

  lazy val abstract_function_declaration = BlockAnnotation (2, "abstract_function_declaration")

  lazy val class_beginning = BlockAnnotation (3, "class_beginning")

  lazy val class_end = BlockAnnotation (4, "class_end")

  lazy val package_declaration = BlockAnnotation (5, "package_declaration")

  lazy val only_comment = BlockAnnotation (6, "only_comment")

  lazy val values =
    Seq (undefined, function_definition, abstract_function_declaration, class_beginning, class_end, package_declaration, only_comment    )

}

case class BlockAnnotationEnum_ ()  extends BlockAnnotationEnum

trait AnnotatedBlock  extends soda.translator.block.Block {

  def annotation: BlockAnnotation

}

case class AnnotatedBlock_ (lines: Seq [String], annotated_lines: Seq [soda.translator.block.AnnotatedLine], annotation: BlockAnnotation )  extends AnnotatedBlock
