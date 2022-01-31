package soda.translator.parser.annotation

trait AbstractDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block: soda.translator.block.Block

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .abstract_declaration

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ () .abstract_reserved_word )

  lazy val abstract_items: Seq [AnnotatedLine] =
    content_lines

}

case class AbstractDeclarationAnnotation_ (block: soda.translator.block.Block )
  extends
    AbstractDeclarationAnnotation
{

}


