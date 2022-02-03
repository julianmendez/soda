package soda.translator.parser.annotation

trait ClassEndAnnotation
  extends
    BlockAnnotationParser
{

  def   block: soda.translator.block.Block
  def   references: Seq [soda.translator.block.AnnotatedBlock]

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_end

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    ((block.readable_lines.head.line.trim == SodaConstant_ () .deprecated_class_end_symbol ) ||
      (block.readable_lines.head.line.trim == SodaConstant_ () .class_end_reserved_word ) )

}

case class ClassEndAnnotation_ (block: soda.translator.block.Block, references: Seq [soda.translator.block.AnnotatedBlock] )
  extends
    ClassEndAnnotation
{

}
