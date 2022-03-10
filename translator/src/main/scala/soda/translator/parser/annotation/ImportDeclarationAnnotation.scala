package soda.translator.parser.annotation

trait ImportDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ ().import_declaration

  lazy val applies : Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ ().import_reserved_word)

  lazy val imported_items : Seq [AnnotatedLine] =
    content_lines

}

case class ImportDeclarationAnnotation_ (block : soda.translator.block.Block) extends ImportDeclarationAnnotation
