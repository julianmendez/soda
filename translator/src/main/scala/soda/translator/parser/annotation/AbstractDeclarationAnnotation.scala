package soda.translator.parser.annotation

/*
 * This package contains classes to handle block annotations for parsing.
 */



trait AbstractDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block
  def   references : Seq [soda.translator.block.AnnotatedBlock]

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ ().abstract_declaration

  lazy val applies : Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ ().abstract_reserved_word)

  lazy val abstract_functions_with_comments : Seq [AnnotatedLine] =
    content_lines

  lazy val abstract_functions : Seq [AnnotatedLine] =
    abstract_functions_with_comments
      .filter (  line => ! line.is_comment)

}

case class AbstractDeclarationAnnotation_ (block : soda.translator.block.Block, references : Seq [soda.translator.block.AnnotatedBlock]) extends AbstractDeclarationAnnotation
