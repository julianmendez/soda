package soda.translator.parser.annotation

/*
 * This package contains classes to handle block annotations for parsing.
 */



trait CommentAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_

  lazy val identifier = BlockAnnotationEnum_ ().comment

  lazy val applies : Boolean =
    block
      .annotated_lines
      .forall (  annotated_line => annotated_line.is_comment)

}

case class CommentAnnotation_ (block : soda.translator.block.Block) extends CommentAnnotation
