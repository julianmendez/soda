package soda.translator.parser.annotation

/*
 * This package contains classes to handle block annotations for parsing.
 */



trait TheoremBlockAnnotation
  extends
    BlockAnnotationParser
{

  def   block : soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ ().theorem_block

  lazy val applies : Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ ().theorem_reserved_word)

}

case class TheoremBlockAnnotation_ (block : soda.translator.block.Block) extends TheoremBlockAnnotation
