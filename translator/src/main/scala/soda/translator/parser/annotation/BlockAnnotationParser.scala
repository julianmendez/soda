package soda.translator.parser.annotation

/*
 * This package contains classes to handle block annotations for parsing.
 */



trait BlockAnnotationParser
  extends
    soda.translator.block.AnnotatedBlock
{

  def   block : soda.translator.block.Block
  def   applies : Boolean
  def   identifier : soda.translator.block.BlockAnnotationId

  import   soda.translator.block.AnnotatedLine
  import   soda.translator.block.AnnotatedLine_
  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.parser.SodaConstant_

  lazy val space = SodaConstant_ ().space

  lazy val default_annotated_line = AnnotatedLine_ ("", true)

  lazy val annotated_lines : Seq [AnnotatedLine] = block.annotated_lines

  lazy val block_annotation : BlockAnnotationId = identifier

  def starts_with_prefix_and_space (prefix : String) : Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.head.line.trim.startsWith (prefix + space)

  lazy val content_lines : Seq [AnnotatedLine] =
    if ( block.readable_lines.isEmpty
    ) block.annotated_lines
    else
      block
        .annotated_lines
        .tail
        .filter (  x => ! x.line.trim.isEmpty)

  lazy val first_readable_line : AnnotatedLine =
    block.readable_lines.headOption.getOrElse (default_annotated_line)

  def get_first_word (line : String) : String =
    (_get_first_word_with (line.trim.indexOf (space) ) (line) ).trim

  private def _get_first_word_with (index : Int) (line : String) : String =
    if ( index >= 0
    ) line.substring (0, index)
    else line

  def skip_first_word (line : String) : String =
    (_skip_first_word_with (line.trim.indexOf (space) ) (line) ).trim

  private def _skip_first_word_with (index : Int) (line : String) : String =
    if ( index >= 0
    ) line.trim.substring (index)
    else ""

}

case class BlockAnnotationParser_ (block : soda.translator.block.Block, applies : Boolean, identifier : soda.translator.block.BlockAnnotationId) extends BlockAnnotationParser
