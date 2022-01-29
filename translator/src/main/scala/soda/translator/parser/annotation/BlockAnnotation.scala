package soda.translator.parser.annotation

trait BlockAnnotation
{

  def   block: soda.translator.block.Block
  def   applies: Boolean
  def   identifier: soda.translator.block.BlockAnnotationId

  import   soda.translator.block.Block
  import   soda.translator.block.BlockAnnotationId
  import   soda.translator.parser.SodaConstant_

  lazy val space = SodaConstant_ () .space

  def starts_with_prefix_and_space (prefix: String ): Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.head.line.trim.startsWith (prefix + space )

  def ends_with_space_and_suffix (suffix: String ): Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.last.line.trim.endsWith (space + suffix )

}
