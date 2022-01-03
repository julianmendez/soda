package soda.translator.parser.annotation

trait BlockAnnotation {

  import soda.translator.block.Block
  import soda.translator.block.BlockAnnotationId
  import soda.translator.parser.SodaConstant_

  def block: Block

  def applies: Boolean

  def identifier: BlockAnnotationId

  lazy val space = SodaConstant_ () .space

  lazy val contains_one_line: Boolean =
    block.readable_lines.length == 1

  def starts_with_prefix_and_space (prefix: String ): Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.head.line.trim.startsWith (prefix + space )

  def ends_with_space_and_suffix (suffix: String ): Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.last.line.trim.endsWith (space + suffix )

}
