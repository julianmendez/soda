package soda.translator.parser.annotation

trait BlockAnnotation {

  import soda.translator.block.Block
  import soda.translator.block.BlockAnnotationId

  def block: Block

  def applies: Boolean

  def identifier: BlockAnnotationId

  lazy val contains_one_line: Boolean =
    block.readable_lines.length == 1

  def starts_with (prefix: String ): Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.head.line.trim.startsWith (prefix )

  def ends_with (suffix: String ): Boolean =
    block.readable_lines.nonEmpty &&
    block.readable_lines.last.line.trim.endsWith (suffix )

}
