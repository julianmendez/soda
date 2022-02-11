package soda.translator.parser.annotation

trait ProofBlockAnnotation
  extends
    BlockAnnotationParser
{

  def   block: soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .proof_block

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ () .proof_reserved_word )

}

case class ProofBlockAnnotation_ (block: soda.translator.block.Block) extends ProofBlockAnnotation
