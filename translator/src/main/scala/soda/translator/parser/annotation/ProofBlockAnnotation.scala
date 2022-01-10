package soda.translator.parser.annotation

trait ProofBlockAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .proof_block

  lazy val applies: Boolean =
    starts_with_prefix_and_space (SodaConstant_ () .proof_reserved_word )

}

case class ProofBlockAnnotation_ (block: soda.translator.block.Block )  extends ProofBlockAnnotation
