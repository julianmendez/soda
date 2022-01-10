package soda.translator.parser.annotation

trait TheoremBlockAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .theorem_block

  lazy val applies: Boolean =
    starts_with_prefix_and_space (SodaConstant_ () .theorem_reserved_word )

}

case class TheoremBlockAnnotation_ (block: soda.translator.block.Block )  extends TheoremBlockAnnotation
