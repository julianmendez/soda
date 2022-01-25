package soda.translator.parser.annotation

trait AbstractBlockDeclarationAnnotation
  extends BlockAnnotation {

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .abstract_block_declaration

  lazy val applies: Boolean =
    block.readable_lines.nonEmpty &&
    (block.readable_lines.head.line.trim == SodaConstant_ () .abstract_reserved_word )

}

case class AbstractBlockDeclarationAnnotation_ (block: soda.translator.block.Block )
  extends AbstractBlockDeclarationAnnotation
