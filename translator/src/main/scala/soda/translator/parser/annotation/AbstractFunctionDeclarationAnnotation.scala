package soda.translator.parser.annotation

trait AbstractFunctionDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .abstract_function_declaration

  lazy val applies: Boolean =
    contains_one_line &&
    starts_with (SodaConstant_ () .has_reserved_word )

}

case class AbstractFunctionDeclarationAnnotation_ (block: soda.translator.block.Block )  extends AbstractFunctionDeclarationAnnotation
