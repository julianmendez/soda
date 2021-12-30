package soda.translator.parser.annotation

trait TestDeclarationAnnotation  extends BlockAnnotation {

  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .test_declaration

  lazy val applies: Boolean =
    starts_with (SodaConstant_ () .test_special_function + SodaConstant_ () .space )

}

case class TestDeclarationAnnotation_ (block: soda.translator.block.Block )  extends TestDeclarationAnnotation
