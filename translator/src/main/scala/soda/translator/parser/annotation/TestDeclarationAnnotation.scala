package soda.translator.parser.annotation

trait TestDeclarationAnnotation
  extends
    BlockAnnotationParser
{

  def   block: soda.translator.block.Block

  import   soda.translator.block.BlockAnnotation
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .test_declaration

  lazy val applies: Boolean =
    starts_with_prefix_and_space (SodaConstant_ () .test_special_function )

}

case class TestDeclarationAnnotation_ (block: soda.translator.block.Block )
  extends
    TestDeclarationAnnotation
{

}
