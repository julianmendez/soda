package soda.translator.parser.annotation

trait ClassAliasAnnotation
  extends
    BlockAnnotationParser
{

  def   block: soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_alias

  lazy val sc = SodaConstant_ ()

  lazy val applies: Boolean =
     (starts_with_prefix_and_space (sc.class_reserved_word ) ||
       starts_with_prefix_and_space (sc.deprecated_class_abbreviation ) ) &&
     contains_the_equals_symbol

  lazy val contains_the_equals_symbol: Boolean =
    FunctionDefinitionAnnotation_ (block ) .contains_the_equals_symbol

}

case class ClassAliasAnnotation_ (block: soda.translator.block.Block )
  extends
    ClassAliasAnnotation
{

}
