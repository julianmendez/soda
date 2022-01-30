package soda.translator.parser.annotation

trait ClassBeginningAnnotation
  extends
    soda.translator.block.BlockAnnotation
{

  def   block: soda.translator.block.Block

  import   soda.translator.block.BlockAnnotation
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_beginning

  lazy val applies: Boolean =
    (starts_with_prefix_and_space (SodaConstant_ () .class_reserved_word ) ||
      starts_with_prefix_and_space (SodaConstant_ () .class_abbreviation ) ) &&
    (ends_with_space_and_suffix (SodaConstant_ () .class_begin_symbol ) ||
      ends_with_space_and_suffix (SodaConstant_ () .class_definition_symbol ) ||
      ends_with_space_and_suffix (SodaConstant_ () .class_definition_synonym ) )

}

case class ClassBeginningAnnotation_ (block: soda.translator.block.Block )
  extends
    ClassBeginningAnnotation
{

}
