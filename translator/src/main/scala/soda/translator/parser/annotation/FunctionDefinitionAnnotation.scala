package soda.translator.parser.annotation

trait FunctionDefinitionAnnotation  extends BlockAnnotation {

  import soda.translator.block.Block
  import soda.translator.block.Block_
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .function_definition

  lazy val symbol_at_the_end: String =
    SodaConstant_ () .space +
    SodaConstant_ () .function_definition_symbol

  lazy val synonym_at_the_end: String =
    SodaConstant_ () .space +
    SodaConstant_ () .function_definition_synonym

  lazy val symbol_in_the_middle: String =
    SodaConstant_ () .space +
    SodaConstant_ () .function_definition_symbol +
    SodaConstant_ () .space

  lazy val synonym_in_the_middle: String =
    SodaConstant_ () .space +
    SodaConstant_ () .function_definition_synonym +
    SodaConstant_ () .space

  lazy val applies: Boolean =
    contains_the_equals_symbol && ! is_a_class_declaration

  lazy val contains_the_equals_symbol: Boolean =
    block.readable_lines.nonEmpty &&
    ((contains_one_line && block.readable_lines.head.line.trim.contains (symbol_in_the_middle ) ) ||
      (contains_one_line && block.readable_lines.head.line.trim.contains (synonym_in_the_middle ) ) ||
      (block.readable_lines.head.line.trim.endsWith (symbol_at_the_end ) ) ||
      (block.readable_lines.head.line.trim.endsWith (synonym_at_the_end ) )    )

  lazy val is_a_class_declaration: Boolean =
    (starts_with_prefix_and_space (SodaConstant_ () .class_reserved_word ) ||
      starts_with_prefix_and_space (SodaConstant_ () .class_abbreviation ) ) &&
    ends_with_space_and_suffix (SodaConstant_ () .class_open_symbol )

}

case class FunctionDefinitionAnnotation_ (block: soda.translator.block.Block )  extends FunctionDefinitionAnnotation
