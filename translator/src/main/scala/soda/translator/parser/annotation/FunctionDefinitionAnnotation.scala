package soda.translator.parser.annotation

trait FunctionDefinitionAnnotation
  extends
    BlockAnnotation
{

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.replacement.ParserStateEnum_
  import   soda.translator.replacement.Tokenizer_

  lazy val identifier = BlockAnnotationEnum_ () .function_definition

  lazy val sc = SodaConstant_ ()

  lazy val symbol_at_the_end: String =
    sc.space +
    sc.function_definition_symbol

  lazy val synonym_at_the_end: String =
    sc.space +
    sc.function_definition_synonym

  lazy val symbol_in_the_middle: String =
    sc.space +
    sc.function_definition_symbol +
    sc.space

  lazy val synonym_in_the_middle: String =
    sc.space +
    sc.function_definition_synonym +
    sc.space

  lazy val plain_state = ParserStateEnum_ () .plain

  lazy val applies: Boolean =
    (contains_the_equals_symbol || starts_with_valid_annotation ) && ! is_a_class_declaration

  lazy val contains_the_equals_symbol: Boolean =
    block.readable_lines.nonEmpty &&
    _contains_the_equals_symbol_with (block.readable_lines.head.line )

  def _contains_the_equals_symbol_with (first_line: String ): Boolean =
    Tokenizer_ (first_line )
      .tokens
      .exists (token =>
        token.parser_state == plain_state &&
        _contains_the_equals_symbol_in_token (token.text )
      )

  def _contains_the_equals_symbol_in_token (token_text: String ): Boolean =
    (
      (token_text.contains (symbol_in_the_middle ) ) ||
      (token_text.contains (synonym_in_the_middle ) ) ||
      (token_text.endsWith (symbol_at_the_end ) ) ||
      (token_text.endsWith (synonym_at_the_end ) )
    )

  lazy val starts_with_valid_annotation: Boolean =
    block.readable_lines.nonEmpty &&
    _starts_with_valid_annotation_with (block.readable_lines.head.line.trim )

  def _starts_with_valid_annotation_with (first_line_trimmed: String ): Boolean =
    (first_line_trimmed == sc.tail_recursion_annotation ||
      first_line_trimmed == sc.override_annotation )

  lazy val is_a_class_declaration: Boolean =
    (starts_with_prefix_and_space (sc.class_reserved_word ) ||
      starts_with_prefix_and_space (sc.class_abbreviation ) )

}

case class FunctionDefinitionAnnotation_ (block: soda.translator.block.Block )
  extends
    FunctionDefinitionAnnotation
{

}
