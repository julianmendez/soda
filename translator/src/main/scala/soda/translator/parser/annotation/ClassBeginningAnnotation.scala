package soda.translator.parser.annotation

trait ClassBeginningAnnotation
  extends
    BlockAnnotationParser
{

  def   block: soda.translator.block.Block

  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.SodaConstant_

  lazy val identifier = BlockAnnotationEnum_ () .class_beginning

  lazy val sc = SodaConstant_ ()

  lazy val applies: Boolean =
    (starts_with_prefix_and_space (sc.class_reserved_word ) ||
      starts_with_prefix_and_space (sc.deprecated_class_abbreviation ) ) &&
    ! contains_the_equals_symbol

  lazy val contains_the_equals_symbol: Boolean =
    FunctionDefinitionAnnotation_ (block ) .contains_the_equals_symbol

  lazy val contains_an_opening_parenthesis: Boolean =
    first_readable_line.line.contains (sc.opening_parenthesis_symbol )

  lazy val class_name_and_type_parameters: String =
    skip_first_word (first_readable_line.line )

  lazy val class_name: String =
    get_first_word (class_name_and_type_parameters )

  lazy val type_parameters_and_bounds: Seq [String] =
    remove_brackets (skip_first_word (class_name_and_type_parameters ) )
      .split (sc.parameter_separator_symbol )
      .toIndexedSeq
      .map (parameter => parameter.trim )
      .filter (parameter => ! parameter.isEmpty )

  lazy val type_parameters: Seq [String] =
    type_parameters_and_bounds
      .map (parameter => get_first_word (parameter ) )

  def remove_brackets (text: String ): String =
    remove_brackets_with (text.trim )

  def remove_brackets_with (trimmed_text: String ): String =
    if (trimmed_text.startsWith (sc.opening_bracket_symbol ) &&
      trimmed_text.endsWith (sc.closing_bracket_symbol )
    ) trimmed_text.substring (sc.opening_bracket_symbol.length, trimmed_text.length - sc.closing_bracket_symbol.length )
    else trimmed_text

  lazy val is_concrete: Boolean = applies && contains_an_opening_parenthesis

  lazy val is_abstract: Boolean = applies && ! contains_an_opening_parenthesis

}

case class ClassBeginningAnnotation_ (block: soda.translator.block.Block) extends ClassBeginningAnnotation
