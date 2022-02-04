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

  lazy val type_separator = ","

  lazy val soda_opening_bracket = "["

  lazy val soda_closing_bracket = "]"

  lazy val applies: Boolean =
    (starts_with_prefix_and_space (sc.class_reserved_word ) ||
      starts_with_prefix_and_space (sc.deprecated_class_abbreviation ) ) &&
    ! contains_the_equals_symbol

  lazy val contains_the_equals_symbol: Boolean =
    FunctionDefinitionAnnotation_ (block ) .contains_the_equals_symbol

  lazy val _get_class_name_and_type_parameters: String =
    skip_first_word (first_readable_line.line )

  lazy val class_name: String =
    get_first_word (_get_class_name_and_type_parameters )

  lazy val type_parameters_and_bounds: Seq [String] =
    remove_brackets (skip_first_word (_get_class_name_and_type_parameters ) )
      .split (sc.parameter_separator_symbol )
      .map (parameter => parameter.trim )

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

}

case class ClassBeginningAnnotation_ (block: soda.translator.block.Block )
  extends
    ClassBeginningAnnotation
{

}
