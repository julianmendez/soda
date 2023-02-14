package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */





trait ClassConstructorBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) => _translate_block (ClassEndAnnotation_ (block, references) )
      case x => annotated_block
    }

  private def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    _translate_block_with (_get_class_beginning (block.references) ) (block)

  private def _translate_block_with (maybe_beginning : Option [ClassBeginningAnnotation] ) (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( maybe_beginning.isEmpty
    ) block
    else _translate_block_with_beginning (maybe_beginning.get) (block)

  private def _translate_block_with_beginning (beginning : ClassBeginningAnnotation) (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( beginning.is_concrete
    ) block
    else _translate_block_with_abstract_beginning (beginning) (block)

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation) (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ ().build (
        block.lines.++ (
          Seq [String] (
            "",
            _get_constructor_declaration (beginning) (_get_abstract_functions (block.references) )
          )
        )
      ),
      block.references
    )

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation) (abstract_functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    _tc.class_declaration_translation_at_beginning_with_paren +
    _tc.scala_space +
    beginning.class_name +
    _sc.constructor_suffix +
    _translate_type_symbols (_get_as_parameter_list (beginning.type_parameters_and_bounds) ) +
    _tc.scala_space +
    _tc.scala_opening_parenthesis +
    abstract_functions.mkString (_tc.scala_parameter_separator_symbol + _tc.scala_space) +
    _tc.scala_closing_parenthesis +
    _tc.scala_space +
    _tc.scala_extends_translation +
    _tc.scala_space +
    beginning.class_name +
    _get_as_parameter_list (beginning.type_parameters)

  private def _get_as_parameter_list (parameters : Seq [String] ) : String =
    if ( parameters.isEmpty
    ) ""
    else _tc.scala_space + _tc.scala_opening_bracket + parameters.mkString (_tc.scala_parameter_separator_symbol + _tc.scala_space) + _tc.scala_closing_bracket

  private def _get_class_beginning (references : Seq [AnnotatedBlock] ) : Option [ClassBeginningAnnotation] =
    references
      .flatMap (  block => _get_as_class_beginning_annotation (block) )
      .headOption

  private def _get_as_class_beginning_annotation (annotated_block : AnnotatedBlock) : Option [ClassBeginningAnnotation] =
    annotated_block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case x => None
    }

  private def _get_abstract_functions (references : Seq [AnnotatedBlock] ) : Seq [String] =
    references
      .flatMap (  block => _get_as_abstract_declaration_annotation (block) )
      .flatMap (  block => block.abstract_functions)
      .map (  annotated_line => _translate_type_symbols (annotated_line.line).trim )

  private def _get_as_abstract_declaration_annotation (block : AnnotatedBlock) : Option [AbstractDeclarationAnnotation] =
    block match  {
      case AbstractDeclarationAnnotation_ (b, references) => Some (AbstractDeclarationAnnotation_ (b, references) )
      case x => None
    }

  private def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (_sc.subtype_reserved_word, _tc.scala_subtype_symbol)
      .replaceAll (_sc.supertype_reserved_word, _tc.scala_supertype_symbol)
      .replaceAll (_sc.function_arrow_symbol, _tc.scala_function_arrow_symbol)

  private def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private def _get_initial_spaces_with (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  private def _get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class ClassConstructorBlockTranslator_ () extends ClassConstructorBlockTranslator
