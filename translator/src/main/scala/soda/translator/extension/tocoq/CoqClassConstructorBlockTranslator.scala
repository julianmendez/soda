package soda.translator.extension.tocoq

trait CoqClassConstructorBlockTranslator
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

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToCoq_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case AbstractDeclarationAnnotation_ (block, references) => _translate_block (AbstractDeclarationAnnotation_ (block, references) )
      case x => annotated_block
    }

  private def _translate_block (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    _translate_block_with (_get_class_beginning (block.references) ) (block)

  private def _translate_block_with (maybe_beginning : Option [ClassBeginningAnnotation] ) (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    if ( maybe_beginning.isEmpty
    ) block
    else _translate_block_with_beginning (maybe_beginning.get) (block)

  private def _translate_block_with_beginning (beginning : ClassBeginningAnnotation) (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    if ( beginning.is_concrete
    ) block
    else _translate_block_with_abstract_beginning (beginning) (block)

  private def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation) (block : AbstractDeclarationAnnotation) : AbstractDeclarationAnnotation =
    AbstractDeclarationAnnotation_ (
      BlockBuilder_ ().build (
        Seq (_tc.coq_opening_comment).++ (
          block.lines.++ (
            Seq [String] (
              _tc.coq_closing_comment,
              "",
              _get_constructor_declaration (beginning) (_get_types_of_abstract_functions (block) )
            )
          )
        )
      ),
      block.references
    )

  private def _get_constructor_declaration (beginning : ClassBeginningAnnotation) (abstract_functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    _tc.coq_inductive_reserved_word +
    _tc.coq_space +
    beginning.class_name +
    _tc.coq_space +
    _tc.coq_type_membership_symbol +
    _tc.coq_space +
    _tc.coq_type_reserved_word +
    _tc.coq_space +
    _tc.coq_function_definition_symbol +
    _tc.coq_new_line +
    _get_initial_spaces (beginning) +
    _tc.coq_space +
    _tc.coq_space +
    _tc.coq_vertical_bar_symbol +
    _tc.coq_space +
    beginning.class_name +
    _sc.constructor_suffix +
    _tc.coq_space +
    _tc.coq_opening_parenthesis +
    _tc.coq_some_variable_name +
    _tc.coq_space +
    _tc.coq_type_membership_symbol +
    _tc.coq_space +
    abstract_functions.mkString (_tc.coq_space + _tc.coq_product_type_symbol + _tc.coq_space) +
    _tc.coq_closing_parenthesis +
    _tc.coq_new_line +
    _get_initial_spaces (beginning) +
    _tc.coq_inductive_end_symbol

  private def _get_class_beginning (references : Seq [AnnotatedBlock] ) : Option [ClassBeginningAnnotation] =
    references
      .flatMap (  block => _get_as_class_beginning_annotation (block) )
      .headOption

  private def _get_as_class_beginning_annotation (annotated_block : AnnotatedBlock) : Option [ClassBeginningAnnotation] =
    annotated_block match  {
      case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
      case x => None
    }

  private def _get_types_of_abstract_functions (block : AbstractDeclarationAnnotation) : Seq [String] =
    block.abstract_functions
      .map (  annotated_line => _translate_type_symbols (annotated_line.line).trim )
      .map (  line => _remove_variable (line) )

  private def _remove_variable (line : String) : String =
    _remove_variable_with (line) (line.indexOf (_sc.type_membership_symbol) )

  private def _remove_variable_with (line : String) (index : Int) : String =
    if ( index < 0
    ) line
    else line.substring (index + _sc.type_membership_symbol.length).trim

  private def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (_sc.subtype_reserved_word, _tc.coq_subtype_symbol)
      .replaceAll (_sc.supertype_reserved_word, _tc.coq_supertype_symbol)
      .replaceAll (_sc.function_arrow_symbol, _tc.coq_function_arrow_symbol)

  private def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  private def _get_initial_spaces_with (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  private def _get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class CoqClassConstructorBlockTranslator_ () extends CoqClassConstructorBlockTranslator
