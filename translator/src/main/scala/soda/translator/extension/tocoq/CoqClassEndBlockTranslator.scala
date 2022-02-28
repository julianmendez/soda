package soda.translator.extension.tocoq

trait CoqClassEndBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val sc = SodaConstant_ ()

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassEndAnnotation_ (block, references) => _translate_block (ClassEndAnnotation_ (block, references) )
      case x => annotated_block
    }

  def _translate_block (block : ClassEndAnnotation) : ClassEndAnnotation =
    _translate_block_with (_get_class_beginning (block.references) ) (block)

  def _translate_block_with (maybe_beginning : Option [ClassBeginningAnnotation] ) (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( maybe_beginning.isEmpty
    ) block
    else _translate_block_with_beginning (maybe_beginning.get) (block)

  def _translate_block_with_beginning (beginning : ClassBeginningAnnotation) (block : ClassEndAnnotation) : ClassEndAnnotation =
    if ( beginning.is_concrete
    ) block
    else _translate_block_with_abstract_beginning (beginning) (block)

  def _translate_block_with_abstract_beginning (beginning : ClassBeginningAnnotation) (block : ClassEndAnnotation) : ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ ().build (
        Seq [String] (
          tc.coq_module_end_reserved_word + tc.coq_space + beginning.class_name + tc.coq_space + tc.coq_end_symbol
        )
      ),
      block.references
    )

  def _get_constructor_declaration (beginning : ClassBeginningAnnotation) (abstract_functions : Seq [String] ) : String =
    tc.coq_module_end_reserved_word +
    tc.coq_space +
    beginning.class_name

  def _get_as_parameter_list (parameters : Seq [String] ) : String =
    if ( parameters.isEmpty
    ) ""
    else tc.coq_space + tc.coq_opening_brace + parameters.mkString (tc.coq_product_type_symbol + tc.coq_space) + tc.coq_closing_brace

  def _get_class_beginning (references : Seq [AnnotatedBlock] ) : Option [ClassBeginningAnnotation] =
    references
      .flatMap (  block =>
        block match  {
          case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
          case x => None
        }
      )
      .headOption

  def _remove_variable (line : String) : String =
    _remove_variable_with (line) (line.indexOf (sc.type_membership_symbol) )

  def _remove_variable_with (line : String) (index : Int) : String =
    if ( index < 0
    ) line
    else line.substring (index + sc.type_membership_symbol.length).trim

  def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (sc.subtype_reserved_word, tc.coq_subtype_symbol)
      .replaceAll (sc.supertype_reserved_word, tc.coq_supertype_symbol)
      .replaceAll (sc.function_arrow_symbol, tc.coq_function_arrow_symbol)

  def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  def _get_initial_spaces_with (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  def _get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class CoqClassEndBlockTranslator_ () extends CoqClassEndBlockTranslator
