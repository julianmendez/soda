package soda.translator.extension.toscala

trait ClassConstructorBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val sc = SodaConstant_ ()

  lazy val tc = TranslationConstantToScala_ ()

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
        block.lines.++ (
          Seq [String] (
            "",
            _get_constructor_declaration (beginning) (_get_abstract_functions (block.references) )
          )
        )
      ),
      block.references
    )

  def _get_constructor_declaration (beginning : ClassBeginningAnnotation) (abstract_functions : Seq [String] ) : String =
    _get_initial_spaces (beginning) +
    tc.class_declaration_translation_at_beginning_with_paren +
    tc.scala_space +
    beginning.class_name +
    sc.constructor_suffix +
    _translate_type_symbols (_get_as_parameter_list (beginning.type_parameters_and_bounds) ) +
    tc.scala_space +
    tc.scala_opening_parenthesis +
    abstract_functions.mkString (tc.scala_parameter_separator_symbol + tc.scala_space) +
    tc.scala_closing_parenthesis +
    tc.scala_space +
    tc.scala_extends_translation +
    tc.scala_space +
    beginning.class_name +
    _get_as_parameter_list (beginning.type_parameters)

  def _get_as_parameter_list (parameters : Seq [String] ) : String =
    if ( parameters.isEmpty
    ) ""
    else tc.scala_space + tc.scala_opening_bracket + parameters.mkString (tc.scala_parameter_separator_symbol + tc.scala_space) + tc.scala_closing_bracket

  def _get_class_beginning (references : Seq [AnnotatedBlock] ) : Option [ClassBeginningAnnotation] =
    references
      .flatMap (  block =>
        block match  {
          case ClassBeginningAnnotation_ (b) => Some (ClassBeginningAnnotation_ (b) )
          case x => None
        }
      )
      .headOption

  def _get_abstract_functions (references : Seq [AnnotatedBlock] ) : Seq [String] =
    references
      .flatMap (  block =>
        block match  {
          case AbstractDeclarationAnnotation_ (b, references) => Some (AbstractDeclarationAnnotation_ (b, references) )
          case x => None
        }
      )
      .flatMap (  block => block.abstract_functions)
      .map (  annotated_line => _translate_type_symbols (annotated_line.line).trim )

  def _translate_type_symbols (line : String) : String =
    line
      .replaceAll (sc.subtype_reserved_word, tc.scala_subtype_symbol)
      .replaceAll (sc.supertype_reserved_word, tc.scala_supertype_symbol)
      .replaceAll (sc.function_arrow_symbol, tc.scala_function_arrow_symbol)

  def _get_initial_spaces (block : AnnotatedBlock) : String =
    _get_initial_spaces_with (_get_first_line (block) )

  def _get_initial_spaces_with (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  def _get_first_line (block : AnnotatedBlock) : String =
    block.lines.headOption.getOrElse ("")

}

case class ClassConstructorBlockTranslator_ () extends ClassConstructorBlockTranslator
