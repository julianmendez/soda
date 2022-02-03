package soda.translator.extension.toscala

trait ClassConstructorBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.AbstractDeclarationAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation
  import   soda.translator.parser.annotation.ClassEndAnnotation_

  lazy val sc = SodaConstant_ ()

  lazy val tc = TranslationConstantToScala_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: ClassEndAnnotation => _translate_block (block )
      case x => annotated_block
    }

  def _translate_block (block: ClassEndAnnotation ): ClassEndAnnotation =
    ClassEndAnnotation_ (
      BlockBuilder_ () .build (
        Seq [String] (
          tc.scala_class_end_symbol,
          "",
          _get_constructor_declaration (
            _get_class_name (block.references ),
            _get_type_parameters (block.references ),
            _get_abstract_functions (block.references )
          )
        )
      ),
      block.references
    )

  def _get_constructor_declaration (class_name: String, type_parameters: String, abstract_functions: Seq [String] ): String =
    tc.class_declaration_translation_at_beginning_with_paren +
    tc.scala_space +
    class_name +
    sc.constructor_suffix +
    tc.scala_space +
    type_parameters +
    tc.scala_space +
    tc.scala_opening_parenthesis +
    abstract_functions.mkString (tc.scala_comma ) +
    tc.scala_closing_parenthesis +
    tc.scala_space +
    tc.scala_extends_translation +
    tc.scala_space +
    class_name +
    tc.scala_space +
    type_parameters

  def _get_class_name (references: Seq [AnnotatedBlock] ): String =
    _get_class_beginning (references )
      .map (block => block.class_name )
      .getOrElse ("")

  def _get_type_parameters (references: Seq [AnnotatedBlock] ): String =
    _get_class_beginning (references )
      .map (block => _translate_type_symbols (block.type_parameters ) )
      .getOrElse ("")

  def _get_class_beginning (references: Seq [AnnotatedBlock] ): Option [ClassBeginningAnnotation] =
    references
      .flatMap (block =>
        block match  {
          case b: ClassBeginningAnnotation => Some (b )
          case x => None
        }
      )
      .headOption

  def _get_abstract_functions (references: Seq [AnnotatedBlock] ): Seq [String] =
    references
      .flatMap (block =>
        block match  {
          case b: AbstractDeclarationAnnotation => Some (b )
          case x => None
        }
      )
      .flatMap (block => block.abstract_items )
      .map (annotated_line =>  _translate_type_symbols (annotated_line.line ) )

  def _translate_type_symbols (line: String ): String =
    line
      .replaceAll (sc.subtype_reserved_word, tc.scala_subtype_symbol )
      .replaceAll (sc.supertype_reserved_word, tc.scala_supertype_symbol )
      .replaceAll (sc.function_arrow_symbol, tc.scala_function_arrow_symbol )

}

case class ClassConstructorBlockTranslator_ ()
  extends
    ClassConstructorBlockTranslator
{

}
