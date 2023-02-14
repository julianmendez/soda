package soda.translator.extension.toscala

/*
 * This package contains classes for the translation to Scala.
 */





trait ClassDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.Block
  import   soda.translator.block.Translator
  import   soda.translator.blocktr.TableTranslator_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.SodaConstant_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassBeginningAnnotation_
  import   soda.translator.parser.annotation.ClassAliasAnnotation
  import   soda.translator.parser.annotation.ClassAliasAnnotation_
  import   soda.translator.replacement.Replacement_

  private lazy val _sc = SodaConstant_ ()

  private lazy val _tc = TranslationConstantToScala_ ()

  lazy val soda_space : String = _sc.space

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case ClassBeginningAnnotation_ (block) => _translate_class_beginning_block (ClassBeginningAnnotation_ (block) )
      case ClassAliasAnnotation_ (block) => _translate_class_alias_block (ClassAliasAnnotation_ (block) )
      case x => annotated_block
    }

  private def _translate_class_beginning_block (block : ClassBeginningAnnotation) : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (_translate_block (block) )

  private def _translate_class_alias_block (block : ClassAliasAnnotation) : ClassAliasAnnotation =
    ClassAliasAnnotation_ (_translate_block (block) )

  private def _translate_block (block : AnnotatedBlock) : Block =
    BlockBuilder_ ().build (
      if ( (has_condition_for_type_alias (get_first_line (block) ) )
      ) _process_head (block) ++ _process_tail (block)
      else _process_head (block) ++ _process_tail (block) ++ Seq [String] (get_initial_spaces (block) + _tc.scala_class_begin_symbol)
    )

  private def _process_head (block : Block) : Seq [String] =
    _process_head_with (get_first_line (block) ) (block)

  private def _process_head_with (line : String) (block : Block) : Seq [String] =
    Seq [String] (Replacement_ (_sc.space + line).replace_at_beginning (0) (get_table_translator (line) ).line.substring (_sc.space.length) )

  private def _process_tail (block : Block) : Seq [String] =
    _process_if_extends (remove_first_line (block) )

  private def _process_if_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block).trim == _sc.extends_reserved_word)
    ) Seq [String] (get_initial_spaces (block) + _tc.scala_extends_translation) ++ _process_after_extends (remove_first_line (block) )
    else block.lines

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (_sc.class_reserved_word, get_class_declaration_translation (line) ) )
    )

  def get_class_declaration_translation (line : String) : String =
    if ( line.contains (_sc.opening_parenthesis_symbol)
    ) _tc.class_declaration_translation_at_beginning_with_paren
    else
      if ( has_condition_for_type_alias (line)
      ) _tc.class_declaration_translation_at_beginning_without_paren_for_type_alias
      else _tc.class_declaration_translation_at_beginning_without_paren

  private def _process_after_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block).trim.nonEmpty)
    ) Seq [String] (get_first_line (block) ) ++ remove_first_line (block).lines.map (  line => get_initial_spaces_for (line) + _tc.scala_with_translation + _tc.scala_space + line.trim)
    else Seq [String] ()

  def remove_first_line (block : Block) : Block =
    BlockBuilder_ ().build (
      if ( block.lines.isEmpty
      ) block.lines
      else block.lines.tail
    )

  def get_first_line (block : Block) : String =
    block.lines.headOption.getOrElse ("")

  def get_initial_spaces (block : Block) : String =
    get_initial_spaces_for (get_first_line (block) )

  def get_initial_spaces_for (line : String) : String =
    line.takeWhile (  ch => ch.isSpaceChar)

  def ends_with_equals (line : String) : Boolean = false

  def ends_with_opening_brace (line : String) : Boolean = false

  def contains_equals (line : String) : Boolean =
    line.trim.contains (_sc.function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line)

}

case class ClassDeclarationBlockTranslator_ () extends ClassDeclarationBlockTranslator
