package soda.translator.extension.toscala

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

  lazy val sc = SodaConstant_ ()

  lazy val tc = TranslationConstantToScala_ ()

  lazy val soda_space : String = sc.space

  lazy val scala_space : String = " "

  lazy val translate : AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block)

  def translate_for (annotated_block : AnnotatedBlock) : AnnotatedBlock =
    annotated_block match  {
      case block : ClassBeginningAnnotation => _translate_class_beginning_block (block)
      case block : ClassAliasAnnotation => _translate_class_alias_block (block)
      case x => annotated_block
    }

  def _translate_class_beginning_block (block : ClassBeginningAnnotation) : ClassBeginningAnnotation =
    ClassBeginningAnnotation_ (_translate_block (block) )

  def _translate_class_alias_block (block : ClassAliasAnnotation) : ClassAliasAnnotation =
    ClassAliasAnnotation_ (_translate_block (block) )

  def _translate_block (block : AnnotatedBlock) : Block =
    BlockBuilder_ ().build (
      if ( (has_condition_for_type_alias (get_first_line (block) ) )
      ) _process_head (block) ++ _process_tail (block)
      else _process_head (block) ++ _process_tail (block) ++  Seq [String] (tc.scala_class_begin_symbol)
    )

  def _process_head (block : Block) : Seq [String] =
    _process_head_with (get_first_line (block), block)

  def _process_head_with (line : String, block : Block) : Seq [String] =
    Seq [String] (Replacement_ (sc.space + line).replace_at_beginning (0) (get_table_translator (line) ).line.substring (sc.space.length) )

  def _process_tail (block : Block) : Seq [String] =
    _process_if_extends (remove_first_line (block) )

  def _process_if_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block).trim == sc.extends_reserved_word)
    ) Seq [String] (get_spaces_at_beginning (get_first_line (block) ) + tc.scala_extends_translation) ++ _process_after_extends (remove_first_line (block) )
    else block.lines

  def get_table_translator (line : String) : Translator =
    TableTranslator_ (
      Seq (Tuple2 (sc.class_reserved_word, get_class_declaration_translation (line) ) )
    )

  def get_class_declaration_translation (line : String) : String =
    if ( line.contains (sc.opening_parenthesis_symbol)
    ) tc.class_declaration_translation_at_beginning_with_paren
    else
      if ( has_condition_for_type_alias (line)
      ) tc.class_declaration_translation_at_beginning_without_paren_for_type_alias
      else tc.class_declaration_translation_at_beginning_without_paren

  def get_number_of_spaces_at_beginning (line : String) : Int =
    line
      .takeWhile (  ch => ch.isSpaceChar)
      .length

  def get_spaces_at_beginning (line : String) : String =
    line.substring (0, get_number_of_spaces_at_beginning (line) )

  def _process_after_extends (block : Block) : Seq [String] =
    if ( (get_first_line (block).trim.nonEmpty)
    ) Seq [String] (get_first_line (block)) ++ remove_first_line (block).lines.map (  line => get_spaces_at_beginning (line) + tc.scala_with_translation + scala_space + line.trim)
    else Seq [String] ()

  def remove_first_line (block : Block) : Block =
    BlockBuilder_ ().build (
      if ( block.lines.isEmpty
      ) block.lines
      else block.lines.tail
    )

  def get_first_line (block : Block) : String =
    block.lines.headOption.getOrElse ("")

  def ends_with_equals (line : String) : Boolean =
    line.trim.endsWith (sc.deprecated_class_definition_symbol)

  def ends_with_opening_brace (line : String) : Boolean =
    line.trim.endsWith (sc.deprecated_class_beginning_symbol)

  def contains_equals (line : String) : Boolean =
    line.trim.contains (sc.function_definition_symbol)

  def has_condition_for_type_alias (line : String) : Boolean =
    contains_equals (line) && ! (ends_with_equals (line) || ends_with_opening_brace (line) )

}

case class ClassDeclarationBlockTranslator_ () extends ClassDeclarationBlockTranslator
