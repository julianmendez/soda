package soda.translator.extension.toscala

trait ClassDeclarationBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.Translator
  import   soda.translator.blocktr.TableTranslator_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.replacement.Replacement_
  import   soda.translator.parser.annotation.ClassBeginningAnnotation
  import   soda.translator.parser.annotation.ClassAliasAnnotation

  lazy val soda_opening_parenthesis: String = "("

  lazy val tc = TranslationConstantToScala_ ()

  lazy val soda_space: String = " "

  lazy val scala_space: String = " "

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val translate: AnnotatedBlock => AnnotatedBlock =
     block =>
      translate_for (block )

  def translate_for (annotated_block: AnnotatedBlock ): AnnotatedBlock =
    annotated_block match  {
      case block: ClassBeginningAnnotation => _translate_block (block )
      case block: ClassAliasAnnotation => _translate_block (block )
      case x => annotated_block
    }

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      if ((has_condition_for_type_alias (get_first_line (block ) ) )
      ) _process_head (block ) ++ _process_tail (block )
      else _process_head (block ) ++ _process_tail (block ) ++  Seq [String] (tc.scala_class_begin_symbol )
,
      block.block_annotation
    )

  def _process_head (block: AnnotatedBlock ): Seq [String] =
    _process_head_with (get_first_line (block ), block )

  def _process_head_with (line: String, block: AnnotatedBlock ): Seq [String] =
    Seq [String] (Replacement_ (soda_space + line ) .replace_at_beginning (0, get_table_translator (line ) ) .line.substring (soda_space.length ) )

  def _process_tail (block: AnnotatedBlock ): Seq [String] =
    _process_if_extends (remove_first_line (block ) )

  def _process_if_extends (block: AnnotatedBlock ): Seq [String] =
    if ((get_first_line (block ) .trim == tc.extends_reserved_word )
    ) Seq [String] (get_spaces_at_beginning (get_first_line (block ) ) + tc.scala_extends_translation ) ++ _process_after_extends (remove_first_line (block ) )
    else block.lines

  def get_table_translator (line: String ): Translator =
    TableTranslator_ (
      Seq (Tuple2 (tc.soda_class_reserved_word, get_class_declaration_translation (line ) ) )
    )

  def get_class_declaration_translation (line: String ): String =
    if (line.contains (soda_opening_parenthesis )
    ) tc.class_declaration_translation_at_beginning_with_paren
    else
      if (has_condition_for_type_alias (line )
      ) tc.class_declaration_translation_at_beginning_without_paren_for_type_alias
      else tc.class_declaration_translation_at_beginning_without_paren

  def get_number_of_spaces_at_beginning (line: String ): Int =
    line
      .takeWhile (ch => ch.isSpaceChar )
      .length

  def get_spaces_at_beginning (line: String ): String =
    line.substring (0, get_number_of_spaces_at_beginning (line ) )

  def _process_after_extends (block: AnnotatedBlock ): Seq [String] =
    if ((get_first_line (block ) .trim.nonEmpty )
    ) Seq [String] (get_first_line (block )  ) ++ remove_first_line (block ) .lines.map (line => get_spaces_at_beginning (line ) + tc.scala_with_translation + scala_space + line.trim )
    else Seq [String] ()

  def remove_first_line (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      (if (block.lines.isEmpty
        ) block.lines
        else block.lines.tail ),
      block.block_annotation
    )

  def get_first_line (block: AnnotatedBlock ): String =
    block.lines.headOption.getOrElse ("")

  def ends_with_equals (line: String ): Boolean =
    line.trim.endsWith (tc.soda_definition )

  def ends_with_opening_brace (line: String ): Boolean =
    line.trim.endsWith (tc.soda_opening_brace )

  def contains_equals (line: String ): Boolean =
    line.trim.contains (tc.soda_definition )

  def has_condition_for_type_alias (line: String ): Boolean =
    contains_equals (line ) && ! (ends_with_equals (line ) || ends_with_opening_brace (line ) )

}

case class ClassDeclarationBlockTranslator_ ()
  extends
    ClassDeclarationBlockTranslator
{

}
