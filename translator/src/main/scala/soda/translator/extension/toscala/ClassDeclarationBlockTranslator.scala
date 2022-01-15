package soda.translator.extension.toscala

trait ClassDeclarationBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.block.Translator
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.blocktr.TableTranslator_
  import soda.translator.replacement.Replacement
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token

  lazy val soda_opening_parenthesis: String = "("

  lazy val tc = TranslationConstantToScala_ ()

  lazy val soda_space: String = " "

  lazy val scala_space: String = " "

  lazy val soda_definition = tc.soda_definition

  lazy val scala_3_class_definition = tc.scala_3_class_definition

  lazy val space_and_definition = soda_space + soda_definition

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val replace_token: Token => String =
     token =>
      Replacement_ (token.text )
        .replace_at_beginning (token.index, get_table_translator (token.text ) )
        .replace_with (_replace_definition_symbol )
        .line

  def _replace_definition_symbol (line: String ): String =
    if ((has_condition_for_type_alias (line )  )
    ) line
    else
      if ((line.trim.endsWith (space_and_definition ) )
      ) line.replaceAll (space_and_definition, scala_3_class_definition )
      else line.replaceAll (space_and_definition, "")

  def get_table_translator (line: String ): Translator =
    TableTranslator_ (Seq (Tuple2 (tc.soda_class_reserved_word, get_class_declaration_translation (line ) ) )    )

  def get_class_declaration_translation (line: String ): String =
    if (line.contains (soda_opening_parenthesis )
    ) tc.class_declaration_translation_at_beginning_with_paren
    else
      if (has_condition_for_type_alias (line )
      ) tc.class_declaration_translation_at_beginning_without_paren_for_type_alias
      else tc.class_declaration_translation_at_beginning_without_paren

  lazy val translator = TokenizedBlockTranslator_ (replace_token )

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == _labels.class_beginning
      || block.block_annotation == _labels.class_declaration
    ) translator.translate (block )
    else block

  def translation_of_class_definition (line: String ): Replacement =
    if (has_condition_for_type_alias (line )
    ) Replacement_ (line )
    else Replacement_ (line ) .replace_all (soda_space + tc.soda_definition, _new_text_for_class_definition (line ) )

  def _new_text_for_class_definition (line: String ): String =
    if (ends_with_equals (line )
    ) tc.scala_3_class_definition
    else ""

  def ends_with_equals (line: String ): Boolean =
    line.trim.endsWith (tc.soda_definition )

  def ends_with_opening_brace (line: String ): Boolean =
    line.trim.endsWith (tc.soda_opening_brace )

  def contains_equals (line: String ): Boolean =
    line.trim.contains (tc.soda_definition )

  def has_condition_for_type_alias (line: String ): Boolean =
    contains_equals (line ) && ! (ends_with_equals (line ) || ends_with_opening_brace (line ) )

}

case class ClassDeclarationBlockTranslator_ ()  extends ClassDeclarationBlockTranslator
