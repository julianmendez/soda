package soda.translator.extension.toscala

trait ClassDeclarationBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.block.Translator
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.blocktr.TableTranslator_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token

  lazy val soda_opening_parenthesis: String = "("

  lazy val tc = TranslationConstantToScala_ ()

  lazy val _labels = BlockAnnotationEnum_ ()

  lazy val replace_token: Token => String =
     token =>
      Replacement_ (token.text )
        .replace_at_beginning (token.index, get_table_translator (token.text ) )
        .line

  def get_table_translator (line: String ): Translator =
    TableTranslator_ (Seq (Tuple2 (tc.soda_class_reserved_word, get_class_declaration_translation (line ) ) )    )

  def get_class_declaration_translation (line: String ): String =
    if (line.contains (soda_opening_parenthesis )
    ) tc.class_declaration_translation_at_beginning_with_paren
    else
      if (DefinitionLineTranslator_ (line ) .condition_for_type_alias
      ) tc.class_declaration_translation_at_beginning_without_paren_for_type_alias
      else tc.class_declaration_translation_at_beginning_without_paren

  lazy val translator = TokenizedBlockTranslator_ (replace_token )

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == _labels.class_beginning
      || block.block_annotation == _labels.class_declaration
    ) translator.translate (block )
    else block

}

case class ClassDeclarationBlockTranslator_ ()  extends ClassDeclarationBlockTranslator
