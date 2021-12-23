package soda.translator.extension.toscala

/**
 * This class translates Soda source code into Scala source code.
 */
trait MicroTranslatorToScala  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.block.BlockTranslatorPipeline_
  import soda.translator.blocktr.LineBackwardJoinerBlockTranslator_
  import soda.translator.blocktr.LineForwardJoinerBlockTranslator_
  import soda.translator.blocktr.TableTranslator_
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.replacement.ReplacementAux_
  import soda.translator.replacement.Token

  lazy val source = "soda"

  lazy val target = "scala"

  lazy val new_line = "\n"

  lazy val tc = TranslationConstantToScala_ ()

  def add_spaces_to_symbols (symbols: Set [Char] ): Token => String =
     token =>
        ReplacementAux_ () .add_spaces_to_symbols (token.text, symbols )

  def replace (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace (token.text, TableTranslator_ (table )  )

  def replace_regex (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace_regex (token.text, TableTranslator_ (table )  )

  def replace_at_beginning (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace_at_beginning (token.text, token.index, TableTranslator_ (table )  )

  lazy val try_definition: Token => String =
     token =>
      DefinitionLineTranslator_ (token.text ) .translation

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (LineForwardJoinerBlockTranslator_ (), LetInBlockTranslator_ (), MatchCaseBlockTranslator_ (), LineBackwardJoinerBlockTranslator_ (), TokenizedBlockTranslator_ (add_spaces_to_symbols (symbols = tc.soda_brackets_and_comma.toSet ) ), TokenizedBlockTranslator_ (replace (tc.scala_non_soda ) ), TokenizedBlockTranslator_ (replace_at_beginning (tc.synonym_at_beginning ) ), TokenizedBlockTranslator_ (replace (tc.synonym )  ), TokenizedBlockTranslator_ (try_definition ), ClassDeclarationBlockTranslator_ (), TokenizedBlockTranslator_ (replace (tc.main_translation ) ), TokenizedBlockTranslator_ (replace_regex (tc.beautifier ) )      )    )

  def translate (block: Block ): Block =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToScala_ ()  extends MicroTranslatorToScala
