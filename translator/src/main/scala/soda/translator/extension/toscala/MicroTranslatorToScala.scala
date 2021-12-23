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

  def replace (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace (token.text, TableTranslator_ (table )  )

  def replace_regex (table: Seq [(String, String )] ): Token => String =
     token =>
        ReplacementAux_ () .replace_regex (token.text, TableTranslator_ (table )  )

  lazy val translation_pipeline =
    BlockTranslatorPipeline_ (Seq (LineForwardJoinerBlockTranslator_ (), LetInBlockTranslator_ (), MatchCaseBlockTranslator_ (), LineBackwardJoinerBlockTranslator_ (), LinePerLineBlockTranslator_ (), TokenizedBlockTranslator_ (replace (tc.main_translation ) ), TokenizedBlockTranslator_ (replace_regex (tc.beautifier ) )      )    )

  def translate (block: Block ): Block =
    translation_pipeline.translate (block )

}

case class MicroTranslatorToScala_ ()  extends MicroTranslatorToScala
