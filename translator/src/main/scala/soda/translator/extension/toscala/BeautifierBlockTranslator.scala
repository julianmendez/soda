package soda.translator.extension.toscala

trait BeautifierBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.blocktr.TableBlockTranslator_
  import soda.translator.blocktr.TokenizedBlockTranslator_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token

  lazy val source = "scala"

  lazy val target = "scala"

  lazy val tc = TranslationConstantToScala_ ()

  lazy val inner_translator = TableBlockTranslator_ (tc.beautifier )

  lazy val replace_token: Token => String =
     token =>
      Replacement_ (token.text )
        .replace_regex (inner_translator )
        .line

  lazy val translator = TokenizedBlockTranslator_ (replace_token )

  def translate (block: Block ): Block =
    translator.translate (block )

}

case class BeautifierBlockTranslator_ ()  extends BeautifierBlockTranslator
