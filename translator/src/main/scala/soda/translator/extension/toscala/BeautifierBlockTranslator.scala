package soda.translator.extension.toscala

trait BeautifierBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.lib.SomeSD_
  import soda.translator.block.Block
  import soda.translator.block.Block_
  import soda.translator.block.Translator
  import soda.translator.blocktr.CommentPreprocessor_
  import soda.translator.blocktr.TableBlockTranslator_
  import soda.translator.replacement.ParserStateEnum_
  import soda.translator.replacement.Replacement_
  import soda.translator.replacement.Token
  import soda.translator.replacement.Token_
  import soda.translator.replacement.Tokenizer_

  lazy val source = "scala"

  lazy val target = "scala"

  lazy val soda_opening_parenthesis: String = "("

  lazy val tc = TranslationConstantToScala_ ()

  lazy val translator = TableBlockTranslator_ (tc.beautifier )

  def translate (block: Block ): Block =
    Block_ (CommentPreprocessor_ (block.lines )
        .annotated_lines
        .map (annotated_line =>
          if (annotated_line.isComment
          ) annotated_line.line
          else _translate_non_comment (annotated_line.line )        )    )

  def _translate_non_comment (line: String ): String =
      SomeSD_ (line )
        .map (x => Replacement_ (x ) .add_space_to_soda_line () .line )
        .map (x => Tokenizer_ (x ) .tokens )
        .map (x => _translate_line (x )  )
        .map (x => _join_tokens (x )  )
        .map (x => Replacement_ (x ) .remove_space_from_scala_line () .line )
        .value

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (token =>
        if (token.parser_state == ParserStateEnum_ () .plain
        ) Token_ (_get_all_replacements (token ), token.parser_state, token.index )
        else token    )

  def _get_all_replacements (token: Token ): String =
    Replacement_ (token.text )
      .replace_regex (translator )
      .line

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

}

case class BeautifierBlockTranslator_ ()  extends BeautifierBlockTranslator
