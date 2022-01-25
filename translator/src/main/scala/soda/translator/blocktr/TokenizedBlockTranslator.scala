package soda.translator.blocktr

trait TokenizedBlockTranslator
  extends soda.translator.block.BlockTranslator {

  import   soda.lib.SomeSD_
  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.block.Translator
  import   soda.translator.replacement.ParserStateEnum_
  import   soda.translator.replacement.Replacement_
  import   soda.translator.replacement.Token
  import   soda.translator.replacement.Token_
  import   soda.translator.replacement.Tokenizer_

  def   replace_token: Token => String

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block
        .annotated_lines
        .map (annotated_line =>
          if (annotated_line.is_comment
          ) annotated_line.line
          else _translate_non_comment (annotated_line.line )
        ),
      block.block_annotation
    )

  def _translate_non_comment (line: String ): String =
      SomeSD_ (line )
        .map (x => Replacement_ (x ) .add_space_to_soda_line () .line )
        .map (x => Tokenizer_ (x ) .tokens )
        .map (x => _translate_line (x )  )
        .map (x => _join_tokens (x )  )
        .map (x => Replacement_ (x ) .remove_space_from_scala_line () .line )
        .value

  def _translate_line (tokens: Seq [Token]  ): Seq [Token] =
    tokens.map (
      token =>
        if (token.parser_state == ParserStateEnum_ () .plain
        ) Token_ (replace_token (token ), token.parser_state, token.index )
        else token
    )

  def _join_tokens (tokens: Seq [Token]  ): String =
    tokens
      .map (token => token.text )
      .mkString ("")

}

case class TokenizedBlockTranslator_ (replace_token: soda.translator.replacement.Token => String )
  extends TokenizedBlockTranslator
