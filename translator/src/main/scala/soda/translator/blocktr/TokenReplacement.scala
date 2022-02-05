package soda.translator.blocktr

trait TokenReplacement
{

  import   soda.translator.replacement.ReplacementAux_
  import   soda.translator.replacement.ReplacementWithTranslator_

  def add_spaces_to_symbols (symbols: Set [Char] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementAux_ () .add_spaces_to_symbols (token.text, symbols )
    )

  def replace (table: Seq [Tuple2 [String, String]] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table ) ) .replace (token.text )
    )

  def replace_regex (table: Seq [Tuple2 [String, String]] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table ) ) .replace_regex (token.text )
    )

  def replace_at_beginning (table: Seq [Tuple2 [String, String]] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table ) ) .replace_at_beginning (token.text, token.index )
    )

}

case class TokenReplacement_ () extends TokenReplacement


