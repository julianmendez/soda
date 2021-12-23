package soda.translator.blocktr

trait TokenReplacement {

  import soda.translator.replacement.ReplacementAux_

  def add_spaces_to_symbols (symbols: Set [Char] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (token =>
        ReplacementAux_ () .add_spaces_to_symbols (token.text, symbols )    )

  def replace (table: Seq [(String, String )] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (token =>
        ReplacementAux_ () .replace (token.text, TableTranslator_ (table )  )    )

  def replace_regex (table: Seq [(String, String )] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (token =>
        ReplacementAux_ () .replace_regex (token.text, TableTranslator_ (table )  )    )

  def replace_at_beginning (table: Seq [(String, String )] ): TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (token =>
        ReplacementAux_ () .replace_at_beginning (token.text, token.index, TableTranslator_ (table )  )    )

}

case class TokenReplacement_ ()  extends TokenReplacement
