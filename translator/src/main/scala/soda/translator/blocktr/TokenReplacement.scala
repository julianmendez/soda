package soda.translator.blocktr

trait TokenReplacement
{

  import   soda.translator.replacement.ReplacementAux_
  import   soda.translator.replacement.ReplacementWithTranslator_

  def replace (table : Seq [ Tuple2 [String, String] ] ) : TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table) ).replace (token.text)
    )

}

case class TokenReplacement_ () extends TokenReplacement
