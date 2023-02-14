package soda.translator.blocktr

/*
 * This package contains shared block translators.
 *
 * @see soda.translator.block.BlockTranslator
 */





trait TokenReplacement
{

  import   soda.translator.replacement.ReplacementWithTranslator_

  def replace (table : Seq [ Tuple2 [String, String] ] ) : TokenizedBlockTranslator =
    TokenizedBlockTranslator_ (
       token =>
        ReplacementWithTranslator_ (TableTranslator_ (table) ).replace (token.text)
    )

}

case class TokenReplacement_ () extends TokenReplacement
