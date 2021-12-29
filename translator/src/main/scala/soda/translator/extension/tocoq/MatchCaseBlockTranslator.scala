package soda.translator.extension.tocoq

trait MatchCaseBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val source = "soda"

  lazy val target = "soda (after match-case)"

  def translate (block: Block ): Block =
    BlockBuilder_ () .build (block.lines
        .map (line => append_with_after_match (line ) )    )

  def append_with_after_match (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    ) line + tc.space + tc.coq_with_reserved_word
    else line

}

case class MatchCaseBlockTranslator_ ()  extends MatchCaseBlockTranslator
