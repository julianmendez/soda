package soda.translator.extension.tocoq

trait MatchCaseBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToCoq_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (block.lines
        .map (line => append_with_after_match (line ) ), block.block_annotation    )

  def append_with_after_match (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    ) line + tc.space + tc.coq_with_reserved_word
    else line

}

case class MatchCaseBlockTranslator_ ()  extends MatchCaseBlockTranslator
