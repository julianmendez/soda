package soda.translator.extension.tocoq

trait MatchCaseBlockTranslator
  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.block.BlockAnnotationEnum_
  import soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToCoq_ ()

  lazy val _labels = BlockAnnotationEnum_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == _labels.function_definition ||
      block.block_annotation == _labels.test_declaration
    ) _translate_block (block )
    else block

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block.lines
        .map (line => append_with_after_match (line ) ), block.block_annotation
    )

  def append_with_after_match (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    ) line + tc.space + tc.coq_with_reserved_word
    else line

}

case class MatchCaseBlockTranslator_ ()
  extends MatchCaseBlockTranslator
