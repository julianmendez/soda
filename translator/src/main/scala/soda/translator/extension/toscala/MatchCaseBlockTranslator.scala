package soda.translator.extension.toscala

trait MatchCaseBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToScala_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (block.lines
        .map (line => insert_match_before_brace_if_found (line ) ), block.block_annotation    )

  def insert_match_before_brace_if_found (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    ) _assemble_parts (index = line.indexOf (tc.soda_match_pattern ), line )
    else line

  def _assemble_parts (index: Int, line: String ): String =
    _left_part (index, line ) + _right_part (index, line ) + tc.scala_match_translation + tc.space + tc.scala_opening_brace

  def _left_part (index: Int, line: String ): String =
    line.substring (0, index )

  def _right_part (index: Int, line: String ): String =
    line.substring (index + tc.soda_match_pattern.length, line.length )

}

case class MatchCaseBlockTranslator_ ()  extends MatchCaseBlockTranslator
