package soda.translator.extension.toscala

trait MatchCaseBlockTranslator
  extends
    soda.translator.block.BlockTranslator
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToScala_ ()

  lazy val _labels = BlockAnnotationEnum_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    if (block.block_annotation == _labels.function_definition ||
      block.block_annotation == _labels.test_declaration
    ) _translate_block (block )
    else block

  def _translate_block (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (
      block.lines
        .map (line => insert_match_before_brace_if_found (line ) )
        .map (line => replace_match_end_if_found (line ) ),
      block.block_annotation
    )

  def insert_match_before_brace_if_found (line: String ): String =
    if (line.trim.startsWith (tc.soda_match_pattern )
    ) _assemble_parts (index = line.indexOf (tc.soda_match_pattern ), line )
    else line

  def replace_match_end_if_found (line: String ): String =
    if (line.trim == tc.soda_match_end_reserved_word
    ) line.replaceAll (tc.soda_match_end_reserved_word, tc.scala_match_end_translation )
    else line

  def _assemble_parts (index: Int, line: String ): String =
    _left_part (index, line ) + _right_part (index, line ) + tc.scala_match_translation + tc.space + tc.scala_opening_brace

  def _left_part (index: Int, line: String ): String =
    line.substring (0, index )

  def _right_part (index: Int, line: String ): String =
    line.substring (index + tc.soda_match_pattern.length, line.length )

}

case class MatchCaseBlockTranslator_ ()
  extends
    MatchCaseBlockTranslator
{

}
