package soda.translator.extension.toscala

trait MatchCaseBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.Block
  import soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToScala_ ()

  def translate (block: Block ): Block =
    BlockBuilder_ () .build (block.lines
        .map (line => insert_match_before_brace_if_found (line ) )    )

  def insert_match_before_brace_if_found (line: String ): String =
    if (line.trim () .startsWith (tc.soda_match_pattern )
    )
      {
        lazy val index_of_match = line.indexOf (tc.soda_match_pattern )
        lazy val left_part = line.substring (0, index_of_match )
        lazy val right_part = line.substring (index_of_match + tc.soda_match_pattern.length, line.length )
        left_part + right_part + tc.scala_match_translation + tc.space + tc.scala_opening_brace }
    else line

}

case class MatchCaseBlockTranslator_ ()  extends MatchCaseBlockTranslator
