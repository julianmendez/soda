package soda.translator.extension.toscala

trait LetInBlockTranslator  extends soda.translator.block.BlockTranslator {

  import soda.translator.block.AnnotatedBlock
  import soda.translator.parser.BlockBuilder_

  lazy val tc = TranslationConstantToScala_ ()

  def translate (block: AnnotatedBlock ): AnnotatedBlock =
    BlockBuilder_ () .build (block.lines
        .map (line => replace_all_when (line, starts_with, tc.soda_in_let_pattern, tc.scala_in_let_translation ) )
        .map (line => replace_all_when (line, are_trim_equal, tc.soda_in_let_pattern.trim, tc.scala_in_let_translation ) )
        .map (line => append_if_condition (line, starts_with, tc.soda_in_pattern, tc.scala_in_translation ) )    )

  def replace_all_when (line: String, condition: (String, String ) => Boolean, pattern: String, new_text: String ): String =
    if (condition (line, pattern )
    ) line.replaceAll (pattern, new_text )
    else line

  def append_if_condition (line: String, condition: (String, String ) => Boolean, pattern: String, to_append: String ): String =
    if (condition (line, pattern )
    ) line + to_append
    else line

  def starts_with (line: String, pattern: String ): Boolean =
    line.trim.startsWith (pattern )

  def are_trim_equal (line: String, pattern: String ): Boolean =
    (line.trim == pattern.trim )

}

case class LetInBlockTranslator_ ()  extends LetInBlockTranslator
