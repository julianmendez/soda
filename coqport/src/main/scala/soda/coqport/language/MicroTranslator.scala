package soda.coqport.language


/**
 * This class translates Soda snippets into Coq snippets.
 */
trait MicroTranslator {
  import soda.lib.SomeSD_
  import soda.translator.replacement.CommentPreprocessor_

  lazy val tc = TranslationConstant_ ()

  lazy val new_line = "\n"

  lazy val mtr = soda.translator.language.MicroTranslator_ ()

  def translate_program (program: String ): String =
    SomeSD_ (program )
      .map (mtr.split_lines )
      .map (mtr.join_lines_with_forward_join )
      .map (mtr.join_lines_with_backward_join )
      .map (translate_lines )
      .map (mtr.join_translated_lines )
      .value

  def translate_lines (lines: Seq [String]  ): Seq [String] =
    CommentPreprocessor_ (lines )
      .annotated_lines
      .map (annotated_line =>
        if (annotated_line.isComment
        ) annotated_line.line
        else _translate_non_comment (annotated_line.line )      )

  def _translate_non_comment (line: String ): String =
      SomeSD_ (line )
        .value
}

case class MicroTranslator_ () extends MicroTranslator
