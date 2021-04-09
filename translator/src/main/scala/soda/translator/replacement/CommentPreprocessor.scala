package soda.translator.replacement

import soda.lib.Rec

case class AnnotatedLine (line: String, isComment: Boolean )

/**
 * This preprocessor annotates lines to determine whether they are comments.
 */
case class CommentPreprocessor () {

  lazy val SodaBeginComment = "/*"
  lazy val SodaEndComment = "*/"

  def annotate_lines (lines: Seq [String]  ): Seq [AnnotatedLine] = {
    lazy val result =
      Rec () .foldLeft (lines, initial_value, next_value )
        .annotated_lines_rev
        .reverse

    case class FoldTuple (comment_state: Boolean, annotated_lines_rev: Seq [AnnotatedLine]  )

    lazy val initial_value = FoldTuple (false, Seq ()  )

    def next_value (pair: FoldTuple, line: String ): FoldTuple = {
      lazy val (current_state, new_comment_state ) = annotate_this_line (line, pair.comment_state )
      FoldTuple (new_comment_state, pair.annotated_lines_rev.+: (AnnotatedLine (line, current_state )  )  )
    }

    def annotate_this_line (line: String, comment_state: Boolean ): (Boolean, Boolean ) =
      if (comment_state
      ) (true, ! line.trim.endsWith (SodaEndComment )  )
      else
        if (line.trim.startsWith (SodaBeginComment )
        ) (true, ! line.trim.endsWith (SodaEndComment )  )
        else (false, false )

    result
  }

}
