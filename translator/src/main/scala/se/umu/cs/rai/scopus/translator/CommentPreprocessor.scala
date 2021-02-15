package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec

case class AnnotatedLine(line: String, isComment: Boolean)

case class CommentPreprocessor() {

  val ScopusBeginComment = "/*"
  val ScopusEndComment = "*/"

  def annotate_lines(lines: Seq[String]): Seq[AnnotatedLine] =
    _identify_comments(lines, comment_state=false, Seq())

  @tailrec final
  def _identify_comments(lines: Seq[String], comment_state: Boolean, annotated_lines_rev: Seq[AnnotatedLine]): Seq[AnnotatedLine] =
    if ( lines.isEmpty
    ) annotated_lines_rev.reverse
    else {
      val line = lines.head
      val (current_state, new_comment_state) =
        if ( comment_state
        ) (true, !line.trim.endsWith(ScopusEndComment))
        else
          if ( line.trim.startsWith(ScopusBeginComment)
          ) (true, !line.trim.endsWith(ScopusEndComment))
          else (false, false)

      _identify_comments(lines.tail, new_comment_state, annotated_lines_rev.+:(AnnotatedLine(line, current_state)))
    }

}
