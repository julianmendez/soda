package se.umu.cs.rai.scopus.translator

import scala.annotation.tailrec

case class AnnotatedLine(line: String, isComment: Boolean)

case class CommentPreprocessor() {

  val ScopusBeginComment = "/*"
  val ScopusEndComment = "*/"

  def annotateLines(lines: Seq[String]): Seq[AnnotatedLine] =
    identifyComments(lines, commentState = false, Seq())

  @tailrec
  final def identifyComments(lines: Seq[String], commentState: Boolean, annotatedLinesRev: Seq[AnnotatedLine]): Seq[AnnotatedLine] = {
    if (lines.isEmpty) {
      annotatedLinesRev.reverse
    } else {
      val line = lines.head
      val (currentState, newCommentState) = if (commentState) {
        (true, !line.trim.endsWith(ScopusEndComment))
      } else {
        if (line.trim.startsWith(ScopusBeginComment)) {
          (true, !line.trim.endsWith(ScopusEndComment))
        } else {
          (false, false)
        }
      }
      identifyComments(lines.tail, newCommentState, AnnotatedLine(line, currentState) +: annotatedLinesRev)
    }
  }

}

