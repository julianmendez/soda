package scopus.translator

import org.scalatest.funsuite.AnyFunSuite

import java.io.File

case class CommentPreprocessorSpec (  ) extends AnyFunSuite {

  /* This is to test how to find commented text. */
  test ("should find commented text") {
    lazy val input = ("\n" +
      "\n/** This is an example */" +
      "\n* Example() {" +
      "\n  /* This is a comment */" +
      "\n  a = \"/** this is not a comment */\"" +
      "\n}" +
      "\n")
      .split ("\n")
      .toSeq
    lazy val expected = Seq (
      AnnotatedLine ("", isComment=false )  ,      AnnotatedLine ("", isComment=false )  ,      AnnotatedLine ("/** This is an example */", isComment=true )  ,      AnnotatedLine ("* Example() {", isComment=false )  ,      AnnotatedLine ("  /* This is a comment */", isComment=true )  ,      AnnotatedLine ("  a = \"/** this is not a comment */\"", isComment=false )  ,      AnnotatedLine ("}", isComment=false )
    )
    lazy val obtained = CommentPreprocessor (  ) .annotate_lines ( input )
    assert ( obtained == expected )
  }

}
