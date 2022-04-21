package soda.translator.extension.todoc

case class MicroTranslatorToDocSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToDoc_ ()
      )
    )

  lazy val original_snippet = "" +
    "/*" +
    "\n * This is an example" +
    "\n*/" +
    "\n"

  lazy val translated_snippet = "" +
    "\n\\end{lstlisting}" +
    "\n" +
    "\nThis is an example" +
    "\n" +
    "\n" +
    "\n\\begin{lstlisting}" +
    "\n" +
    "\n"

  test ("Document generation of a snippet") (
    check (
      obtained = instance.translate (original_snippet)
    ) (
      expected = translated_snippet
    )
  )

}
