package soda.translator.extension.tocoq

/*
 * This package contains test for the translator to Coq.
 */

case class MicroTranslatorToCoqSpec ()
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
        MicroTranslatorToCoq_ ()
      )
    )

  test ("Coq translation of a constant") (
    check (
      obtained = instance.translate ("x = 0")
    ) (
      expected = " Definition x := 0\n.\n"
    )
  )

  test ("Coq translation of a function") (
    check (
      obtained = instance.translate ("f (a: nat) = 0")
    ) (
      expected = " Definition f (a: nat) := 0\n.\n"
    )
  )

}
