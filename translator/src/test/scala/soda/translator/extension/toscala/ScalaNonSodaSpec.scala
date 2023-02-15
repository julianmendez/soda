package soda.translator.extension.toscala

/*
 * This package contains tests for the translator to Scala.
 */

/**
 * This tests how translation is done for Scala reserved words that are not Soda reserved words.
 */

case class ScalaNonSodaSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val bp =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  test ("Scala reserved words are replaced") (
    check (
      obtained = bp.translate ("" +
        "\nval x =" +
        "\n  while (x != 0)"
      )
    ) (
      expected = "" +
        "private lazy val __soda__val x =" +
        "\n  __soda__while (x != 0)" +
        "\n"
    )
  )

  test ("some synonyms are Scala reserved words") (
    check (
      obtained = bp.translate ("" +
        "\nclass A0 [B0 <: C0]" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass C0 [D0 >: E0]" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass A1 [B1 subtype C1]" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass C1 [D1 supertype E1]" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected = "" +
        "trait A0 [B0 <: C0]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class A0_ [B0 <: C0] () extends A0 [B0]" +
        "\n" +
        "\ntrait C0 [D0 >: E0]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class C0_ [D0 >: E0] () extends C0 [D0]" +
        "\n" +
        "\ntrait A1 [B1 <: C1]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class A1_ [B1 <: C1] () extends A1 [B1]" +
        "\n" +
        "\ntrait C1 [D1 >: E1]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class C1_ [D1 >: E1] () extends C1 [D1]" +
        "\n"
    )
  )

}
