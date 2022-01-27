package soda.translator.extension.toscala

/**
 * This tests how translation is done for Scala reserved words that are not Soda reserved words.
 */

case class ScalaNonSodaSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.parser.BlockProcessor_

  test ("Scala reserved words are replaced")
    {
      lazy val program = "" +
        "\nval x =" +
        "\n  while (x != 0)"
      lazy val expected = "" +
        "lazy val __soda__val x =" +
        "\n  __soda__while (x != 0 )" +
        "\n"
      lazy val obtained = BlockProcessor_ (MicroTranslatorToScala_ ()  ) .translate (program )
      assert (obtained == expected ) }

  test ("some synonyms are Scala reserved words")
    {
      lazy val program = "" +
        "\n* A0[B0 <: C0]" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass C0[D0 >: E0]" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass A1[B1 subtype C1]" +
        "\n" +
        "\nend" +
        "\n" +
        "\n* C1[D1 supertype E1]" +
        "\n" +
        "\nend" +
        "\n"
      lazy val expected = "" +
        "trait A0 [B0 <: C0]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ntrait C0 [D0 >: E0]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ntrait A1 [B1 <: C1]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ntrait C1 [D1 >: E1]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n"
      lazy val obtained = BlockProcessor_ (MicroTranslatorToScala_ ()  ) .translate (program )
      assert (obtained == expected ) }

}
