package soda.translator.extension.toscala

/**
 * This tests how translation is done for Scala reserved words that are not Soda reserved words.
 */
case class ScalaNonSodaSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  import soda.translator.parser.BlockProcessor_

  test ("Scala reserved words are replaced")
    {
      lazy val program = "" +
        "\nval x = 0" +
        "\nwhile (x != 0)"
      lazy val expected = "" +
        "\nlazy val __soda__val x = 0" +
        "\n__soda__while (x != 0 )" +
        "\n"
      lazy val obtained = BlockProcessor_ (MicroTranslatorToScala_ ()  ) .translate (program )
      assert (obtained == expected ) }

  test ("some synonyms are Scala reserved words")
    {
      lazy val program = "" +
        "\n* A0[B0 <: C0]" +
        "\n" +
        "\nclass C0[D0 >: E0]" +
        "\n" +
        "\nclass A1[B1 subtype C1]" +
        "\n" +
        "\n* C1[D1 supertype E1]"
      lazy val expected = "" +
        "\ntrait A0 [B0 <: C0]" +
        "\n" +
        "\ntrait C0 [D0 >: E0]" +
        "\n" +
        "\ntrait A1 [B1 <: C1]" +
        "\n" +
        "\ntrait C1 [D1 >: E1]" +
        "\n"
      lazy val obtained = BlockProcessor_ (MicroTranslatorToScala_ ()  ) .translate (program )
      assert (obtained == expected ) }

}
