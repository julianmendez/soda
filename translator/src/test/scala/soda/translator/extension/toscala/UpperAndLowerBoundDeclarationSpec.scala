package soda.translator.extension.toscala

case class UpperAndLowerBoundDeclarationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  test ("should translate a single upper bound") (
    check (
      obtained = instance.translate ("class BlackBox()" +
        "\n  extends " +
        "\n    AbstractBlackBox[A subtype AbstractInput]" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected =
        "case class BlackBox()" +
        "\n  extends" +
        "\n    AbstractBlackBox[A <: AbstractInput]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n"
    )
  )

  test ("should translate multiple upper bounds") (
    check (
      obtained = instance.translate (        "  class BlackBox()" +
        "\n    extends " +
        "\n      AbstractBlackBox[A subtype AbstractInput]" +
        "\n      AbstractDevice[B subtype AbstractDeviceInput]" +
        "\n" +
        "\n  end" +
        "\n"
      )
    ) (
      expected = "  case class BlackBox()" +
        "\n    extends" +
        "\n      AbstractBlackBox[A <: AbstractInput]" +
        "\n      with AbstractDevice[B <: AbstractDeviceInput]" +
        "\n  {" +
        "\n" +
        "\n  }" +
        "\n"
    )
  )

  test ("should translate a single lower bound") (
    check (
      obtained = instance.translate (" class BlackBox()" +
        "\n   extends " +
        "\n     AbstractBlackBox[A supertype (AbstractInput)]" +
        "\n" +
        "\n end" +
        "\n"
      )
    ) (
      expected = " case class BlackBox()" +
        "\n   extends" +
        "\n     AbstractBlackBox[A >: (AbstractInput)]" +
        "\n {" +
        "\n" +
        "\n }" +
        "\n"
    )
  )

  test ("should translate multiple lower bounds") (
    check (
      obtained = instance.translate ("  class BlackBox()" +
        "\n  extends " +
        "\n    AbstractBlackBox[A supertype (AbstractInput)]" +
        "\n    AbstractDevice[B supertype (AbstractDeviceInput)]" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected = "  case class BlackBox()" +
        "\n  extends" +
        "\n    AbstractBlackBox[A >: (AbstractInput)]" +
        "\n    with AbstractDevice[B >: (AbstractDeviceInput)]" +
        "\n  {" +
        "\n" +
        "\n}" +
        "\n"
    )
  )

}
