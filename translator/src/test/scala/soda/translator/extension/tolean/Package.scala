package soda.translator.extension.tolean

/*
 * This package contains test for the translator to Lean.
 */
trait Package

case class LeanFullTranslationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   org.scalatest.Assertion
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_
  import   java.nio.file.Files
  import   java.nio.file.Paths

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val base = "/soda/translator/example/"

  lazy val soda_suffix = ".soda"

  lazy val lean_suffix = ".lean"

  lazy val swap_example = "forlean/algorithms/SwapExample"

  lazy val triangular_number = "forlean/mathematics/TriangularNumberForLean"

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  def test_translation_with (input_file_name : String) (expected_file_name : String) : Assertion =
    check (
      obtained =
        BlockProcessor_(
          DefaultBlockSequenceTranslator_ (
            MicroTranslatorToLean_()
          )
        ) .translate (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = base + file_name + soda_suffix) (
      expected_file_name = base + file_name + lean_suffix)

  test ("should translate the swap example") (
    test_translation (swap_example)
  )

  test ("should translate the example of triangular numbers") (
    test_translation (triangular_number)
  )

}


case class MicroTranslatorToLeanSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToLean_ ()
      )
    )

  test ("Lean translation of a constant") (
    check (
      obtained = instance .translate ("x = 0")
    ) (
      expected = " def x := 0\n\n"
    )
  )

  test ("Lean translation of a function") (
    check (
      obtained = instance .translate ("f (a : Nat) = 0")
    ) (
      expected = " def f (a : Nat) := 0\n\n"
    )
  )

  test ("Lean translation of type parameters") (
    check (
      obtained = instance .translate (
      "identity (x : List [Boolean] ) : List [Boolean] = x\n")
    ) (
      expected = " def identity (x : List ( Bool ) ) : List ( Bool ) := x\n\n"
    )
  )

  test ("Lean translation of abstract parameters") (
    check (
      obtained = instance .translate (
        "\n  abstract" +
        "\n    list0 : List [Tuple2 [Boolean] [Boolean] ]" +
        "\n    list1 : List [List [Int] ]" +
        "\n"
      )
    ) (
      expected = "" +
        "  abstract" +
        "\n    list0 : List ( Prod ( Bool ) ( Bool )  )" +
        "\n    list1 : List ( List ( Int )  )" +
        "\n"
    )
  )

  test ("Lean translation of comments") (
    check (
      obtained = instance .translate (
        "\n/*" +
        "\n * This is a nicely " +
        "\n * written comment" +
        "\n * " +
        "\n   " +
        "\n   f (x) = 0" +
        "\n   " +
        "\n */" +
        "\n"
      )
    ) (
      expected = "" +
        "/-" +
        "This is a nicely " +
        "\nwritten comment" +
        "\n" +
        "\n    f (x) = 0" +
        "\n-/" +
        "\n"
    )
  )

  test ("Lean translation of documentation") (
    check (
      obtained = instance .translate (
        "\n/**" +
        "\n * This is a nicely " +
        "\n * written documentation" +
        "\n * " +
        "\n   " +
        "\n   f (x) = 0" +
        "\n   " +
        "\n */" +
        "\n"
      )
    ) (
      expected = "" +
        "/--" +
        "This is a nicely " +
        "\nwritten documentation" +
        "\n" +
        "\n    f (x) = 0" +
        "\n-/" +
        "\n"
    )
  )

  test ("Lean translation of class alias") (
    check (
      obtained = instance .translate (
        "class Money = Int" +
        "\n"
      )
    ) (
      expected = "" +
        "notation:max \"Money\" => Int" +
        "\n"
    )
  )

}

