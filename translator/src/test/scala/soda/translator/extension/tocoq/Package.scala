package soda.translator.extension.tocoq

/*
 * This package contains test for the translator to Coq.
 */
trait Package

case class CoqFullTranslationSpec ()
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

  lazy val Base = "/soda/translator/example/"

  lazy val SodaSuffix = ".soda"

  lazy val CoqSuffix = ".v"

  lazy val SwapExample = "forcoq/algorithms/SwapExample"

  lazy val TriangularNumber = "forcoq/mathematics/TriangularNumberForCoq"

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
            MicroTranslatorToCoq_()
          )
        ) .translate (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = Base + file_name + SodaSuffix) (
      expected_file_name = Base + file_name + CoqSuffix)

  test ("should translate the swap example") (
    test_translation (SwapExample)
  )

  test ("should translate the example of triangular numbers") (
    test_translation (TriangularNumber)
  )

}


case class MicroTranslatorToCoqSpec ()
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
        MicroTranslatorToCoq_ ()
      )
    )

  test ("Coq translation of a constant") (
    check (
      obtained = instance .translate ("x = 0")
    ) (
      expected = " Definition x := 0\n.\n"
    )
  )

  test ("Coq translation of a function") (
    check (
      obtained = instance .translate ("f (a : Nat) = 0")
    ) (
      expected = " Definition f (a : nat) := 0\n.\n"
    )
  )

  test ("Coq translation of type parameters") (
    check (
      obtained = instance .translate (
      "identity (x : List [Boolean] ) : List [Boolean] = x\n")
    ) (
      expected = " Definition identity (x : list ( bool ) ) : list ( bool ) := x\n.\n"
    )
  )

  test ("Coq translation of abstract parameters") (
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
        "\n    list0 : list ( prod ( bool ) ( bool )  )" +
        "\n    list1 : list ( list ( Int )  )" +
        "\n"
    )
  )

  test ("Coq translation of comments") (
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
        "(*" +
        "This is a nicely" +
        "\nwritten comment" +
        "\n" +
        "\n    f (x) = 0" +
        "\n*)" +
        "\n"
    )
  )

  test ("Coq translation of documentation") (
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
        "(**" +
        "This is a nicely" +
        "\nwritten documentation" +
        "\n" +
        "\n    f (x) = 0" +
        "\n*)" +
        "\n"
    )
  )

  test ("Coq translation of class alias") (
    check (
      obtained = instance .translate (
        "class Money = Int" +
        "\n"
      )
    ) (
      expected = "" +
        "Notation \"'Money'\" := Int (at level 99) ." +
        "\n"
    )
  )

  test ("Coq translation of class Pair") (
    check (
      obtained = instance .translate (
        "\nclass Pair" +
        "\n" +
        "\n  abstract" +
        "\n    first : Int" +
        "\n    second : Int" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected = "" +
        "Module Pair ." +
        "\n" +
        "\nClass Pair : Type :=" +
        "\n" +
        "\n  mk {" +
        "\n    first : Int ;" +
        "\n    second : Int" +
        "\n} ." +
        "\n" +
        "\nNotation \"'Pair_'\" := Pair.mk (at level 99) ." +
        "\n" +
        "\nEnd Pair ." +
        "\n" +
        "\nImport Pair ." +
        "\n"
    )
  )

  test ("Coq translation of class Pair with type parameters") (
    check (
      obtained = instance .translate (
        "\nclass Pair [A : Type] [B : Type]" +
        "\n" +
        "\n  abstract" +
        "\n    first : A" +
        "\n    second : B" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected = "" +
        "Module Pair ." +
        "\n" +
        "\nClass Pair ( A : Type ) ( B : Type ) : Type :=" +
        "\n" +
        "\n  mk {" +
        "\n    first : A ;" +
        "\n    second : B" +
        "\n} ." +
        "\n" +
        "\nNotation \"'Pair_'\" := Pair.mk (at level 99) ." +
        "\n" +
        "\nEnd Pair ." +
        "\n" +
        "\nImport Pair ." +
        "\n"
    )
  )

  test ("Coq translation of package") (
    check (
      obtained = instance .translate (
        "package soda.forcoq" +
        "\n"
      )
    ) (
      expected = "" +
        "(*" +
        "\npackage soda.forcoq" +
        "\n*)" +
        "\n" +
        "\nRequire Import Coq.ZArith.BinInt ." +
        "\n(* https://coq.inria.fr/library/Coq.ZArith.BinInt.html *)" +
        "\n" +
        "\nRequire Import Coq.Lists.List ." +
        "\n(* https://coq.inria.fr/library/Coq.Lists.List.html *)" +
        "\n" +
        "\nNotation Int := Z ." +
        "\n" +
        "\n"
    )
  )

  test ("Coq translation of types 1") (
    check (
      obtained = instance .translate (
        "as_list (a : Boolean) : List [Boolean] =" +
        "\n  (a) :: (Nil)" +
        "\n"
      )
    ) (
      expected = "" +
        " Definition as_list (a : bool) : list ( bool ) :=" +
        "\n  (a) :: (nil)" +
        "\n." +
        "\n"
    )
  )

  test ("Coq translation of types 2") (
    check (
      obtained = instance .translate (
        "class Example" +
        "\n" +
        "\n  abstract" +
        "\n    bit : Boolean" +
        "\n    sequence : Seq [Boolean]" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected = "" +
        "Module Example ." +
        "\n" +
        "\nClass Example : Type :=" +
        "\n" +
        "\n  mk {" +
        "\n    bit : bool ;" +
        "\n    sequence : list ( bool )" +
        "\n} ." +
        "\n" +
        "\nNotation \"'Example_'\" := Example.mk (at level 99) ." +
        "\n" +
        "\nEnd Example ." +
        "\n" +
        "\nImport Example ." +
        "\n"
    )
  )

}

