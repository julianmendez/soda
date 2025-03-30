package soda.translator.extension.tocoq

/*
 * This package contains test for the translator to Coq.
 */



case class CoqFullTranslationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   org.scalatest.Assertion
  import   soda.translator.block.DefaultBlockSequenceTranslator
  import   soda.translator.parser.BlockProcessor
  import   java.nio.file.Files
  import   java.nio.file.Paths

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val Base = "/soda/translator/example/"

  lazy val SodaSuffix = ".soda"

  lazy val CoqSuffix = ".v"

  lazy val SwapExample = "forcoq/algorithms/SwapExample"

  lazy val TriangularNumber = "forcoq/mathematics/TriangularNumberForCoq"

  lazy val Recursion = "forcoq/algorithms/Recursion"

  def read_file (file_name : String) : String =
    new String (
      Files .readAllBytes (
        Paths .get (getClass .getResource (file_name) .toURI)
      )
    )

  def test_translation_with (input_file_name : String) (expected_file_name : String) : Assertion =
    check (
      obtained =
        BlockProcessor .mk (
          DefaultBlockSequenceTranslator .mk (
            MicroTranslatorToCoq .mk
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

  test ("should translate the example of recursion") (
    test_translation (Recursion)
  )

}


case class MicroTranslatorToCoqSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator
  import   soda.translator.parser.BlockProcessor

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor .mk (
      DefaultBlockSequenceTranslator .mk (
        MicroTranslatorToCoq .mk
      )
    )

  test ("Coq translation of a constant 1") (
    check (
      obtained = instance .translate ("x = 0")
    ) (
      expected = "Definition x := 0\n.\n"
    )
  )

  test ("Coq translation of a constant 2") (
    check (
      obtained = instance .translate ("def x = 0")
    ) (
      expected = "Definition x := 0\n.\n"
    )
  )

  test ("Coq translation of a function 1") (
    check (
      obtained = instance .translate ("f (a : Nat) = 0")
    ) (
      expected = "Definition f (a : nat) := 0\n.\n"
    )
  )

  test ("Coq translation of a function 2") (
    check (
      obtained = instance .translate ("  def f (a : Nat) = 0")
    ) (
      expected = "Definition   f (a : nat) := 0\n.\n"
    )
  )

  test ("Coq translation of type parameters 1") (
    check (
      obtained = instance .translate (
      "identity (x : List [Boolean] ) : List [Boolean] = x\n")
    ) (
      expected = "Definition identity (x : list ( bool ) ) : list ( bool ) := x\n.\n"
    )
  )

  test ("Coq translation of type parameters 2") (
    check (
      obtained = instance .translate (
      " def identity (x : List [Boolean] ) : List [Boolean] = x\n")
    ) (
      expected = "Definition  identity (x : list ( bool ) ) : list ( bool ) := x\n.\n"
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
        "\n This is a nicely" +
        "\n written comment" +
        "\n" +
        "\n   f (x) = 0" +
        "\n" +
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
        "\n This is a nicely" +
        "\n written documentation" +
        "\n" +
        "\n   f (x) = 0" +
        "\n" +
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
        "Definition as_list (a : bool) : list ( bool ) :=" +
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

  test ("should translate the definition of the inductive data type Nat to Coq") (
    check (
      obtained = instance .translate ("datatype Nat" +
        "\n  Zero : Nat" +
        "\n  Succ : (n : Nat) -> Nat" +
        "\n"
      )
    ) (
      expected = "Inductive Nat : Type :=" +
        "\n  | Zero : Nat" +
        "\n  | Succ : (Nat) -> Nat" +
        "\n." +
        "\n"
    )
  )

  test ("should translate a simplified definition of the inductive data type Nat to Coq") (
    check (
      obtained = instance .translate ("datatype Nat" +
        "\n  Zero" +
        "\n  Succ (n : Nat)" +
        "\n"
      )
    ) (
      expected = "Inductive Nat : Type :=" +
        "\n  | Zero : Nat" +
        "\n  | Succ : (Nat) -> Nat" +
        "\n." +
        "\n"
    )
  )

  test ("should translate the definition of the inductive data type Seq1 to Coq") (
    check (
      obtained = instance .translate ("datatype Seq1 [A : Type]" +
        "\n  Nil : Seq1 [A]" +
        "\n  Cons : (elem : A) -> (seq : Seq1 [A] ) -> Seq1 [A]" +
        "\n"
      )
    ) (
      expected = "Inductive Seq1 (A : Type) : Type :=" +
        "\n  | Nil : Seq1 (A)" +
        "\n  | Cons : (A) -> (Seq1 (A) ) -> Seq1 (A)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate a simplified definition of the inductive data type Seq1 to Coq") (
    check (
      obtained = instance .translate ("datatype Seq1 [A : Type]" +
        "\n  Nil" +
        "\n  Cons (elem : A) (seq : Seq1 [A] )" +
        "\n"
      )
    ) (
      expected = "Inductive Seq1 (A : Type) : Type :=" +
        "\n  | Nil : Seq1 (A)" +
        "\n  | Cons : (A) -> (Seq1 (A) ) -> Seq1 (A)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate the definition of the inductive data type BTree to Coq") (
    check (
      obtained = instance .translate ("datatype BTree [A : Type]" +
        "\n  Empty : BTree [A]" +
        "\n  Node : (value : A) -> (left : BTree [A] ) -> (right : BTree [A] ) -> BTree [A]" +
        "\n"
      )
    ) (
      expected = "Inductive BTree (A : Type) : Type :=" +
        "\n  | Empty : BTree (A)" +
        "\n  | Node : (A) -> (BTree (A) ) -> (BTree (A) ) -> BTree (A)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate a simplified definition of the inductive data type BTree to Coq") (
    check (
      obtained = instance .translate ("datatype BTree [A : Type]" +
        "\n  Empty" +
        "\n  Node (value : A) (left : BTree [A] ) (right : BTree [A] )" +
        "\n"
      )
    ) (
      expected = "Inductive BTree (A : Type) : Type :=" +
        "\n  | Empty : BTree (A)" +
        "\n  | Node : (A) -> (BTree (A) ) -> (BTree (A) ) -> BTree (A)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate the definition of the data type Triple to Coq") (
    check (
      obtained = instance .translate ("datatype Triple [A : Type] [B : Type] [C : Type]" +
        "\n  Triple_ : (fst : A) -> (snd : B) -> (trd : C) -> Triple [A] [B] [C]" +
        "\n"
      )
    ) (
      expected = "Inductive Triple (A : Type) (B : Type) (C : Type) : Type :=" +
        "\n  | Triple_ : (A) -> (B) -> (C) -> Triple (A) (B) (C)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate a simplified definition of the data type Triple to Coq") (
    check (
      obtained = instance .translate ("datatype Triple [A : Type] [B : Type] [C : Type]" +
        "\n  Triple_ (fst : A) (snd : B) (trd : C)" +
        "\n"
      )
    ) (
      expected = "Inductive Triple (A : Type) (B : Type) (C : Type) : Type :=" +
        "\n  | Triple_ : (A) -> (B) -> (C) -> Triple (A) (B) (C)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate the definition of a more complex data type to Coq") (
    check (
      obtained = instance
          .translate ("datatype Composition [A : Type] [B : Type] [C : Type] [D : Type]" +
        "\n  Composition2 : (fst : A -> B) -> (snd : B -> C) -> Composition [A] [B] [C] [D]" +
        "\n  Composition3 : (fst : A -> B) -> (snd : B -> C) -> (trd : C -> D) -> Composition [A] [B] [C] [D]" +
        "\n"
      )
    ) (
      expected = "Inductive Composition (A : Type) (B : Type) (C : Type) (D : Type) : Type :=" +
        "\n  | Composition2 : (A -> B) -> (B -> C) -> Composition (A) (B) (C) (D)" +
        "\n  | Composition3 : (A -> B) -> (B -> C) ->" +
          " (C -> D) -> Composition (A) (B) (C) (D)" +
        "\n." +
        "\n"
    )
  )

  test ("should translate a simplified definition of a more complex data type to Coq") (
    check (
      obtained = instance
          .translate ("datatype Composition [A : Type] [B : Type] [C : Type] [D : Type]" +
        "\n  Composition2 (fst : A -> B) (snd : B -> C)" +
        "\n  Composition3 (fst : A -> B) (snd : B -> C) (trd : C -> D)" +
        "\n"
      )
    ) (
      expected = "Inductive Composition (A : Type) (B : Type) (C : Type) (D : Type) : Type :=" +
        "\n  | Composition2 : (A -> B) -> (B -> C) -> Composition (A) (B) (C) (D)" +
        "\n  | Composition3 : (A -> B) -> (B -> C) ->" +
          " (C -> D) -> Composition (A) (B) (C) (D)" +
        "\n." +
        "\n"
    )
  )

}

