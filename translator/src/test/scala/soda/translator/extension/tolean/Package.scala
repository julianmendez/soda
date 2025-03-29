package soda.translator.extension.tolean

/*
 * This package contains test for the translator to Lean.
 */



case class LeanFullTranslationSpec ()
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

  lazy val base = "/soda/translator/example/"

  lazy val soda_suffix = ".soda"

  lazy val lean_suffix = ".lean"

  lazy val pair_example = "forlean/algorithms/PairParam"

  lazy val swap_example = "forlean/algorithms/SwapExample"

  lazy val recursion_example = "forlean/algorithms/Recursion"

  lazy val list_example = "forlean/lib/MyList"

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
        BlockProcessor .mk (
          DefaultBlockSequenceTranslator .mk (
            MicroTranslatorToLean .mk
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

  test ("should translate the parameterized pair example") (
    test_translation (pair_example)
  )

  test ("should translate the recursion example") (
    test_translation (recursion_example)
  )
  test ("should translate the list example") (
    test_translation (list_example)
  )

}


case class MicroTranslatorToLeanSpec ()
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
        MicroTranslatorToLean .mk
      )
    )

  test ("Lean translation of a constant 1") (
    check (
      obtained = instance .translate ("x = 0")
    ) (
      expected = "def x := 0\n\n"
    )
  )

  test ("Lean translation of a constant 2") (
    check (
      obtained = instance .translate ("def x = 0")
    ) (
      expected = "def x := 0\n\n"
    )
  )

  test ("Lean translation of a constant 3") (
    check (
      obtained = instance .translate ("  def x = 0")
    ) (
      expected = "def   x := 0\n\n"
    )
  )

  test ("Lean translation of a function 1") (
    check (
      obtained = instance .translate ("f (a : Nat) = 0")
    ) (
      expected = "def f (a : Nat) := 0\n\n"
    )
  )

  test ("Lean translation of a function 2") (
    check (
      obtained = instance .translate ("def f (a : Nat) = 0")
    ) (
      expected = "def f (a : Nat) := 0\n\n"
    )
  )

  test ("Lean translation of type parameters 1") (
    check (
      obtained = instance .translate (
      "identity (x : List [Boolean] ) : List [Boolean] = x\n")
    ) (
      expected = "def identity (x : List ( Bool ) ) : List ( Bool ) := x\n\n"
    )
  )

  test ("Lean translation of type parameters 2") (
    check (
      obtained = instance .translate (
      "def identity (x : List [Boolean] ) : List [Boolean] = x\n")
    ) (
      expected = "def identity (x : List ( Bool ) ) : List ( Bool ) := x\n\n"
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
        "\n This is a nicely " +
        "\n written comment" +
        "\n" +
        "\n   f (x) = 0" +
        "\n" +
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
        "\n This is a nicely " +
        "\n written documentation" +
        "\n" +
        "\n   f (x) = 0" +
        "\n" +
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
        "notation \"Money\" => Int" +
        "\n"
    )
  )

  test ("Lean translation of class Pair") (
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
        "class Pair" +
        "\n" +
        "\nwhere" +
        "\n  mk ::" +
        "\n    first : Int" +
        "\n    second : Int" +
        "\n  deriving DecidableEq" +
        "\n" +
        "\nnamespace Pair" +
        "\n" +
        "\n" +
        "\nend Pair" +
        "\n" +
        "\nnotation \"Pair_\" => Pair.mk" +
        "\n"
    )
  )

  test ("Lean translation of class Pair with type parameters") (
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
        "class Pair ( A : Type ) ( B : Type )" +
        "\n" +
        "\nwhere" +
        "\n  mk ::" +
        "\n    first : A" +
        "\n    second : B" +
        "\n  deriving DecidableEq" +
        "\n" +
        "\nnamespace Pair" +
        "\n" +
        "\n" +
        "\nend Pair" +
        "\n" +
        "\nnotation \"Pair_\" => Pair.mk" +
        "\n"
    )
  )

  test ("Lean translation of package") (
    check (
      obtained = instance .translate (
        "package soda.forlean" +
        "\n"
      )
    ) (
      expected = "" +
        "/-" +
        "\npackage soda.forlean" +
        "\n-/" +
        "\nnotation \"Boolean\" => Bool" +
        "\nnotation \"None\" => Option.none" +
        "\nnotation \"Some\" => Option.some" +
        "\nnotation \"Seq\" => List" +
        "\nnotation \"Nil\" => List.nil" +
        "\nnotation \"Tuple2\" => Prod" +
        "\n"
    )
  )

  test ("Lean translation of types 1") (
    check (
      obtained = instance .translate (
        "as_list (a : Boolean) : List [Boolean] =" +
        "\n  a :: Nil" +
        "\n"
      )
    ) (
      expected = "" +
        "def as_list (a : Bool) : List ( Bool ) :=" +
        "\n  a :: List.nil" +
        "\n" +
        "\n"
    )
  )

  test ("Lean translation of types 2") (
    check (
      obtained = instance .translate (
        "as_seq (a : Boolean) : Seq [Boolean] =" +
        "\n  (a) +: (Nil)" +
        "\n"
      )
    ) (
      expected = "" +
        "def as_seq (a : Bool) : List ( Bool ) :=" +
        "\n  (a) :: (List.nil)" +
        "\n" +
        "\n"
    )
  )

  test ("Lean translation of types 3") (
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
        "class Example" +
        "\n" +
        "\nwhere" +
        "\n  mk ::" +
        "\n    bit : Bool" +
        "\n    sequence : List ( Bool )" +
        "\n  deriving DecidableEq" +
        "\n" +
        "\nnamespace Example" +
        "\n" +
        "\n" +
        "\nend Example" +
        "\n" +
        "\nnotation \"Example_\" => Example.mk" +
        "\n"
    )
  )

  test ("should translate the definition of the inductive data type Nat to Lean") (
    check (
      obtained = instance .translate ("datatype Nat" +
        "\n  Zero : Nat" +
        "\n  Succ : (n : Nat) -> Nat" +
        "\n"
      )
    ) (
      expected = "inductive Nat where" +
        "\n  | Zero : Nat" +
        "\n  | Succ : (n : Nat) -> Nat" +
        "\n"
    )
  )

  test ("should translate the definition of the inductive data type Seq1 to Lean") (
    check (
      obtained = instance .translate ("datatype Seq1 [A : Type]" +
        "\n  Nil : Seq1 [A]" +
        "\n  Cons : (elem : A) -> (seq : Seq1 [A] ) -> Seq1 [A]" +
        "\n"
      )
    ) (
      expected = "inductive Seq1 (A : Type) where" +
        "\n  | Nil : Seq1 (A)" +
        "\n  | Cons : (elem : A) -> (seq : Seq1 (A) ) -> Seq1 (A)" +
        "\n"
    )
  )

  test ("should translate the definition of the inductive data type BTree to Lean") (
    check (
      obtained = instance .translate ("datatype BTree [A : Type]" +
        "\n  Empty : BTree [A]" +
        "\n  Node : (value : A) -> (left : BTree [A] ) -> (right : BTree [A] ) -> BTree [A]" +
        "\n"
      )
    ) (
      expected = "inductive BTree (A : Type) where" +
        "\n  | Empty : BTree (A)" +
        "\n  | Node : (value : A) -> (left : BTree (A) ) -> (right : BTree (A) ) -> BTree (A)" +
        "\n"
    )
  )

  test ("should translate the definition of the data type Triple to Lean") (
    check (
      obtained = instance .translate ("datatype Triple [A : Type] [B : Type] [C : Type]" +
        "\n  Triple_ : (fst : A) -> (snd : B) -> (trd : C) -> Triple [A] [B] [C]" +
        "\n"
      )
    ) (
      expected = "inductive Triple (A : Type) (B : Type) (C : Type) where" +
        "\n  | Triple_ : (fst : A) -> (snd : B) -> (trd : C) -> Triple (A) (B) (C)" +
        "\n"
    )
  )

  test ("should translate the definition of a more complex data type to Lean") (
    check (
      obtained = instance
          .translate ("datatype Composition [A : Type] [B : Type] [C : Type] [D : Type]" +
        "\n  Composition2 : (fst : A -> B) -> (snd : B -> C) -> Composition [A] [B] [C] [D]" +
        "\n  Composition3 : (fst : A -> B) -> (snd : B -> C) -> (trd : C -> D) -> Composition [A] [B] [C] [D]" +
        "\n"
      )
    ) (
      expected = "inductive Composition (A : Type) (B : Type) (C : Type) (D : Type) where" +
        "\n  | Composition2 : (fst : A -> B) -> (snd : B -> C) -> Composition (A) (B) (C) (D)" +
        "\n  | Composition3 : (fst : A -> B) -> (snd : B -> C) ->" +
          " (trd : C -> D) -> Composition (A) (B) (C) (D)" +
        "\n"
    )
  )

}

