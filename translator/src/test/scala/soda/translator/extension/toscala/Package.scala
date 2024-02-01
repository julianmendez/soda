package soda.translator.extension.toscala

/*
 * This package contains tests for the translator to Scala.
 */



case class BeautifierSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val original = "  beautify_this  (  original  : String   )   :  String   =  \n" +
    "    original .  replaceAll(\"  \" ,  \" \")   \n"

  test ("the translated source code should respect unnecessary spaces") (
    check (
      obtained =
        BlockProcessor_(
          DefaultBlockSequenceTranslator_ (
            MicroTranslatorToScala_()
          )
        ) .translate (original)
    ) (
      expected = "  def beautify_this  (  original  : String   )   :  String   =  \n" +
        "    original .  replaceAll(\"  \" ,  \" \")   \n"
    )
  )

}


case class FullTranslationSpec ()
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

  lazy val ScalaSuffix = ".scala"

  lazy val SwapExample = "forlean/algorithms/SwapExample"

  lazy val FiboExample = "mathematics/FiboExample"

  lazy val FactorialConcise = "mathematics/FactorialConcise"

  lazy val FactorialWithFold = "mathematics/FactorialWithFold"

  lazy val Fairness = "ethicalissues/fairness/Fairness"

  lazy val PiIterator = "mathematics/PiIterator"

  lazy val SaladMaker = "algorithms/SaladMaker"

  lazy val FizzBuzzUnicode = "algorithms/FizzBuzzUnicode"

  lazy val FiboUnicodeExample = "mathematics/FiboUnicodeExample"

  lazy val NatExample = "mathematics/Nat"

  lazy val SimpleListExample = "forlean/lib/SimpleList"

  lazy val ScalaReservedWordEscaping = "algorithms/ScalaReservedWordEscaping"

  lazy val InANutshell = "inanutshell/InANutshell"

  lazy val ManualInput = "/soda/translator/documentation/Manual.soda"

  lazy val ManualExpected = "/soda/translator/documentation/Manual.scala"

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
            MicroTranslatorToScala_()
          )
        ) .translate (read_file (input_file_name) )
    ) (
      expected = read_file (expected_file_name)
    )

  def test_translation (file_name : String) : Assertion =
    test_translation_with (input_file_name = Base + file_name + SodaSuffix) (
      expected_file_name = Base + file_name + ScalaSuffix)

  test ("should translate the swap example") (
    test_translation (SwapExample)
  )

  test ("should translate the Fibonacci example") (
    test_translation (FiboExample)
  )

  test ("should translate the Factorial Concise example") (
    test_translation (FactorialConcise)
  )

  test ("should translate the Factorial With Fold example") (
    test_translation (FactorialWithFold)
  )

  test ("should translate the Fairness example") (
    test_translation (Fairness)
  )

  test ("should translate the example that calculates pi") (
    test_translation (PiIterator)
  )

  test ("should translate the Salad Maker example") (
    test_translation (SaladMaker)
  )

  test ("should translate the FizzBuzz Unicode example") (
    test_translation (FizzBuzzUnicode)
  )

  test ("should translate Soda code that uses Scala reserved words") (
    test_translation (ScalaReservedWordEscaping)
  )

  test ("should translate the Nat example") (
    test_translation (NatExample)
  )

  test ("should translate the example of a simple list") (
    test_translation (SimpleListExample)
  )

  test ("should translate the manual In A Nutshell") (
    test_translation (InANutshell)
  )

  test ("should translate the manual") (
    test_translation_with (ManualInput) (ManualExpected)
  )

}


trait ExampleWithWrongOrder
{



  lazy val this_is_null_but = constant_defined_later

  lazy val constant_defined_later = "Success!"

}

case class ExampleWithWrongOrder_ () extends ExampleWithWrongOrder

object ExampleWithWrongOrder {
  def mk : ExampleWithWrongOrder =
    ExampleWithWrongOrder_ ()
}

trait ExampleWithRightOrder
{



  lazy val constant_defined_before = "Success!"

  lazy val this_is_not_null = constant_defined_before

}

case class ExampleWithRightOrder_ () extends ExampleWithRightOrder

object ExampleWithRightOrder {
  def mk : ExampleWithRightOrder =
    ExampleWithRightOrder_ ()
}

trait ExampleWithEmptyParentheses
{



  def this_is_not_null  () = constant_defined_later

  lazy val constant_defined_later = "Success!"

}

case class ExampleWithEmptyParentheses_ () extends ExampleWithEmptyParentheses

object ExampleWithEmptyParentheses {
  def mk : ExampleWithEmptyParentheses =
    ExampleWithEmptyParentheses_ ()
}

trait AnotherExampleWithEmptyParentheses
{



  lazy val this_is_not_null = constant_function_defined_later  ()

  def constant_function_defined_later  () = "Success!"

}

case class AnotherExampleWithEmptyParentheses_ () extends AnotherExampleWithEmptyParentheses

object AnotherExampleWithEmptyParentheses {
  def mk : AnotherExampleWithEmptyParentheses =
    AnotherExampleWithEmptyParentheses_ ()
}

/**
 * In Soda constants cannot be defined as 'lazy val'.
 * These tests detect and test this problem, and test workarounds.
 */

case class LazySyntaxSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("should show what happens when constants are defined in the wrong order") (
    check (
      obtained = ExampleWithWrongOrder_ () .this_is_null_but
    ) (
      expected = "Success!"
    )
  )

  test ("should show what happens when constants are defined in the right order") (
    check (
      obtained = ExampleWithRightOrder_ () .this_is_not_null
    ) (
      expected = "Success!"
    )
  )

  test ("should show what happens when one work-around is used") (
    check (
      obtained = ExampleWithEmptyParentheses_ () .this_is_not_null  ()
    ) (
      expected = "Success!"
    )
  )

  test ("should show what happens when another work-around is used") (
    check (
      obtained = AnotherExampleWithEmptyParentheses_ () .this_is_not_null
    ) (
      expected = "Success!"
    )
  )

}


case class MicroTranslatorToScalaSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  test ("should translate a small snippet") (
    check (
      obtained = instance .translate ("  input_lines = Seq (" +
        "\n    \"  f ( x : Int,\\t\", " +
        "\n    \"     y : Int) =\"," +
        "\n    \"       x + y\")" +
        "\n"
      )
    ) (
      expected = "  lazy val input_lines = Seq (" +
        "\n    \"  f ( x : Int,\\t\", " +
        "\n    \"     y : Int) =\"," +
        "\n    \"       x + y\")" +
        "\n"
    )
  )

  test ("should translate a creation of instance with parameters") (
    check (
      obtained = instance .translate ("    min_max (a : Int) (b : Int) : MinMaxPair =" +
        "\n      MinMaxPair_ (" +
        "        min := MaxAndMin_ () .min (a) (b) ) (" +
        "        max := MaxAndMin_ () .max (a) (b)" +
        "      )" +
        "\n"
      )
    ) (
      expected = "    def min_max (a : Int) (b : Int) : MinMaxPair =" +
        "\n      MinMaxPair_ (" +
        "        min = MaxAndMin_ () .min (a) (b) , " +
        "        max = MaxAndMin_ () .max (a) (b)" +
        "      )" +
        "\n"
    )
  )

  test ("should leave content of apostrophes unchanged") (
    check (
      obtained = instance .translate (
        " a = Seq ('\\'', \'', '\\\"', ' or ', \'or\', '0x00', '->', '/*', '*/')\n")
    ) (
      expected =
        " lazy val a = Seq ('\\'', '', '\\\"', ' or ', 'or', '0x00', '->', '/*', '*/')\n"
    )
  )

  test ("should leave content of quotation marks unchanged") (
    check (
      obtained = instance .translate (" a = Seq (\"\\\"\", \"\", \"\\\'\", \"" +
        " or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n" )
    ) (
      expected = " lazy val a = Seq (\"\\\"\", \"\", \"\\'\", \"" +
        " or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n"
    )
  )

  test ("should translate classes") (
    check (
      obtained = instance .translate ("class D" +
        "\n" +
        "\n  f (x : Int) : Int = x + 1" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass E" +
        "\n" +
        "\n  g (x : Int) : Int = 2 * x" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass F ()" +
        "\n  extends" +
        "\n    D" +
        "\n" +
        "\n  abstract" +
        "\n    /** name for this object */" +
        "\n    name : String" +
        "\n    /**" +
        "\n     * value for this object" +
        "\n     */" +
        "\n    value : Int" +
        "\n" +
        "\n  h [B : Type] (x : Int) : Int = 2 * x + 1" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass E [A : Type]" +
        "\n" +
        "\n  i (x : A) : A = x" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected =
        "trait D" +
        "\n{" +
        "\n" +
        "\n  def f (x : Int) : Int = x + 1" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class D_ () extends D" +
        "\n" +
        "\nobject D {" +
        "\n  def mk : D =" +
        "\n    D_ ()" +
        "\n}" +
        "\n" +
        "\ntrait E" +
        "\n{" +
        "\n" +
        "\n  def g (x : Int) : Int = 2 * x" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class E_ () extends E" +
        "\n" +
        "\nobject E {" +
        "\n  def mk : E =" +
        "\n    E_ ()" +
        "\n}" +
        "\n" +
        "\ncase class F ()" +
        "\n  extends" +
        "\n    D" +
        "\n{" +
        "\n" +
        "\n    /** name for this object */" +
        "\n  def   name : String" +
        "\n    /**" +
        "\n     * value for this object" +
        "\n     */" +
        "\n  def   value : Int" +
        "\n" +
        "\n  def h [B ] (x : Int) : Int = 2 * x + 1" +
        "\n" +
        "\n}" +
        "\n" +
        "\ntrait E [A ]" +
        "\n{" +
        "\n" +
        "\n  def i (x : A) : A = x" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class E_ [A] () extends E [A]" +
        "\n" +
        "\nobject E {" +
        "\n  def mk [A] : E [A] =" +
        "\n    E_ [A] ()" +
        "\n}" +
        "\n"
    )
  )

  test ("should translate a class in a class") (
    check (
      obtained = instance .translate ("class D" +
        "\n" +
        "\n  f (x : Int) : Int = x + 1" +
        "\n" +
        "\n  class E" +
        "\n    extends" +
        "\n      D" +
        "\n" +
        "\n    g (x : Int) : Int = 2 * x" +
        "\n" +
        "\n  end" +
        "\n" +
        "\n  class F = E" +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected =
        "trait D" +
        "\n{" +
        "\n" +
        "\n  def f (x : Int) : Int = x + 1" +
        "\n" +
        "\n  trait E" +
        "\n    extends" +
        "\n      D" +
        "\n  {" +
        "\n" +
        "\n    def g (x : Int) : Int = 2 * x" +
        "\n" +
        "\n  }" +
        "\n" +
        "\n  case class E_ () extends E" +
        "\n" +
        "\n  object E {" +
        "\n    def mk : E =" +
        "\n      E_ ()" +
        "\n  }" +
        "\n" +
        "\n  type F = E" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class D_ () extends D" +
        "\n" +
        "\nobject D {" +
        "\n  def mk : D =" +
        "\n    D_ ()" +
        "\n}" +
        "\n"
    )
  )

  test ("should translate type aliases") (
    check (
      obtained = instance .translate ("class A [T : Type] = B [T]" +
        "\n" +
        "\nclass C = D" +
        "\n" +
        "\nclass M = Map [Int] [Seq [Int] ]" +
        "\n"
      )
    ) (
      expected =
        "type A [T ] = B [T]" +
        "\n" +
        "\ntype C = D" +
        "\n" +
        "\ntype M = Map [Int, Seq [Int] ]" +
        "\n"
    )
  )

  test ("should translate a tuple assignment") (
    check (
      obtained = instance .translate ("  (x, y) = (f (a), g (a))" +
        "\n" +
        "\n  (p, q) =" +
        "\n    h (1, 2)" +
        "\n"
      )
    ) (
      expected = "  lazy val (x, y) = (f (a), g (a))" +
        "\n" +
        "\n  lazy val (p, q) =" +
        "\n    h (1, 2)" +
        "\n"
    )
  )

  test ("should translate a pattern matching") (
    check (
      obtained = instance .translate ("fibo (n : Int) : Int = " +
        "\n  match n" +
        "\n  case 0 ==> 1 " +
        "\n  case 1 ==> 1 " +
        "\n  case _otherwise ==> if x > 0 then fibo (x - 1) + fibo (x - 2) else 0" +
        "\n"
      )
    ) (
      expected = "def fibo (n : Int) : Int = " +
        "\n  n match  {" +
        "\n  case 0 => 1 " +
        "\n  case 1 => 1 " +
        "\n  case _otherwise => if ( x > 0 ) fibo (x - 1) + fibo (x - 2) else 0" +
        "\n  }" +
        "\n"
    )
  )

  test ("should translate another pattern matching") (
    check (
      obtained = instance .translate ("fibo (n : Int) : Int = " +
        "\n  match n" +
        "\n    case 0 ==> 1 " +
        "\n    case 1 ==> 1 " +
        "\n    case _otherwise ==> if x > 0 then fibo (x - 1) + fibo (x - 2) else 0" +
        "\n"
      )
    ) (
      expected = "def fibo (n : Int) : Int = " +
        "\n  n match  {" +
        "\n    case 0 => 1 " +
        "\n    case 1 => 1 " +
        "\n    case _otherwise => if ( x > 0 ) fibo (x - 1) + fibo (x - 2) else 0" +
        "\n  }" +
        "\n"
    )
  )

  test ("should ignore a pattern matching written in the Scala style") (
    check (
      obtained = instance .translate ("fibo (n : Int) : Int = " +
        "\n  n match" +
        "\n  case 0 ==> 1 " +
        "\n  case 1 ==> 1 " +
        "\n  case _otherwise ==> if x > 0 then fibo (x - 1) + fibo (x - 2) else 0" +
        "\n"
      )
    ) (
      expected = "def fibo (n : Int) : Int = " +
        "\n  n match" +
        "\n  case 0 => 1 " +
        "\n  case 1 => 1 " +
        "\n  case _otherwise => if ( x > 0 ) fibo (x - 1) + fibo (x - 2) else 0" +
        "\n"
    )
  )

  test ("should translate an explicit lambda expression using lambda") (
    check (
      obtained = instance .translate ("plus_1 : Int = lambda (x : Int) --> x + 1" +
        "\n"
      )
    ) (
      expected = "lazy val plus_1 : Int =  (x : Int) => x + 1" +
        "\n"
    )
  )

  test ("should translate another explicit lambda expression using any") (
    check (
      obtained = instance .translate ("plus_1 : Int = any (x : Int) --> x + 1" +
        "\n"
      )
    ) (
      expected = "lazy val plus_1 : Int =  (x : Int) => x + 1" +
        "\n"
    )
  )

  test ("should translate another explicit lambda expression containing parentheses") (
    check (
      obtained = instance .translate ("plus_1 (s : Seq [Int] ) : Seq [Int] =" +
         "\n  s .map (lambda x --> x + 1)" +
        "\n"
      )
    ) (
      expected = "def plus_1 (s : Seq [Int] ) : Seq [Int] =" +
        "\n  s .map ( x => x + 1)" +
        "\n"
    )
  )

  test ("should translate a constructor with parameters 1") (
    check (
      obtained = instance .translate ("p = Constructor_ (a) (b) (c)" +
        "\n"
      )
    ) (
      expected = "lazy val p = Constructor_ (a, b, c)" +
        "\n"
    )
  )

  test ("should translate a constructor with parameters 2") (
    check (
      obtained = instance .translate (
        "p (x : Int) : Constructor = Constructor_ [String] (a) (b) (c)" +
        "\n"
      )
    ) (
      expected = "def p (x : Int) : Constructor = Constructor_ [String] (a, b, c)" +
        "\n"
    )
  )

  test ("should translate a constructor with parameters 3") (
    check (
      obtained = instance .translate ("p = Constructor_ [Int] [String] (a) (b) (c)" +
        "\n"
      )
    ) (
      expected = "lazy val p = Constructor_ [Int, String] (a, b, c)" +
        "\n"
    )
  )

  test ("should translate a constructor with parameters 4") (
    check (
      obtained = instance .translate (
        "p = (Constructor_ [Int] [String] (a) (b) (c) ) .process (d) (e)" +
        "\n"
      )
    ) (
      expected = "lazy val p = (Constructor_ [Int, String] (a, b, c) ) .process (d) (e)" +
        "\n"
    )
  )

  test ("should translate a constructor with parameters 5") (
    check (
      obtained = instance .translate (
        "p = \"(Constructor_ [Int] [String] (a) (b) (c) ) .process (d) (e)\"" +
        "\n"
      )
    ) (
      expected =
        "lazy val p = \"(Constructor_ [Int] [String] (a) (b) (c) ) .process (d) (e)\"" +
        "\n"
    )
  )

  test ("should translate a constructor with parameters 6") (
    check (
      obtained = instance .translate (
        "  case Triplet_ (x) (y) (z) ==> (Triplet_ (x) (y) (z) )" +
        " .get (x) (y) (z) + \" (x) (y) (z)\"" +
        "\n"
      )
    ) (
      expected = "  case Triplet_ (x, y, z) ==> (Triplet_ (x, y, z) )" +
        " .get (x) (y) (z) + \" (x) (y) (z)\"" +
        "\n"
    )
  )

  test ("Translation of comments") (
    check (
      obtained = instance .translate (
        "\n/*" +
        "\n * This is commented text" +
        "\n * " +
        "\n   " +
        "\n   f (x) = 0" +
        "\n   " +
        "\n */" +
        "\n"
      )
    ) (
      expected = "" +
        "/*" +
        "\n * This is commented text" +
        "\n * " +
        "\n   f (x) = 0" +
        "\n */" +
        "\n"
    )
  )

  test ("Translation of documentation") (
    check (
      obtained = instance .translate (
        "\n/**" +
        "\n * This is nicely " +
        "\n * written documentation  " +
        "\n   " +
        "\n   f (x) = 0" +
        "\n   " +
        "\n */" +
        "\n"
      )
    ) (
      expected = "" +
        "/**" +
        "\n * This is nicely " +
        "\n * written documentation  " +
        "\n   f (x) = 0" +
        "\n */" +
        "\n"
    )
  )

  test ("Translation of class alias") (
    check (
      obtained = instance .translate (
        "class Money = Int" +
        "\n"
      )
    ) (
      expected = "" +
        "type Money = Int" +
        "\n"
    )
  )

  test ("Translation of lambda synonyms") (
    check (
      obtained = instance .translate (
        "f : Int -> Int -> Int -> Int =" +
        "\n  lambda x --> any y --> fun z --> (x + y + z)" +
        "\n"
      )
    ) (
      expected = "" +
        "lazy val f : Int => Int => Int => Int =" +
         "\n   x =>  y =>  z => (x + y + z)" +
         "\n"
    )
  )

}


case class MultiLineSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.AnnotatedBlock
  import   soda.translator.block.BlockAnnotationEnum_
  import   soda.translator.block.DefaultBlockTranslator_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.annotation.AnnotationFactory_
  import   soda.translator.parser.BlockBuilder_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val bp =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  lazy val mt = MicroTranslatorToScala_ ()

  lazy val original_input =
    "  value = 1\n" +
    "  sequence = Seq(1 ,\n" +
    "    2,  \n" +
    "    3)\n" +
    "  f( x: Int,\t\n" +
    "     y: Int,\n" +
    "     z: Int) =\n" +
    "       x * x + y * y + z * z\n"

  lazy val original_input_lines = Seq (
    "  value = 1" ,
    "  sequence = Seq(1 ," ,
    "    2,  " ,
    "    3)" ,
    "  f( x: Int,\t" ,
    "     y: Int," ,
    "     z: Int) =" ,
    "       x * x + y * y + z * z")

  lazy val joined_comma_lines = Seq (
    "  value = 1" ,
    "  sequence = Seq(1 ,    2,      3)" ,
    "  f( x: Int,\t     y: Int,     z: Int) =" ,
    "       x * x + y * y + z * z")

  lazy val joined_output =
    "  value = 1\n" +
    "  sequence = Seq(1 ," +
    "    2,  " +
    "    3)\n" +
    "  f( x: Int,\t" +
    "     y: Int," +
    "     z: Int) =\n" +
    "       x * x + y * y + z * z"

  def build_block (lines : Seq [String] ) : AnnotatedBlock =
    AnnotationFactory_ () .annotate (BlockBuilder_ () .build (lines) )

  test ("should split a program in multiple lines") (
    check (
      obtained = bp .make_block (original_input)
    ) (
      expected = build_block (original_input_lines )
    )
  )

  test ("should join the translated lines of a program") (
    check (
      obtained = build_block (joined_comma_lines)
    ) (
      expected = bp .make_block (joined_output)
    )
  )

}


case class ScalaClassConstructorBlockTranslatorSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.BlockTranslatorPipeline_
  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_program_0 =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example0" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n" +
    "\n  abstract" +
    "\n    /** this value is an example */" +
    "\n    map : Map [Int] [Int]" +
    "\n    /** this function is also an example */" +
    "\n    a_function : Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\nclass Example0Spec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n" +
    "\n"

  lazy val expected_program_0 =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example0" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n" +
    "\n  abstract" +
    "\n    /** this value is an example */" +
    "\n    map : Map [Int] [Int]" +
    "\n    /** this function is also an example */" +
    "\n    a_function : Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\ncase class Example0_ (map : Map [Int, Int], a_function : Int => Double => String)" +
    " extends Example0" +
    "\n" +
    "\nobject Example0 {" +
    "\n  def mk (map : Map [Int, Int]) (a_function : Int => Double => " +
    "String) : Example0 =" +
    "\n    Example0_ (map, a_function)" +
    "\n}" +
    "\n" +
    "\nclass Example0Spec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n"

  lazy val example_program_1 =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example [A : Type] [B subtype SuperType]" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n    AnotherSuperClassExample [A]" +
    "\n" +
    "\n  abstract" +
    "\n    value : Int" +
    "\n    a_function : Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\nclass ExampleSpec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n" +
    "\n"

  lazy val expected_program_1 =
    "package soda.example.mytest" +
    "\n" +
    "\nclass Example [A : Type] [B subtype SuperType]" +
    "\n  extends" +
    "\n    SuperClassExample" +
    "\n    AnotherSuperClassExample [A]" +
    "\n" +
    "\n  abstract" +
    "\n    value : Int" +
    "\n    a_function : Int -> Double -> String" +
    "\n" +
    "\nend" +
    "\n" +
    "\ncase class Example_ [A, B <: SuperType] (value : Int," +
    " a_function : Int => Double => String) extends Example [A, B]" +
    "\n" +
    "\nobject Example {" +
    "\n  def mk [A, B <: SuperType] (value : Int) (a_function : Int => " +
    "Double => String) : Example [A, B] =" +
    "\n    Example_ [A, B] (value, a_function)" +
    "\n}" +
    "\n" +
    "\nclass ExampleSpec ()" +
    "\n  extends" +
    "\n    org.scalatest.funsuite.AnyFunSuite" +
    "\n" +
    "\nend" +
    "\n"

  private lazy val _translation_pipeline =
    BlockTranslatorPipeline_ (
      Seq (
        ScalaClassConstructorBlockTranslator_ ()
      )
    )

  lazy val translator =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        _translation_pipeline
      )
    )

  test ("should produce the constructors for a simple class") (
    check (
      obtained = translator .translate (example_program_0)
    ) (
      expected = expected_program_0
    )
  )

  test ("should produce the constructors for a class with type parameters") (
    check (
      obtained = translator .translate (example_program_1)
    ) (
      expected = expected_program_1
    )
  )

}


/**
 * This tests how translation is done for Scala reserved words that are not Soda reserved words.
 */

case class ScalaNonSodaSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val bp =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  test ("Scala reserved words are replaced") (
    check (
      obtained = bp .translate ("" +
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
      obtained = bp .translate ("" +
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
        "\nobject A0 {" +
        "\n  def mk [B0 <: C0] : A0 [B0] =" +
        "\n    A0_ [B0] ()" +
        "\n}" +
        "\n" +
        "\ntrait C0 [D0 >: E0]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class C0_ [D0 >: E0] () extends C0 [D0]" +
        "\n" +
        "\nobject C0 {" +
        "\n  def mk [D0 >: E0] : C0 [D0] =" +
        "\n    C0_ [D0] ()" +
        "\n}" +
        "\n" +
        "\ntrait A1 [B1 <: C1]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class A1_ [B1 <: C1] () extends A1 [B1]" +
        "\n" +
        "\nobject A1 {" +
        "\n  def mk [B1 <: C1] : A1 [B1] =" +
        "\n    A1_ [B1] ()" +
        "\n}" +
        "\n" +
        "\ntrait C1 [D1 >: E1]" +
        "\n{" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class C1_ [D1 >: E1] () extends C1 [D1]" +
        "\n" +
        "\nobject C1 {" +
        "\n  def mk [D1 >: E1] : C1 [D1] =" +
        "\n    C1_ [D1] ()" +
        "\n}" +
        "\n"
    )
  )

}


case class MainSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  lazy val instance = IndividualProcessor_ ()

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  private def _mk_FileNamePair (input_file_name : String) (output_file_name : String) : FileNamePair =
    FileNamePair_ (input_file_name, output_file_name)

  test ("should get the input and output file names without path") (
    check (
      obtained = instance .get_input_output_file_names("my_file.soda")
    ) (
      expected = _mk_FileNamePair ("my_file.soda") ("my_file.scala")
    )
  )

  test ("should get the input and output file names with path") (
    check (
      obtained = instance .get_input_output_file_names("/path/to/file.soda")
    ) (
      expected = _mk_FileNamePair ("/path/to/file.soda") ("/path/to/file.scala")
    )
  )

}


case class UpperAndLowerBoundDeclarationSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance =
    BlockProcessor_(
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  test ("should translate a single upper bound") (
    check (
      obtained = instance .translate ("class BlackBox()" +
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
      obtained = instance .translate ("  class BlackBox()" +
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
      obtained = instance .translate (" class BlackBox()" +
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
      obtained = instance .translate ("  class BlackBox()" +
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

  test ("should translate mixed upper and lower bounds") (
    check (
      obtained = instance .translate (
        "  class BlackBox [A subtype AbstractModel] [B subtype AbstractParameter]" +
        "\n  extends " +
        "\n    AbstractBlackBox[A][B]" +
        "\n    AbstractDevice [A]   [B] " +
        "\n" +
        "\nend" +
        "\n"
      )
    ) (
      expected = "  trait BlackBox [A <: AbstractModel, B <: AbstractParameter]" +
        "\n  extends" +
        "\n    AbstractBlackBox[A, B]" +
        "\n    with AbstractDevice [A, B]" +
        "\n  {" +
        "\n" +
        "\n}" +
        "\n" +
        "\n  case class BlackBox_ [A <: AbstractModel, B <: AbstractParameter] ()" +
        " extends BlackBox [A, B]" +
        "\n" +
        "\n  object BlackBox {" +
        "\n    def mk [A <: AbstractModel, B <: AbstractParameter] : " +
        "BlackBox [A, B] =" +
        "\n      BlackBox_ [A, B] ()" +
        "\n  }" +
        "\n"
    )
  )

}

