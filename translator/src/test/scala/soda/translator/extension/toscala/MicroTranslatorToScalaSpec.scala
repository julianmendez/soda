package soda.translator.extension.toscala

case class MicroTranslatorToScalaSpec ()
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

  test ("should translate a small snippet") (
    check (
      obtained = instance.translate ("  input_lines = Seq (" +
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

  test ("should leave content of apostrophes unchanged") (
    check (
      obtained = instance.translate (" a = Seq ('\\'', \'', '\\\"', ' or ', \'or\', '0x00', '->', '/*', '*/')\n")
    ) (
      expected = " lazy val a = Seq ('\\'', '', '\\\"', ' or ', 'or', '0x00', '->', '/*', '*/')\n"
    )
  )

  test ("should leave content of quotation marks unchanged") (
    check (
      obtained = instance.translate (" a = Seq (\"\\\"\", \"\", \"\\\'\", \" or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n" )
    ) (
      expected = " lazy val a = Seq (\"\\\"\", \"\", \"\\'\", \" or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n"
    )
  )

  test ("should translate classes") (
    check (
      obtained = instance.translate ("class D" +
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
        "\n  h (x : Int) : Int = 2 * x + 1" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass E [A]" +
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
        "\ntrait E" +
        "\n{" +
        "\n" +
        "\n  def g (x : Int) : Int = 2 * x" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class E_ () extends E" +
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
        "\n  def h (x : Int) : Int = 2 * x + 1" +
        "\n" +
        "\n}" +
        "\n" +
        "\ntrait E [A]" +
        "\n{" +
        "\n" +
        "\n  def i (x : A) : A = x" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class E_ [A] () extends E [A]" +
        "\n"
    )
  )

  test ("should translate a class in a class") (
    check (
      obtained = instance.translate ("class D" +
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
        "\n  type F = E" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class D_ () extends D" +
        "\n"
    )
  )

  test ("should translate type aliases") (
    check (
      obtained = instance.translate ("class A [T] = B [T]" +
        "\n" +
        "\nclass C = D" +
        "\n" +
        "\nclass M = Map [Int, Seq [Int]]" +
        "\n"
      )
    ) (
      expected =
        "type A [T] = B [T]" +
        "\n" +
        "\ntype C = D" +
        "\n" +
        "\ntype M = Map [Int, Seq [Int]]" +
        "\n"
    )
  )

  test ("should translate a tuple assignment") (
    check (
      obtained = instance.translate ("  (x, y) = (f (a), g (a))" +
        "\n  (p, q) =" +
        "\n    h (1, 2)" +
        "\n"
      )
    ) (
      expected = "  lazy val (x, y) = (f (a), g (a))" +
        "\n  lazy val (p, q) =" +
        "\n    h (1, 2)" +
        "\n"
    )
  )

  test ("should translate a pattern matching") (
    check (
      obtained = instance.translate ("fibo (n : Int) : Int = " +
        "\n  match n" +
        "\n  case 0 ==> 1 " +
        "\n  case 1 ==> 1 " +
        "\n  case m ==> if m > 0 then fibo (m - 1) + fibo (m - 2) else 0" +
        "\n end" +
        "\n"
      )
    ) (
      expected = "def fibo (n : Int) : Int = " +
        "\n  n match  {" +
        "\n  case 0 => 1 " +
        "\n  case 1 => 1 " +
        "\n  case m => if ( m > 0 ) fibo (m - 1) + fibo (m - 2) else 0" +
        "\n }" +
        "\n"
    )
  )

  test ("should translate another pattern matching") (
    check (
      obtained = instance.translate ("fibo (n : Int) : Int = " +
        "\n  match n" +
        "\n    case 0 ==> 1 " +
        "\n    case 1 ==> 1 " +
        "\n    case m ==> if m > 0 then fibo (m - 1) + fibo (m - 2) else 0" +
        "\n  end" +
        "\n"
      )
    ) (
      expected = "def fibo (n : Int) : Int = " +
        "\n  n match  {" +
        "\n    case 0 => 1 " +
        "\n    case 1 => 1 " +
        "\n    case m => if ( m > 0 ) fibo (m - 1) + fibo (m - 2) else 0" +
        "\n  }" +
        "\n"
    )
  )

  test ("should ignore a pattern matching written in the Scala style") (
    check (
      obtained = instance.translate ("fibo (n : Int) : Int = " +
        "\n  n match" +
        "\n  case 0 ==> 1 " +
        "\n  case 1 ==> 1 " +
        "\n  case m ==> if m > 0 then fibo (m - 1) + fibo (m - 2) else 0" +
        "\n"
      )
    ) (
      expected = "def fibo (n : Int) : Int = " +
        "\n  n match" +
        "\n  case 0 => 1 " +
        "\n  case 1 => 1 " +
        "\n  case m => if ( m > 0 ) fibo (m - 1) + fibo (m - 2) else 0" +
        "\n"
    )
  )

  test ("should translate an explicit lambda expression") (
    check (
      obtained = instance.translate ("plus_1 : Int = lambda (x : Int) --> x + 1" +
        "\n"
      )
    ) (
      expected = "lazy val plus_1 : Int =  (x : Int) => x + 1" +
        "\n"
    )
  )

}
