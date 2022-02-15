package soda.translator.extension.toscala

case class MicroTranslatorToScalaSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   soda.translator.block.DefaultBlockSequenceTranslator_
  import   soda.translator.parser.BlockProcessor_

  lazy val instance =
    BlockProcessor_ (
      DefaultBlockSequenceTranslator_ (
        MicroTranslatorToScala_ ()
      )
    )

  test ("should translate a small snippet")
    {
      lazy val original = "  input_lines = Seq(" +
        "\n    \"  f( x: Int,\\t\", " +
        "\n    \"     y: Int) =\"," +
        "\n    \"       x + y\")" +
        "\n"
      lazy val expected = "  lazy val input_lines = Seq (" +
        "\n    \"  f( x: Int,\\t\"," +
        "\n    \"     y: Int) =\"," +
        "\n    \"       x + y\")" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should leave content of apostrophes unchanged")
    {
      lazy val input = " a = Seq('\\'', \'', '\\\"', ' or ', \'or\', '0x00', '->', '/*', '*/')\n"
      lazy val expected = " lazy val a = Seq ('\\'', '', '\\\"', ' or ', 'or', '0x00', '->', '/*', '*/')\n"
      lazy val obtained = instance.translate (input )
      assert (obtained == expected ) }

  test ("should leave content of quotation marks unchanged")
    {
      lazy val input = " a = Seq(\"\\\"\", \"\", \"\\\'\", \" or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n"
      lazy val expected = " lazy val a = Seq (\"\\\"\", \"\", \"\\'\", \" or \", \"or\", \"0x00\", \"->\", \"/*\", \"*/\")\n"
      lazy val obtained = instance.translate (input )
      assert (obtained == expected ) }

  test ("should translate classes")
    {
      lazy val original =
        "class D" +
        "\n" +
        "\n  f(x: Int): Int = x + 1" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass E" +
        "\n" +
        "\n  g(x: Int): Int = 2 * x" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass F ()" +
        "\n  extends" +
        "\n    D" +
        "\n" +
        "\n  abstract" +
        "\n    /** name for this object */" +
        "\n    name: String" +
        "\n    /**" +
        "\n     * value for this object" +
        "\n     */" +
        "\n    value: Int" +
        "\n" +
        "\n  h(x: Int): Int = 2 * x + 1" +
        "\n" +
        "\nend" +
        "\n" +
        "\nclass E [A]" +
        "\n" +
        "\n  i(x: A): A = x" +
        "\n" +
        "\nend" +
        "\n"
      lazy val expected =
        "trait D" +
        "\n{" +
        "\n" +
        "\n  def f (x: Int ): Int = x + 1" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class D_ () extends D" +
        "\n" +
        "\ntrait E" +
        "\n{" +
        "\n" +
        "\n  def g (x: Int ): Int = 2 * x" +
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
        "\n  def   name: String" +
        "\n    /**" +
        "\n     * value for this object" +
        "\n     */" +
        "\n  def   value: Int" +
        "\n" +
        "\n  def h (x: Int ): Int = 2 * x + 1" +
        "\n" +
        "\n}" +
        "\n" +
        "\ntrait E [A]" +
        "\n{" +
        "\n" +
        "\n  def i (x: A ): A = x" +
        "\n" +
        "\n}" +
        "\n" +
        "\ncase class E_ [A] () extends E [A]" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate type aliases")
    {
      lazy val original =
        "* A[T] = B[T]" +
        "\n" +
        "\nclass C = D" +
        "\n" +
        "\n* M = Map[Int, Seq[Int]]" +
        "\n"
      lazy val expected =
        "type A [T] = B [T]" +
        "\n" +
        "\ntype C = D" +
        "\n" +
        "\ntype M = Map [Int, Seq [Int]]" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate a tuple assignment")
    {
      lazy val original = "  (x, y) = (f(a), g(a))" +
        "\n  (p, q) =" +
        "\n    h(1, 2)" +
        "\n"
      lazy val expected = "  lazy val (x, y ) = (f (a ), g (a )  )" +
        "\n  lazy val (p, q ) =" +
        "\n    h (1, 2 )" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate a pattern matching")
    {
      lazy val original = "fibo(n: Int): Int = " +
        "\n  match n" +
        "\n  | 0 => 1 " +
        "\n  | 1 => 1 " +
        "\n  | m => if m > 0 then fibo(m - 1) + fibo(m - 2) else 0" +
        "\n end" +
        "\n"
      lazy val expected = "def fibo (n: Int ): Int =" +
        "\n  n match  {" +
        "\n  case 0 => 1 " +
        "\n  case 1 => 1 " +
        "\n  case m => if (m > 0 ) fibo (m - 1 ) + fibo (m - 2 ) else 0" +
        "\n }" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate another pattern matching")
    {
      lazy val original = "fibo(n: Int): Int = " +
        "\n  match n" +
        "\n    | 0 => 1 " +
        "\n    | 1 => 1 " +
        "\n    | m => if m > 0 then fibo(m - 1) + fibo(m - 2) else 0" +
        "\n  end" +
        "\n"
      lazy val expected = "def fibo (n: Int ): Int =" +
        "\n  n match  {" +
        "\n    case 0 => 1 " +
        "\n    case 1 => 1 " +
        "\n    case m => if (m > 0 ) fibo (m - 1 ) + fibo (m - 2 ) else 0" +
        "\n  }" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should ignore a pattern matching written in the Scala style")
    {
      lazy val original = "fibo(n: Int): Int = " +
        "\n  n match" +
        "\n  | 0 => 1 " +
        "\n  | 1 => 1 " +
        "\n  | m => if m > 0 then fibo(m - 1) + fibo(m - 2) else 0" +
        "\n"
      lazy val expected = "def fibo (n: Int ): Int =" +
        "\n  n match" +
        "\n  case 0 => 1 " +
        "\n  case 1 => 1 " +
        "\n  case m => if (m > 0 ) fibo (m - 1 ) + fibo (m - 2 ) else 0" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate an explicit lambda expression")
    {
      lazy val original = "plus_1: Int = (lambda x: Int) --> x + 1" +
        "\n"
      lazy val expected = "lazy val plus_1: Int = (x: Int ) => x + 1" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate let in 1")
    {
      lazy val original = "f(x: Int): Int =" +
        "\n  let y = x + x" +
        "\n  in y"
      lazy val expected = "def f (x: Int ): Int =" +
        "\n  { lazy val y = x + x" +
        "\n    y }" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate let in 2")
    {
      lazy val original = "f(x: Int): Int =" +
        "\n  let" +
        "\n    y = x + x" +
        "\n  in y"
      lazy val expected = "def f (x: Int ): Int =" +
        "\n  {" +
        "\n    lazy val y = x + x" +
        "\n    y }" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate let in 3")
    {
      lazy val original = "f(x: Int): Int =" +
        "\n  let" +
        "\n    y = x + x" +
        "\n  in let z = y + y" +
        "\n  in z"
      lazy val expected = "def f (x: Int ): Int =" +
        "\n  {" +
        "\n    lazy val y = x + x" +
        "\n   lazy val z = y + y" +
        "\n    z }" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

  test ("should translate let in 4")
    {
      lazy val original = "f(x: Int): Int =" +
        "\n  let" +
        "\n    y = x + x" +
        "\n  in let" +
        "\n    z = y + y" +
        "\n  in z"
      lazy val expected = "def f (x: Int ): Int =" +
        "\n  {" +
        "\n    lazy val y = x + x" +
        "\n   " +
        "\n    lazy val z = y + y" +
        "\n    z }" +
        "\n"
      lazy val obtained = instance.translate (original )
      assert (obtained == expected ) }

}
