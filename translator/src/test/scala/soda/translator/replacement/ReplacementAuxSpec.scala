package soda.translator.replacement

case class ReplacementAuxSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  lazy val instance = ReplacementAux_ ()

  lazy val line_0 = "if, then, else, let, in, match, case, end"

  lazy val line_1 = "class has extends with this subtype supertype "

  lazy val line_2 = " false ,true ,not ,and ,or ,package ,import ,theorem ,proof ,is ,lambda"

  lazy val line_3 = "  @new, @tailrec, @override "

  lazy val one_word = "lambda"

  test ("replace_if_found_at_beginning 1")
    {
      lazy val expected = "trait has extends with this subtype supertype "
      lazy val obtained = instance.replace_if_found_at_beginning (line_1) ("class") ("trait")
     assert (obtained == expected) }

  test ("replace_if_found_at_beginning 2")
    {
      lazy val expected = line_1
      lazy val obtained = instance.replace_if_found_at_beginning (line_1) ("has") ("---")
     assert (obtained == expected) }

  test ("replace_first 1")
    {
      lazy val expected = "trait has extends with this subtype supertype "
      lazy val obtained = instance.replace_first (line_1) ("class") ("trait")
     assert (obtained == expected) }

  test ("replace_first 2")
    {
      lazy val expected = "class has extends with that subtype supertype "
      lazy val obtained = instance.replace_first (line_1) ("this") ("that")
     assert (obtained == expected) }

  test ("replace_at 1")
    {
      lazy val expected = "class has extends without this subtype supertype "
      lazy val obtained = instance.replace_at (22) (line_1) (" ") ("out ")
     assert (obtained == expected) }

  test ("replace_at 2")
    {
      lazy val expected = line_1
      lazy val obtained = instance.replace_at (-1) (line_1) (" ") ("no replacement here")
     assert (obtained == expected) }

  test ("replace_at 3")
    {
      lazy val expected = "class has extends with this subtype supertype"
      lazy val obtained = instance.replace_at (line_1.length - 1) (line_1) (" ") ("")
     assert (obtained == expected) }

  test ("replace_at 4")
    {
      lazy val expected = line_1
      lazy val obtained = instance.replace_at (line_1.length) (line_1) (" ") ("no replacement")
     assert (obtained == expected) }

  test ("replace_if_found")
    {
      lazy val expected = "class has extends with this subclass superclass "
      lazy val obtained = instance.replace_if_found (line_1) ("type") ("class")
     assert (obtained == expected) }

  test ("replace_all 1")
    {
      lazy val expected = "class,has,extends,with,this,subtype,supertype,"
      lazy val obtained = instance.replace_all (line_1) (" ") (",")
     assert (obtained == expected) }

  test ("replace_all 2")
    {
      lazy val expected = line_1
      lazy val obtained = instance.replace_all (line_1) ("z") ("-")
     assert (obtained == expected) }

  test ("add_spaces_to_symbols 1")
    {
      lazy val expected = "if , then , else , let , in , match , case , end"
      lazy val obtained = instance.add_spaces_to_symbols (line_0) ( (Seq [Char] (',') ).toSet )
     assert (obtained == expected) }

  test ("add_spaces_to_symbols 2")
    {
      lazy val expected = " false , true , not , and , or , package , import , theorem , proof , is , lambda"
      lazy val obtained = instance.add_spaces_to_symbols (line_2) ( (Seq [Char] (',') ).toSet )
     assert (obtained == expected) }

  test ("add_spaces_to_symbols 3")
    {
      lazy val expected = "cl a ss h a s e xt e nds w i th th i s s u btyp e s u p e rtyp e "
      lazy val obtained = instance.add_spaces_to_symbols (line_1) ( (Seq [Char] ('a', 'e', 'i', 'o', 'u') ).toSet )
     assert (obtained == expected) }

  test ("remove_space_from_scala_line")
    {
      lazy val expected = " @new, @tailrec, @override"
      lazy val obtained = instance.remove_space_from_scala_line (line_3)
     assert (obtained == expected) }

  test ("add_after_spaces_or_pattern 1")
    {
      lazy val expected = "class here has extends with this subtype supertype "
      lazy val obtained = instance.add_after_spaces_or_pattern (line_1) ("class") (" here")
     assert (obtained == expected) }

  test ("add_after_spaces_or_pattern 2")
    {
      lazy val expected = " here class has extends with this subtype supertype "
      lazy val obtained = instance.add_after_spaces_or_pattern (line_1) ("has") (" here ")
     assert (obtained == expected) }

  test ("add_after_spaces_or_pattern 3")
    {
      lazy val expected = "  @new, here, @tailrec, @override "
      lazy val obtained = instance.add_after_spaces_or_pattern (line_3) ("@new") (", here")
     assert (obtained == expected) }

}
