package soda.translator.replacement

case class ReplacementAuxSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val instance = ReplacementAux_ ()

  lazy val line_0 = "lambda, if, then, else, match, case"

  lazy val line_1 = "class has extends with this subtype supertype "

  lazy val line_2 = " false ,true ,not ,and ,or ,package ,import ,theorem ,proof ,is ,lambda"

  lazy val line_3 = "  @new, @tailrec, @override "

  lazy val one_word = "lambda"

  test ("replace_if_found_at_beginning 1") (
    check (
      obtained = instance.replace_if_found_at_beginning (line_1) ("class") ("trait")
    ) (
      expected = "trait has extends with this subtype supertype "
    )
  )

  test ("replace_if_found_at_beginning 2") (
    check (
      obtained = instance.replace_if_found_at_beginning (line_1) ("has") ("---")
    ) (
      expected = line_1
    )
  )

  test ("replace_first 1") (
    check (
      obtained = instance.replace_first (line_1) ("class") ("trait")
    ) (
      expected = "trait has extends with this subtype supertype "
    )
  )

  test ("replace_first 2") (
    check (
      obtained = instance.replace_first (line_1) ("this") ("that")
    ) (
      expected = "class has extends with that subtype supertype "
    )
  )

  test ("replace_at 1") (
    check (
      obtained = instance.replace_at (22) (line_1) (" ") ("out ")
    ) (
      expected = "class has extends without this subtype supertype "
    )
  )

  test ("replace_at 2") (
    check (
      obtained = instance.replace_at (-1) (line_1) (" ") ("no replacement here")
    ) (
      expected = line_1
    )
  )

  test ("replace_at 3") (
    check (
      obtained = instance.replace_at (line_1.length - 1) (line_1) (" ") ("")
    ) (
      expected = "class has extends with this subtype supertype"
    )
  )

  test ("replace_at 4") (
    check (
      obtained = instance.replace_at (line_1.length) (line_1) (" ") ("no replacement")
    ) (
      expected = line_1
    )
  )

  test ("replace_if_found") (
    check (
      obtained = instance.replace_if_found (line_1) ("type") ("class")
    ) (
      expected = "class has extends with this subclass superclass "
    )
  )

  test ("replace_all 1") (
    check (
      obtained = instance.replace_all (line_1) (" ") (",")
    ) (
      expected = "class,has,extends,with,this,subtype,supertype,"
    )
  )

  test ("replace_all 2") (
    check (
      obtained = instance.replace_all (line_1) ("z") ("-")
    ) (
      expected = line_1
    )
  )

  test ("add_spaces_to_symbols 1") (
    check (
      obtained = instance.add_spaces_to_symbols (line_0) ( (Seq [Char] (',') ).toSet )
    ) (
      expected = "lambda , if , then , else , match , case"
    )
  )

  test ("add_spaces_to_symbols 2") (
    check (
      obtained = instance.add_spaces_to_symbols (line_2) ( (Seq [Char] (',') ).toSet )
    ) (
      expected = " false , true , not , and , or , package , import , theorem , proof , is , lambda"
    )
  )

  test ("add_spaces_to_symbols 3") (
    check (
      obtained = instance.add_spaces_to_symbols (line_1) ( (Seq [Char] ('a', 'e', 'i', 'o', 'u') ).toSet )
    ) (
      expected = "cl a ss h a s e xt e nds w i th th i s s u btyp e s u p e rtyp e "
    )
  )

  test ("remove_space_from_scala_line") (
    check (
      obtained = instance.remove_space_from_scala_line (line_3)
    ) (
      expected = " @new, @tailrec, @override"
    )
  )

  test ("add_after_spaces_or_pattern 1") (
    check (
      obtained = instance.add_after_spaces_or_pattern (line_1) ("class") (" here")
    ) (
      expected = "class here has extends with this subtype supertype "
    )
  )

  test ("add_after_spaces_or_pattern 2") (
    check (
      obtained = instance.add_after_spaces_or_pattern (line_1) ("has") (" here ")
    ) (
      expected = " here class has extends with this subtype supertype "
    )
  )

  test ("add_after_spaces_or_pattern 3") (
    check (
      obtained = instance.add_after_spaces_or_pattern (line_3) ("@new") (", here")
    ) (
      expected = "  @new, here, @tailrec, @override "
    )
  )

}
