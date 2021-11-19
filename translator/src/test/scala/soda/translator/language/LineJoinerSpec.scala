package soda.translator.language


case class LineJoinerSpec ()  extends org.scalatest.funsuite.AnyFunSuite {

  lazy val mt = MicroTranslatorToScala_ ()

  lazy val Original_parenthesis_example =
    Seq ("a[ ", "T", " ]( ", "x: Int,", " y: Int", " ) = f[T](", "x + y", ")"    )

  lazy val Result_with_only_opening_brackets_example =
    Seq ("a[ T", " ]( x: Int, y: Int", " ) = f[T](x + y", ")"    )

  lazy val Result_with_only_closing_brackets_example =
    Seq ("a[ ", "T ]( ", "x: Int,", " y: Int ) = f[T](", "x + y)"    )

  lazy val Result_with_opening_closing_brackets_example =
    Seq ("a[ T ]( x: Int, y: Int ) = f[T](x + y)"    )

  lazy val Example_using_extends_and_with =
    Seq ("it should join lines ending with the extends", "keyword", "it should join lines ending with the with", "keyword", "it should join lines starting with the", "extends keyword", "it should join lines starting with the", "with keyword", "it should not join lines ending with suffixextends", "it should not join lines ending with suffixwith", "extendsprefix should not join lines", "withprefix should not join lines", "trailing spaces using extends  ", "and using with ", "should be respected", "leading spaces using", " extends and using ", "  with should be respected"    )

  test ("should join lines ending in comma") {
    lazy val input = Seq ("this ", "should join,", "all the lines , ", "  that end with a", "comma.")
    lazy val expected = Seq ("this ", "should join,all the lines ,   that end with a", "comma.")
    lazy val obtained = mt.join_lines_with_forward_join (input )

    assert (obtained == expected )
  }

  test ("should join lines ending in opening parenthesis and brackets") {
    lazy val input = Original_parenthesis_example
    lazy val expected = Result_with_only_opening_brackets_example
    lazy val obtained = mt.join_lines_with_forward_join (input )

    assert (obtained == expected )
  }

  test ("should join lines starting in closing parenthesis and brackets") {
    lazy val input = Original_parenthesis_example
    lazy val expected = Result_with_only_closing_brackets_example
    lazy val obtained = mt.join_lines_with_backward_join (input )

    assert (obtained == expected )
  }

  test ("should join lines ending with opening parenthesis and brackets or starting in closing parenthesis and brackets") {
    lazy val input = Original_parenthesis_example
    lazy val expected = Result_with_opening_closing_brackets_example
    lazy val obtained =
      Some (input )
        .map (x => mt.join_lines_with_forward_join (x )  )
        .map (x => mt.join_lines_with_backward_join (x )  )
        .get

    assert (obtained == expected )
  }

  test ("should join lines starting or ending with keyword 'extends' and 'with' with forward join") {
    lazy val input = Example_using_extends_and_with
    lazy val expected =
      Seq ("it should join lines ending with the extendskeyword", "it should join lines ending with the withkeyword", "it should join lines starting with the", "extends keyword", "it should join lines starting with the", "with keyword", "it should not join lines ending with suffixextends", "it should not join lines ending with suffixwith", "extendsprefix should not join lines", "withprefix should not join lines", "trailing spaces using extends  and using with should be respected", "leading spaces using", " extends and using ", "  with should be respected"      )
    lazy val obtained = mt.join_lines_with_forward_join (input )

    assert (obtained == expected )
  }

  test ("should join lines starting or ending with keyword 'extends' and 'with' with backward join") {
    lazy val input = Example_using_extends_and_with
    lazy val expected =
      Seq ("it should join lines ending with the extends", "keyword", "it should join lines ending with the with", "keyword", "it should join lines starting with theextends keyword", "it should join lines starting with thewith keyword", "it should not join lines ending with suffixextends", "it should not join lines ending with suffixwith", "extendsprefix should not join lines", "withprefix should not join lines", "trailing spaces using extends  ", "and using with ", "should be respected", "leading spaces using extends and using   with should be respected"      )
    lazy val obtained = mt.join_lines_with_backward_join (input )

    assert (obtained == expected )
  }
}
