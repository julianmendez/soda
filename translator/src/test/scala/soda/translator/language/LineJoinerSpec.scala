package soda.translator.language


case class LineJoinerSpec () extends org.scalatest.funsuite.AnyFunSuite {

  lazy val mt = MicroTranslator ()

  lazy val Original_parenthesis_example =
    Seq ("a[ ", "T", " ]( ", "x: Int,", " y: Int", " ) = f[T](", "x + y", ")"    )

  lazy val Result_with_only_opening_brackets_example =
    Seq ("a[ T", " ]( x: Int, y: Int", " ) = f[T](x + y", ")"    )

  lazy val Result_with_only_closing_brackets_example =
    Seq ("a[ ", "T ]( ", "x: Int,", " y: Int ) = f[T](", "x + y)"    )

  lazy val Result_with_opening_closing_brackets_example =
    Seq ("a[ T ]( x: Int, y: Int ) = f[T](x + y)"    )

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

  test ("should join lines starting or ending with keyword 'extends'") {
    lazy val input = Seq ("this should join the lines ending with the extends", "keyword", "this should join the lines starting with the", " extends keyword", "but not if theextends", "keyword is a suffix", "or if the", "extendskeyword is a prefix"    )
    lazy val expected = Seq ("this should join the lines ending with the extends keyword", "this should join the lines starting with the extends keyword", "but not if theextends", "keyword is a suffix", "or if the", "extendskeyword is a prefix"    )
    lazy val obtained = mt.join_lines_with_forward_join (input )

    assert (obtained == expected )
  }

  test ("should join lines starting or ending with keyword 'with'") {
    lazy val input = Seq ("this should join the lines ending with the with", "keyword", "this should join the lines starting with the", "with keyword", "but not if thewith", "keyword is a suffix", "or if the", "withkeyword is a prefix."    )
    lazy val expected = Seq ("this should join the lines ending with the with keyword", "this should join the lines starting with the with keyword", "but not if thewith", "keyword is a suffix", "or if the", "withkeyword is a prefix."    )
    lazy val obtained = mt.join_lines_with_forward_join (input )

    assert (obtained == expected )
  }
}
