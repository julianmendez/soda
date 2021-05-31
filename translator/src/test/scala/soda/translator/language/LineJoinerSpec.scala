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
    lazy val input = Seq ("this ", "should join,", "all the lines , ", "  that end with", "comma.")
    lazy val expected = Seq ("this ", "should join,all the lines ,   that end with", "comma.")
    lazy val obtained = mt.join_lines_with_opening_brackets (input )

    assert (obtained == expected )
  }

  test ("should join lines ending in opening parenthesis and brackets") {
    lazy val input = Original_parenthesis_example
    lazy val expected = Result_with_only_opening_brackets_example
    lazy val obtained = mt.join_lines_with_opening_brackets (input )

    assert (obtained == expected )
  }

  test ("should join lines starting in closing parenthesis and brackets") {
    lazy val input = Original_parenthesis_example
    lazy val expected = Result_with_only_closing_brackets_example
    lazy val obtained = mt.join_lines_with_closing_brackets (input )

    assert (obtained == expected )
  }

  test ("should join lines ending with opening parenthesis and brackets or starting in closing parenthesis and brackets") {
    lazy val input = Original_parenthesis_example
    lazy val expected = Result_with_opening_closing_brackets_example
    lazy val obtained =
      Some (input )
        .map (x => mt.join_lines_with_opening_brackets (x )  )
        .map (x => mt.join_lines_with_closing_brackets (x )  )
        .get

    assert (obtained == expected )
  }
}
