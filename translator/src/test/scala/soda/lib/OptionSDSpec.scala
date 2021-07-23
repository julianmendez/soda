package soda.lib


case class OptionSDSpec () extends org.scalatest.funsuite.AnyFunSuite {
  import scala.util.Try
  import scala.util.Success
  import scala.util.Failure

  test ("should test an empty option") {
    lazy val empty = NoInst ()

    assert (empty.isEmpty &&
      ! empty.isDefined &&
      ! empty.nonEmpty )
  }

  test ("should test a non empty option") {
    lazy val element = SomeInst (1 )

    assert (! element.isEmpty &&
      element.isDefined &&
      element.nonEmpty )
  }

  test ("should get a default value, when empty") {
    lazy val empty = NoInst [Int]  ()
    lazy val expected = 1
    lazy val obtained = empty.getOrElse (1 )

    assert (obtained == expected )
  }

  test ("should get a value") {
    lazy val empty = SomeInst [Int]  (2 )
    lazy val expected = 2
    lazy val obtained = empty.getOrElse (1 )

    assert (obtained == expected )
  }

  test ("should open an empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: String ): String = "Its value is " + value + "."
    lazy val empty = NoInst [String]  ()
    lazy val expected = "It is empty."
    lazy val obtained = empty.opt (ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should open an non empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: String ): String = "Its value is " + value + "."
    lazy val some_element = SomeInst [String]  ("0")
    lazy val expected = "Its value is 0."
    lazy val obtained = some_element.opt (ifEmpty = result_if_empty, ifNonEmpty = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should try fold an empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: String ): String = "Its value is " + value + "."
    lazy val empty = NoInst [String]  ()
    lazy val expected = "It is empty."
    lazy val obtained = empty.fold (ifEmpty = result_if_empty, f = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should try fold an non empty option") {
    lazy val result_if_empty: String = "It is empty."
    def result_if_non_empty (value: String ): String = "Its value is " + value + "."
    lazy val some_element = SomeInst [String]  ("0")
    lazy val expected = "Its value is 0."
    lazy val obtained = some_element.fold (ifEmpty = result_if_empty, f = result_if_non_empty )

    assert (obtained == expected )
  }

  test ("should map empty to empty") {
    def to_string (n: Int ): String = "" + n
    lazy val empty = NoInst [Int]  ()
    lazy val expected = NoInst [String]  ()
    lazy val obtained = empty.map (to_string )

    assert (obtained == expected )
  }

  test ("should map a non-empty to another non-empty") {
    def to_string (n: Int ): String = "" + n
    lazy val some_element = SomeInst [Int]  (2 )
    lazy val expected = SomeInst [String]  ("2")
    lazy val obtained = some_element.map (to_string )

    assert (obtained == expected )
  }

  test ("should flat map empty to empty") {
    def to_string (n: Int ): SomeInst [String] = SomeInst [String]  ("" + n )
    lazy val empty = NoInst [Int]  ()
    lazy val expected = NoInst [String]  ()
    lazy val obtained = empty.flatMap (to_string )

    assert (obtained == expected )
  }

  test ("should flat map a non-empty to another non-empty") {
    def to_string (n: Int ): SomeInst [String] = SomeInst [String]  ("" + n )
    lazy val some_element = SomeInst [Int]  (2 )
    lazy val expected = SomeInst [String]  ("2")
    lazy val obtained = some_element.flatMap (to_string )

    assert (obtained == expected )
  }


  test ("should try how successive applications of open works") {
    def toInt (s: String ): OptionSD [Int] =
      OptionSDBuilderImpl () .build (Try (Integer.parseInt (s.trim ) )
          .toOption      )

    lazy val stringA = "1"
    lazy val stringB = "2"
    lazy val stringC = "3"

    lazy val maybeA = toInt (stringA )
    lazy val maybeB = toInt (stringB )
    lazy val maybeC = toInt (stringC )

    lazy val expected = SomeInst (6 )
    lazy val obtained =
      maybeA.opt (ifEmpty = NoInst, ifNonEmpty = a =>
          maybeB.opt (ifEmpty = NoInst, ifNonEmpty = b =>
              maybeC.opt (ifEmpty = NoInst, ifNonEmpty = c =>
                  SomeInst (a + b + c ) ) ) )

    assert (obtained == expected )
  }

  test ("toOption with non empty option") {
    lazy val input: OptionSD [Int] = SomeInst (1 )
    lazy val expected: Option [Int] = Some (1 )
    lazy val obtained = input.toOption

    assert (obtained == expected )
  }

  test ("toOption with another non empty option") {
    lazy val input: SomeInst [Int] = SomeInst (2 )
    lazy val expected: Some [Int] = Some (2 )
    lazy val obtained = input.toOption

    assert (obtained == expected )
  }

  test ("toOption with empty option") {
    lazy val input: OptionSD [Int] = NoInst ()
    lazy val expected = None
    lazy val obtained = input.toOption

    assert (obtained == expected )
  }

  test ("toSeq with non empty option") {
    lazy val input: OptionSD [Int] = SomeInst (1 )
    lazy val expected: Seq [Int] = Seq (1 )
    lazy val obtained = input.toSeq

    assert (obtained == expected )
  }

  test ("toSeq with another non empty option") {
    lazy val input: SomeInst [Int] = SomeInst (2 )
    lazy val expected: Seq [Int] = Seq (2 )
    lazy val obtained = input.toSeq

    assert (obtained == expected )
  }

  test ("toSeq with empty option") {
    lazy val input: OptionSD [Int] = NoInst ()
    lazy val expected = Seq ()
    lazy val obtained = input.toSeq

    assert (obtained == expected )
  }

  test ("filter should work for None") {
    lazy val input = NoInst [Int]  ()
    lazy val expected = NoInst [Int]  ()
    lazy val obtained = input.filter (x => true )

    assert (obtained == expected )
  }

  test ("filter should work for Some, if predicate does not hold") {
    lazy val input = SomeInst [Int]  (0 )
    lazy val expected = NoInst [Int]  ()
    lazy val obtained = input.filter (x => x > 0 )

    assert (obtained == expected )
  }

  test ("filter should work for Some, if predicate holds") {
    lazy val input = SomeInst [Int]  (1 )
    lazy val expected = SomeInst [Int]  (1 )
    lazy val obtained = input.filter (x => x > 0 )

    assert (obtained == expected )
  }
}
