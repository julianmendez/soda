package soda.lib

/*
 * This package contains tests for the Soda library.
 */



case class OptionSDSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   scala.util.Try

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val result_if_empty : String = "It is empty."

  def result_if_non_empty (value : String) : String = "Its value is " + value + "."

  def int_to_string (n : Int) : String = "" + n

  def maybe_int_to_string (n : Int) : SomeSD_ [String] = SomeSD_ [String] ("" + n)

  def maybe_string_to_int (s : String) : OptionSD [Int] =
    OptionSDBuilder_ ().build (
      Try ( Integer.parseInt (s.trim) )
        .toOption
    )

  test ("should test an empty option") (
    check (
      obtained = empty_opt.isEmpty && ! empty_opt.isDefined && ! empty_opt.isNonEmpty
    ) (
      expected = true
    )
  )

  lazy val empty_opt = NoneSD_ ()

  test ("should test a non empty option") (
    check (
      obtained = ! non_empty_opt.isEmpty && non_empty_opt.isDefined && non_empty_opt.isNonEmpty
    ) (
      expected = true
    )
  )

  lazy val non_empty_opt = SomeSD_ (1)

  test ("should get a default value, when empty") (
    check (
      obtained = (NoneSD_ [Int] () ).getOrElse (1)
    ) (
      expected = 1
    )
  )

  test ("should get a value") (
    check (
      obtained = (SomeSD_ [Int] (2) ).getOrElse (1)
    ) (
      expected = 2
    )
  )

  test ("should open an empty option") (
    check (
      obtained = (NoneSD_ [String] () ).opt (ifEmpty = result_if_empty) (ifNonEmpty = result_if_non_empty)
    ) (
      expected = "It is empty."
    )
  )

  test ("should open an non empty option") (
    check (
      obtained = (SomeSD_ [String] ("0") ).opt (ifEmpty = result_if_empty) (ifNonEmpty = result_if_non_empty)
    ) (
      expected = "Its value is 0."
    )
  )

  test ("should try fold an empty option") (
    check (
      obtained = (NoneSD_ [String] () ).fold (ifEmpty = result_if_empty) (f = result_if_non_empty)
    ) (
      expected = "It is empty."
    )
  )

  test ("should try fold an non empty option") (
    check (
      obtained = (SomeSD_ [String] ("0") ).fold (ifEmpty = result_if_empty) (f = result_if_non_empty)
    ) (
      expected = "Its value is 0."
    )
  )

  test ("should map empty to empty") (
    check (
      obtained = (NoneSD_ [Int] () ).map (int_to_string)
    ) (
      expected = NoneSD_ [String] ()
    )
  )

  test ("should map a non-empty to another non-empty") (
    check (
      obtained = (SomeSD_ [Int] (2) ).map (int_to_string)
    ) (
      expected = SomeSD_ [String] ("2")
    )
  )

  test ("should flat map empty to empty") (
    check (
      obtained = (NoneSD_ [Int] () ).flatMap (maybe_int_to_string)
    ) (
      expected = NoneSD_ [String] ()
    )
  )

  test ("should flat map a non-empty to another non-empty") (
    check (
      obtained = (SomeSD_ [Int] (2) ).flatMap (maybe_int_to_string)
    ) (
      expected = SomeSD_ [String] ("2")
    )
  )

  test ("should try how successive applications of open works") (
    check (
      obtained =
        maybe_string_to_int ("1").opt (ifEmpty = _empty_opt ) (
          ifNonEmpty =  a =>
            maybe_string_to_int ("2").opt (ifEmpty = _empty_opt ) (
              ifNonEmpty =  b =>
                maybe_string_to_int ("3").opt (ifEmpty = _empty_opt ) (
                  ifNonEmpty =  c =>
                    SomeSD_ (a + b + c) ) ) )
    ) (
      expected = SomeSD_ (6)
    )
  )

  private lazy val _empty_opt : OptionSD [Int] = NoneSD_ [Int] ()

  test ("toOption with non empty option") (
    check (
      obtained = (SomeSD_ [Int] (1) ).toOption
    ) (
      expected = Some (1)
    )
  )

  test ("toOption with another non empty option") (
    check (
      obtained = (SomeSD_ [Int] (2) ).toOption
    ) (
      expected = Some (2)
    )
  )

  test ("toOption with empty option") (
    check (
      obtained = (NoneSD_ [Int] () ).toOption
    ) (
      expected = None
    )
  )

  test ("toSeq with non empty option") (
    check (
      obtained = (SomeSD_ (1) ).toSeq
    ) (
      expected = Seq (1)
    )
  )

  test ("toSeq with another non empty option") (
    check (
      obtained = (SomeSD_ (2) ).toSeq
    ) (
      expected = Seq (2)
    )
  )

  test ("toSeq with empty option") (
    check (
      obtained = (NoneSD_ () ).toSeq
    ) (
      expected = Seq ()
    )
  )

  test ("filter should work for None") (
    check (
      obtained = (NoneSD_ [Int] () ).filter (  x => true)
    ) (
      expected = NoneSD_ [Int] ()
    )
  )

  test ("filter should work for Some, if predicate does not hold") (
    check (
      obtained = (SomeSD_ [Int] (0) ).filter (  x => x > 0)
    ) (
      expected = NoneSD_ [Int] ()
    )
  )

  test ("filter should work for Some, if predicate holds") (
    check (
      obtained = (SomeSD_ [Int] (1) ).filter (  x => x > 0)
    ) (
      expected = SomeSD_ [Int] (1)
    )
  )

}
