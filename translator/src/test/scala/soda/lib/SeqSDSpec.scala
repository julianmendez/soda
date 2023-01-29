package soda.lib

case class SeqSDSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val int_seq : Seq [Int] = Seq (2, 7, 1, 8, 2, 8, 1, 8, 2, 8, 4, 5, 9)

  lazy val rev_int_seq : Seq [Int] = Seq (9, 5 ,4, 8, 2, 8, 1, 8, 2, 8, 1, 7, 2)

  test ("should detect an empty sequence") (
    check (
      obtained = SeqSDBuilder_ ().build (Seq [Int] () ).opt (ifEmpty = true) (ifNonEmpty =  nonEmpty => false)
    ) (
      expected = true
    )
  )

  test ("should detect an non empty sequence") (
    check (
      obtained = SeqSDBuilder_ ().build (Seq [Int] (1) ).opt (ifEmpty = false) (ifNonEmpty =  nonEmpty => true)
    ) (
      expected = true
    )
  )

  test ("should get the maximum") (
    check (
      obtained = SeqSDBuilder_ ().build (int_seq).opt (ifEmpty = empty_opt) (ifNonEmpty = non_empty_opt)
    ) (
      expected = SomeSD_ [Int] (9)
    )
  )

  lazy val empty_opt : OptionSD [Int] = NoneSD_ ()

  lazy val non_empty_opt : NonEmptySeqSD [Int] => SomeSD [Int] =  sequence => SomeSD_ (max (sequence) )

  private lazy val _fold = Fold_ ()

  def max_of_2 (a : Int) (b : Int) : Int =
    if ( a > b ) a else b

  def max (s : NonEmptySeqSD [Int]) : Int =
    _fold.apply (s.tail.toSeq) (s.head) (max_of_2)

  test ("should reverse a sequence") (
    check (
      obtained = SeqSDBuilder_ ().build (int_seq).reverse
    ) (
      expected = SeqSDBuilder_ ().build (rev_int_seq)
    )
  )

  test ("should reverse another sequence") (
    check (
      obtained = SeqSDBuilder_ ().build (int_seq).opt (ifEmpty = empty_seq) (ifNonEmpty = non_empty_seq)
    ) (
      expected = SeqSDBuilder_ ().build (rev_int_seq)
    )
  )

  lazy val empty_seq : SeqSD [Int] = EmptySeqSD_ ()

  lazy val non_empty_seq : NonEmptySeqSD [Int] => NonEmptySeqSD [Int] =  sequence => sequence.reverse

}
