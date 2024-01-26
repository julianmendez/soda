package soda.lib

/*
 * This package contains tests for the Soda library.
 */

trait Package

case class CartesianProductSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val int_seq_a = Seq (10 , 20)

  lazy val int_seq_b = Seq (100 , 200 , 300)

  lazy val str_seq_a = Seq ("A" , "B")

  lazy val str_seq_b = Seq ("0" , "1" , "2")

  lazy val str_seq_c = Seq ("a" , "b" , "c" , "d")

  lazy val instance = CartesianProduct_ ()

  test ("Cartesian product of two sequences") (
    check (
      obtained = instance .apply (Seq (int_seq_a , int_seq_b) )
    ) (
      expected = Seq (
        Seq (10 , 100) , Seq (10 , 200) , Seq (10 , 300),
        Seq (20 , 100) , Seq (20 , 200) , Seq (20 , 300)
      )
    )
  )

  test ("Cartesian product of an empty sequence") (
    check (
      obtained = instance .apply (Seq () )
    ) (
      expected = Seq ()
    )
  )

  test ("Cartesian product of only empty sequences") (
    check (
      obtained = instance .apply (Seq (Seq () , Seq () , Seq () ) )
    ) (
      expected = Seq ()
    )
  )

  test ("Cartesian product with at least one empty sequence") (
    check (
      obtained = instance .apply (Seq (Seq ("A") , Seq () ) )
    ) (
      expected = Seq ()
    )
  )

  test ("Cartesian product of three sequences") (
    check (
      obtained = instance .apply (Seq (str_seq_a , str_seq_b , str_seq_c) )
    ) (
      expected = Seq (
        Seq ("A" , "0" , "a") , Seq ("A" , "0" , "b") , Seq ("A" , "0" , "c") ,
        Seq ("A" , "0" , "d") ,
        Seq ("A" , "1" , "a") , Seq ("A" , "1" , "b") , Seq ("A" , "1" , "c") ,
        Seq ("A" , "1" , "d") ,
        Seq ("A" , "2" , "a") , Seq ("A" , "2" , "b") , Seq ("A" , "2" , "c") ,
        Seq ("A" , "2" , "d") ,
        Seq ("B" , "0" , "a") , Seq ("B" , "0" , "b") , Seq ("B" , "0" , "c") ,
        Seq ("B" , "0" , "d") ,
        Seq ("B" , "1" , "a") , Seq ("B" , "1" , "b") , Seq ("B" , "1" , "c") ,
        Seq ("B" , "1" , "d") ,
        Seq ("B" , "2" , "a") , Seq ("B" , "2" , "b") , Seq ("B" , "2" , "c") ,
        Seq ("B" , "2" , "d")
      )
    )
  )

}


trait DayOfTheWeek
  extends
    EnumConstant
{

  def   ordinal : Int
  def   name : String

}

case class DayOfTheWeek_ (ordinal : Int, name : String) extends DayOfTheWeek

object DayOfTheWeek {
  def mk (ordinal : Int) (name : String) : DayOfTheWeek =
    DayOfTheWeek_ (ordinal, name)
}

trait DayOfTheWeekConstant
{

  private def _mk_DayOfTheWeek (ordinal : Int) (name : String) : DayOfTheWeek =
    DayOfTheWeek_ (ordinal, name)

  lazy val sunday = _mk_DayOfTheWeek (0) ("Sunday")

  lazy val monday = _mk_DayOfTheWeek (1) ("Monday")

  lazy val tuesday = _mk_DayOfTheWeek (2) ("Tuesday")

  lazy val wednesday = _mk_DayOfTheWeek (3) ("Wednesday")

  lazy val thursday = _mk_DayOfTheWeek (4) ("Thursday")

  lazy val friday = _mk_DayOfTheWeek (5) ("Friday")

  lazy val saturday = _mk_DayOfTheWeek (6) ("Saturday")

  lazy val DayOfTheWeek_values = Seq (sunday , monday , tuesday , wednesday , thursday , friday , saturday)

}

case class DayOfTheWeekConstant_ () extends DayOfTheWeekConstant

object DayOfTheWeekConstant {
  def mk : DayOfTheWeekConstant =
    DayOfTheWeekConstant_ ()
}

trait DayOfTheWeekEnum
  extends
    DayOfTheWeekConstant
{

  lazy val values = DayOfTheWeek_values

}

case class DayOfTheWeekEnum_ () extends DayOfTheWeekEnum

object DayOfTheWeekEnum {
  def mk : DayOfTheWeekEnum =
    DayOfTheWeekEnum_ ()
}

case class EnumSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  test ("the names of the elements in enumerations") (
    check (
      obtained = DayOfTheWeekEnum_ () .values .map ( x => x .toString)
    ) (
      expected = Seq ("DayOfTheWeek_(0,Sunday)" , "DayOfTheWeek_(1,Monday)" ,
        "DayOfTheWeek_(2,Tuesday)" , "DayOfTheWeek_(3,Wednesday)" ,
        "DayOfTheWeek_(4,Thursday)" , "DayOfTheWeek_(5,Friday)" , "DayOfTheWeek_(6,Saturday)" )
    )
  )

}


case class OptionSDSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  import   scala.util.Try

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val result_if_empty : String = "It is empty."

  def result_if_non_empty (value : String) : String =
    "Its value is " + value + "."

  def int_to_string (n : Int) : String =
    "" + n

  def maybe_int_to_string (n : Int) : SomeSD_ [String] =
    SomeSD_ [String] ("" + n)

  def maybe_string_to_int (s : String) : OptionSD [Int] =
    OptionSDBuilder_ () .build (
      Try (Integer .parseInt (s .trim) )
        .toOption
    )

  test ("should test an empty option") (
    check (
      obtained = empty_opt .isEmpty && ! empty_opt .isDefined && ! empty_opt .isNonEmpty
    ) (
      expected = true
    )
  )

  lazy val empty_opt = NoneSD_ ()

  test ("should test a non empty option") (
    check (
      obtained = ! non_empty_opt .isEmpty && non_empty_opt .isDefined && non_empty_opt .isNonEmpty
    ) (
      expected = true
    )
  )

  lazy val non_empty_opt = SomeSD_ (1)

  test ("should get a default value, when empty") (
    check (
      obtained = (NoneSD_ [Int] () ) .getOrElse (1)
    ) (
      expected = 1
    )
  )

  test ("should get a value") (
    check (
      obtained = (SomeSD_ [Int] (2) ) .getOrElse (1)
    ) (
      expected = 2
    )
  )

  test ("should open an empty option") (
    check (
      obtained = (NoneSD_ [String] () ) .opt (ifEmpty = result_if_empty) (
        ifNonEmpty = result_if_non_empty)
    ) (
      expected = "It is empty."
    )
  )

  test ("should open an non empty option") (
    check (
      obtained = (SomeSD_ [String] ("0") ) .opt (ifEmpty = result_if_empty) (
        ifNonEmpty = result_if_non_empty)
    ) (
      expected = "Its value is 0."
    )
  )

  test ("should try fold an empty option") (
    check (
      obtained = (NoneSD_ [String] () ) .fold (ifEmpty = result_if_empty) (
        f = result_if_non_empty)
    ) (
      expected = "It is empty."
    )
  )

  test ("should try fold an non empty option") (
    check (
      obtained = (SomeSD_ [String] ("0") ) .fold (ifEmpty = result_if_empty) (
        f = result_if_non_empty)
    ) (
      expected = "Its value is 0."
    )
  )

  test ("should map empty to empty") (
    check (
      obtained = (NoneSD_ [Int] () ) .map (int_to_string)
    ) (
      expected = NoneSD_ [String] ()
    )
  )

  test ("should map a non-empty to another non-empty") (
    check (
      obtained = (SomeSD_ [Int] (2) ) .map (int_to_string)
    ) (
      expected = SomeSD_ [String] ("2")
    )
  )

  test ("should flat map empty to empty") (
    check (
      obtained = (NoneSD_ [Int] () ) .flatMap (maybe_int_to_string)
    ) (
      expected = NoneSD_ [String] ()
    )
  )

  test ("should flat map a non-empty to another non-empty") (
    check (
      obtained = (SomeSD_ [Int] (2) ) .flatMap (maybe_int_to_string)
    ) (
      expected = SomeSD_ [String] ("2")
    )
  )

  test ("should try how successive applications of open works") (
    check (
      obtained =
        maybe_string_to_int ("1") .opt (ifEmpty = _empty_opt ) (
          ifNonEmpty =  a =>
            maybe_string_to_int ("2") .opt (ifEmpty = _empty_opt ) (
              ifNonEmpty =  b =>
                maybe_string_to_int ("3") .opt (ifEmpty = _empty_opt ) (
                  ifNonEmpty =  c =>
                    SomeSD_ (a + b + c) ) ) )
    ) (
      expected = SomeSD_ (6)
    )
  )

  private lazy val _empty_opt : OptionSD [Int] = NoneSD_ [Int] ()

  test ("toOption with non empty option") (
    check (
      obtained = (SomeSD_ [Int] (1) ) .toOption
    ) (
      expected = Some (1)
    )
  )

  test ("toOption with another non empty option") (
    check (
      obtained = (SomeSD_ [Int] (2) ) .toOption
    ) (
      expected = Some (2)
    )
  )

  test ("toOption with empty option") (
    check (
      obtained = (NoneSD_ [Int] () ) .toOption
    ) (
      expected = None
    )
  )

  test ("toSeq with non empty option") (
    check (
      obtained = (SomeSD_ (1) ) .toSeq
    ) (
      expected = Seq (1)
    )
  )

  test ("toSeq with another non empty option") (
    check (
      obtained = (SomeSD_ (2) ) .toSeq
    ) (
      expected = Seq (2)
    )
  )

  test ("toSeq with empty option") (
    check (
      obtained = (NoneSD_ () ) .toSeq
    ) (
      expected = Seq ()
    )
  )

  test ("filter should work for None") (
    check (
      obtained = (NoneSD_ [Int] () ) .filter ( x => true)
    ) (
      expected = NoneSD_ [Int] ()
    )
  )

  test ("filter should work for Some, if predicate does not hold") (
    check (
      obtained = (SomeSD_ [Int] (0) ) .filter ( x => x > 0)
    ) (
      expected = NoneSD_ [Int] ()
    )
  )

  test ("filter should work for Some, if predicate holds") (
    check (
      obtained = (SomeSD_ [Int] (1) ) .filter ( x => x > 0)
    ) (
      expected = SomeSD_ [Int] (1)
    )
  )

}


case class RecursionSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val example_seq : Seq [Int] = Seq (0 , 1 , 1 , 2 , 3 , 5 , 8)

  private lazy val _fold_while = FoldWhile_ ()

  private lazy val _fold = Fold_ ()

  private lazy val _range = Range_ ()

  test ("fold left while with Seq") (
    check (
      obtained = _fold_while .apply (example_seq) (_fold0_initial_value) (
        _fold0_next_value_function) (_fold0_condition)
    ) (
      expected = Seq ("103" , "102" , "101" , "101" , "100")
    )
  )

  private lazy val _fold0_initial_value = Seq [String] ()

  private lazy val _fold0_next_value_function : Seq [String] => Int => Seq [String] =
     (s : Seq [String] ) =>  (e : Int) => (s .+: ("" + (e + 100) ) )

  private lazy val _fold0_condition : Seq [String] => Int => Boolean =
     (s : Seq [String] ) =>  (e : Int) => (e < 5)

  test ("fold left with Seq") (
    check (
      obtained = _fold .apply (example_seq) (_fold1_initial_value) (
        _fold1_next_value_function)
    ) (
      expected = Seq ("108" , "105" , "103" , "102" , "101" , "101" , "100")
    )
  )

  private lazy val _fold1_initial_value = Seq [String] ()

  private lazy val _fold1_next_value_function : Seq [String] => Int => Seq [String] =
     (s : Seq [String] ) =>  (e : Int) => (s .+: ("" + (e + 100) ) )

  test ("range with positive number") (
    check (
      obtained = _range .apply (8)
    ) (
      expected = Seq [Int] (0 , 1 , 2 , 3 , 4 , 5 , 6 , 7)
    )
  )

  test ("range with zero size") (
    check (
      obtained = _range .apply (-1)
    ) (
      expected = Seq [Int] ()
    )
  )

  test ("range with negative number") (
    check (
      obtained = _range .apply (-1)
    ) (
      expected = Seq [Int] ()
    )
  )

}


case class SeqSDSpec ()
  extends
    org.scalatest.funsuite.AnyFunSuite
{

  def check [A ] (obtained : A) (expected : A) : org.scalatest.compatible.Assertion =
    assert (obtained == expected)

  lazy val int_seq : Seq [Int] = Seq (2 , 7 , 1 , 8 , 2 , 8 , 1 , 8 , 2 , 8 , 4 , 5 , 9)

  lazy val rev_int_seq : Seq [Int] = Seq (9 , 5 , 4 , 8 , 2 , 8 , 1 , 8 , 2 , 8 , 1 , 7 , 2)

  test ("should detect an empty sequence") (
    check (
      obtained = SeqSDBuilder_ () .build (Seq [Int] () ) .opt (ifEmpty = true) (
        ifNonEmpty =  nonEmpty => false)
    ) (
      expected = true
    )
  )

  test ("should detect an non empty sequence") (
    check (
      obtained = SeqSDBuilder_ () .build (Seq [Int] (1) ) .opt (ifEmpty = false) (
        ifNonEmpty =  nonEmpty => true)
    ) (
      expected = true
    )
  )

  lazy val empty_opt : OptionSD [Int] = NoneSD_ ()

  lazy val non_empty_opt : NonEmptySeqSD [Int] => SomeSD [Int] =
     sequence => SomeSD_ (max (sequence) )

  test ("should get the maximum") (
    check (
      obtained = SeqSDBuilder_ () .build (int_seq) .opt (ifEmpty = empty_opt) (
        ifNonEmpty = non_empty_opt)
    ) (
      expected = SomeSD_ [Int] (9)
    )
  )

  private lazy val _fold = Fold_ ()

  def max_of_2 (a : Int) (b : Int) : Int =
    if ( a > b ) a else b

  def max (s : NonEmptySeqSD [Int] ) : Int =
    _fold .apply (s .tail .toSeq) (s .head) (max_of_2)

  test ("should reverse a sequence") (
    check (
      obtained = SeqSDBuilder_ () .build (int_seq) .reverse
    ) (
      expected = SeqSDBuilder_ () .build (rev_int_seq)
    )
  )

  lazy val empty_seq : SeqSD [Int] = EmptySeqSD_ ()

  lazy val non_empty_seq : NonEmptySeqSD [Int] => NonEmptySeqSD [Int] =
     sequence => sequence .reverse

  test ("should reverse another sequence") (
    check (
      obtained = SeqSDBuilder_ () .build (int_seq) .opt (ifEmpty = empty_seq) (
        ifNonEmpty = non_empty_seq)
    ) (
      expected = SeqSDBuilder_ () .build (rev_int_seq)
    )
  )

}

