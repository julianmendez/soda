package soda.example.mathematics

/*
 * This package contains examples in Soda.
 * These examples use mathematical properties.
 */

trait FactorialConcise
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fact (n : Int) (accum : Int) : Int =
    if ( n < 2
    ) accum
    else _tailrec_fact (n - 1) (n * accum)

  def apply (n : Int) : Int =
    _tailrec_fact (n) (1)

}

case class FactorialConcise_ () extends FactorialConcise

object FactorialConcise {
  def mk : FactorialConcise =
    FactorialConcise_ ()
}


trait FactorialPatternMatching
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_fact (n : Int) (accum : Int) : Int =
    n match  {
      case 0 => accum
      case _otherwise => _tailrec_fact (n - 1) (n * accum)
    }

  def apply (n : Int) : Int =
    if ( n < 0
    ) 1
    else _tailrec_fact (n) (1)

}

case class FactorialPatternMatching_ () extends FactorialPatternMatching

object FactorialPatternMatching {
  def mk : FactorialPatternMatching =
    FactorialPatternMatching_ ()
}


trait FactorialSimple
{



  def apply (n : Int) : Int =
    if ( n < 2
    ) 1
    else n * apply (n - 1)

}

case class FactorialSimple_ () extends FactorialSimple

object FactorialSimple {
  def mk : FactorialSimple =
    FactorialSimple_ ()
}


trait FactorialWithFold
{



  lazy val fold = soda.lib.Fold_ ()

  lazy val range = soda.lib.Range_ ()

  def apply (n : Int) : Int =
    fold .apply [Int, Int] (range .apply (n) ) (1) (
       accum =>
         k => (accum * (k + 1) ) )

}

case class FactorialWithFold_ () extends FactorialWithFold

object FactorialWithFold {
  def mk : FactorialWithFold =
    FactorialWithFold_ ()
}


trait NonNegativeMod
{

  def invariant (v : Int) : Boolean =
    v >= 0

  def mko (v : Int) : Option [Int] =
    if ( invariant (v)
    ) Some (v)
    else None

  private def _plus (v : Int) (b : Option [Int] ) : Option [Int] =
    b match  {
      case Some (w) => mko (v + w)
      case None => None
    }

  def plus (a : Option [Int] ) (b : Option [Int] ) : Option [Int] =
    a match  {
      case Some (v) => _plus (v) (b)
      case None => None
    }

  private def _minus1 (v : Int) : Option [Int] =
    if ( v > 0
    ) Option [Int] (v - 1)
    else None

  def minus1 (a : Option [Int]) : Option [Int] =
    a match  {
      case Some (v) => _minus1 (v)
      case None => None
    }

}

case class NonNegativeMod_ () extends NonNegativeMod

object NonNegativeMod {
  def mk : NonNegativeMod =
    NonNegativeMod_ ()
}

trait FiboAlternativeExampleInSoda
{



  private lazy val _mm = NonNegativeMod .mk

  private lazy val _zero = _mm .mko (0)

  private lazy val _one = _mm .mko (1)

  private def _rec (m : Option [Int] ) (a : Option [Int] ) (b : Option [Int] ) : Option [Int] =
    if ( m == _zero ) a
    else if ( m == _one ) b
    else _rec (_mm .minus1 (m) ) (b) (_mm .plus (a) (b) )

  private def _apply (n : Option [Int] ) : Option [Int] =
    _rec (n) (_zero) (_one)

  def apply (n : Int) : Int =
    _apply (_mm .mko (n) ) match  {
      case Some (v) => v
      case None => -1
    }

}

case class FiboAlternativeExampleInSoda_ () extends FiboAlternativeExampleInSoda

object FiboAlternativeExampleInSoda {
  def mk : FiboAlternativeExampleInSoda =
    FiboAlternativeExampleInSoda_ ()
}




trait NonNegative
{

  def   v : Int

  lazy val invariant : Boolean =
    v >= 0

  lazy val check : Option [NonNegative] =
    if ( invariant
    ) Some (this)
    else None

}

case class NonNegative_ (v : Int) extends NonNegative

object NonNegative {
  def mk (v : Int) : NonNegative =
    NonNegative_ (v)
}

trait FiboExampleInSoda
{



  private def _plus (a : NonNegative) (b : NonNegative) : NonNegative =
    NonNegative .mk (a .v + b .v)

 private def _monus1 (a : NonNegative) : NonNegative =
   if ( a .v > 0
   ) NonNegative .mk (a .v - 1)
   else a

  private def _rec (m : NonNegative) (a : NonNegative) (b : NonNegative) : NonNegative =
    if ( m .v == 0 ) a
    else if ( m .v == 1 ) b
    else _rec (_monus1 (m) ) (b) (_plus (a) (b) )

  private def _apply (n : NonNegative) : NonNegative =
    _rec (n) (NonNegative .mk (0) ) (NonNegative .mk (1) )

  def apply (n : Int) : Int =
    NonNegative .mk (n) .check match  {
      case Some (non_negative) => (_apply (non_negative) ) .v
      case None => -1
    }

}

case class FiboExampleInSoda_ () extends FiboExampleInSoda

object FiboExampleInSoda {
  def mk : FiboExampleInSoda =
    FiboExampleInSoda_ ()
}


trait FiboUnicodeExample
{



  private def _rec (m : Int) (a : Int) (b : Int) : Int =
    if ( m == 0 ) a
     else if ( m == 1 ) b
        else _rec (m - 1) (b) (a + b)

  def apply (n : Int) : Int =
    _rec (n) (0) (1)

}

case class FiboUnicodeExample_ () extends FiboUnicodeExample

object FiboUnicodeExample {
  def mk : FiboUnicodeExample =
    FiboUnicodeExample_ ()
}


trait InputPair [A , B ]
{

  def   value : A
  def   memoized_values : Map [A, B]

}

case class InputPair_ [A, B] (value : A, memoized_values : Map [A, B]) extends InputPair [A, B]

object InputPair {
  def mk [A, B] (value : A) (memoized_values : Map [A, B]) : InputPair [A, B] =
    InputPair_ [A, B] (value, memoized_values)
}

trait OutputPair [A , B ]
{

  def   value : B
  def   memoized_values : Map [A, B]

}

case class OutputPair_ [A, B] (value : B, memoized_values : Map [A, B]) extends OutputPair [A, B]

object OutputPair {
  def mk [A, B] (value : B) (memoized_values : Map [A, B]) : OutputPair [A, B] =
    OutputPair_ [A, B] (value, memoized_values)
}

trait MemoizableFunction [A , B ]
{

    /** compute (input : InputPair [A] [B] ) : OutputPair [A] [B] */
  def   abs_compute : InputPair [A, B] => OutputPair [A, B]

  def compute (input : InputPair [A, B] ) : OutputPair [A, B] =
    abs_compute (input)

  def mk_InputPair (value : A) (memoized_values : Map [A, B] ) : InputPair [A, B] =
    InputPair_ (value, memoized_values)

  def mk_OutputPair (value : B) (memoized_values : Map [A, B] ) : OutputPair [A, B] =
    OutputPair_ (value, memoized_values)

}

case class MemoizableFunction_ [A, B] (abs_compute : InputPair [A, B] => OutputPair [A, B]) extends MemoizableFunction [A, B]

object MemoizableFunction {
  def mk [A, B] (abs_compute : InputPair [A, B] => OutputPair [A, B]) : MemoizableFunction [A, B] =
    MemoizableFunction_ [A, B] (abs_compute)
}

trait MainFunction [A , B ]
{

  def   main_function : InputPair [A, B] => OutputPair [A, B]

}

case class MainFunction_ [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) extends MainFunction [A, B]

object MainFunction {
  def mk [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) : MainFunction [A, B] =
    MainFunction_ [A, B] (main_function)
}

trait Memoizer [A , B ]
  extends
    MemoizableFunction [A, B]
    with MainFunction [A, B]
{

  def   main_function : InputPair [A, B] => OutputPair [A, B]

  private def _add_element (output : OutputPair [A, B] ) (new_pair : Tuple2 [A, B] )
      : OutputPair [A, B] =
    mk_OutputPair (output .value) (output .memoized_values + new_pair)

  private def _compute_and_update_with (input_value : A) (output : OutputPair [A, B] )
      : OutputPair [A, B] =
    _add_element (output) (Tuple2 (input_value, output .value) )

  def compute_and_update (input : InputPair [A, B] ) : OutputPair [A, B] =
    _compute_and_update_with (input .value) (main_function (input) )

  private def _compute_with (maybe_res : Option [B] ) (input : InputPair [A, B] ) : OutputPair [A, B] =
    if ( maybe_res .isEmpty
    ) compute_and_update (input)
    else mk_OutputPair (maybe_res .get) (input .memoized_values)

  def compute_for (input : InputPair [A, B] ) : OutputPair [A, B] =
    _compute_with (input .memoized_values .get (input .value) ) (input)

  lazy val abs_compute : InputPair [A, B] => OutputPair [A, B] =
     input =>
      compute_for (input)

}

case class Memoizer_ [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) extends Memoizer [A, B]

object Memoizer {
  def mk [A, B] (main_function : InputPair [A, B] => OutputPair [A, B]) : Memoizer [A, B] =
    Memoizer_ [A, B] (main_function)
}

trait HardProblem
  extends
    MemoizableFunction [Int, Int]
{



  def is_even (n : Int) : Boolean =
    n % 2 == 0

  def one_step (n : Int) : Int =
    if ( is_even (n)
    ) n / 2
    else 3 * n + 1

  private def _plus_one (pair : OutputPair [Int, Int] ) : OutputPair [Int, Int] =
    mk_OutputPair (1 + pair .value) (pair .memoized_values)

  lazy val main_function : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      if ( input .value == 1
      ) mk_OutputPair (0) (input .memoized_values)
      else _plus_one (compute (
        mk_InputPair (one_step (input .value) ) (input .memoized_values) ) )

  lazy val memoizer = Memoizer_ [Int, Int] (main_function)

  def compute_for (input : InputPair [Int, Int] ) : OutputPair [Int, Int] =
    memoizer .compute (input)

  lazy val abs_compute : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input)

}

case class HardProblem_ () extends HardProblem

object HardProblem {
  def mk : HardProblem =
    HardProblem_ ()
}

trait MemoizedFibonacci
  extends
    MemoizableFunction [Int, Int]
{



  private def _get_next_fibo (a : Int) (b : Int) : Int =
    a + b

  private def _compute_and_update_3 (res : Int) (second_map : Map [Int, Int] ) (n : Int)
      : OutputPair [Int, Int] =
    mk_OutputPair (res) (second_map + Tuple2 (n, res) )

  private def _compute_and_update_2 (first_value : Int) (second_tuple : OutputPair [Int, Int] ) (n : Int )
      : OutputPair [Int, Int] =
    _compute_and_update_3 (_get_next_fibo (first_value) (second_tuple .value) ) (
      second_tuple .memoized_values) (n)

  private def _compute_and_update_1 (first_tuple : OutputPair [Int, Int] ) (n : Int )
      : OutputPair [Int, Int] =
    _compute_and_update_2 (first_tuple .value) (compute (
      mk_InputPair (n - 1) (first_tuple .memoized_values) ) ) (n)

  def main_function_for (input : InputPair [Int, Int] ): OutputPair [Int, Int] =
    if ( (input .value == 0) || (input .value == 1)
    ) mk_OutputPair (input .value) (input .memoized_values )
    else _compute_and_update_1 (compute (
      mk_InputPair (input .value - 2) (input .memoized_values ) ) ) (input .value)

  lazy val main_function : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      main_function_for (input)

  lazy val memoizer = Memoizer_ [Int, Int] (main_function)

  def compute_for (input : InputPair [Int, Int] ) : OutputPair [Int, Int] =
    memoizer .compute (input)

  lazy val abs_compute : InputPair [Int, Int] => OutputPair [Int, Int] =
     input =>
      compute_for (input)

}

case class MemoizedFibonacci_ () extends MemoizedFibonacci

object MemoizedFibonacci {
  def mk : MemoizedFibonacci =
    MemoizedFibonacci_ ()
}


/*
 * This is an implementation of Nat in Scala for Soda.
 */

sealed trait Nat {
  def v : Int
}
case class _Nat_ (v : Int) extends Nat {
}
object Nat {
  def mk (v : Int) : Nat =
    if (v <= 0) Zero_ else _Nat_ (v)
}
case object Zero_ extends Nat {
  def v : Int = 0
}
object Succ_ {
  def apply (n : Nat) : Nat =
    if (n.v <= 0) Nat.mk (1) else Nat.mk (n.v + 1)
  def unapply (n : Nat) : Option [Nat] =
    if (n.v <= 0) None else Some (Nat.mk (n.v - 1) )
}


trait Status
{

  def   r : BigInt
  def   n : Int
  def   q : BigInt
  def   t : BigInt
  def   l : Int
  def   k : Int

  override
  lazy val toString = " r=" + r + " n=" + n + " q=" + q + " t=" + t + " l=" + l + " k=" + k

}

case class Status_ (r : BigInt, n : Int, q : BigInt, t : BigInt, l : Int, k : Int) extends Status

object Status {
  def mk (r : BigInt) (n : Int) (q : BigInt) (t : BigInt) (l : Int) (k : Int) : Status =
    Status_ (r, n, q, t, l, k)
}

trait IntAndStatus
{

  def   digit : Int
  def   new_status : Status

}

case class IntAndStatus_ (digit : Int, new_status : Status) extends IntAndStatus

object IntAndStatus {
  def mk (digit : Int) (new_status : Status) : IntAndStatus =
    IntAndStatus_ (digit, new_status)
}

trait PiIterator
{



  private def _mk_Status (r : BigInt) (n : Int) (q : BigInt) (t : BigInt) (l : Int) (k : Int) : Status =
    Status_ (r, n, q, t, l, k)

  private def _mk_IntAndStatus (digit : Int) (new_status : Status) : IntAndStatus =
    IntAndStatus_ (digit, new_status)

  private lazy val _initial_status =
    _mk_Status (r = 0) (n = 3) (q = 1) (t = 1) (l = 3) (k = 1)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_compute_new_status (s : Status) : Status =
    if ( (4 * s .q + s .r - s .t) < (s .n * s .t)
    ) s
    else
      _tailrec_compute_new_status (
        _mk_Status (
          r = (2 * s .q + s .r) * s .l) (
          n = ( (s .q * (7 * s .k) + 2 + (s .r * s .l) ) / (s .t * s .l) ) .toInt) (
          q = s .q * s .k) (
          t = s .t * s .l) (
          l = s .l + 2) (
          k = s .k + 1
        )
      )

  private def _compute_new_status (s : Status) : Status =
    _tailrec_compute_new_status (s)

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_take (n : Int) (rev_seq : Seq [Int] ) (s : Status) (t : IntAndStatus) : Seq [Int] =
    if ( n == 0
    ) rev_seq .reverse
    else _tailrec_take (n - 1) (rev_seq .+: (t .digit) ) (t .new_status) (
      _get_next (t .new_status) )

  private def _get_next_with_new_status (s : Status) : IntAndStatus =
    _mk_IntAndStatus (s .n) (
      _mk_Status (
        r = 10 * (s .r - s .n * s .t)) (
        n = ( ( (10 * (3 * s .q + s .r) ) / s .t) - (10 * s .n) ) .toInt) (
        q = s .q * 10) (
        t = s .t) (
        l = s .l) (
        k = s .k
      )
    )

  private def _get_next (s : Status) : IntAndStatus =
    _get_next_with_new_status (_compute_new_status (s) )

  def apply (n : Int) : Seq [Int] =
    _tailrec_take (n) (Seq () ) (_initial_status) (_get_next (_initial_status) )

}

case class PiIterator_ () extends PiIterator

object PiIterator {
  def mk : PiIterator =
    PiIterator_ ()
}

