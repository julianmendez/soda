package soda.example.forlean.lib

/*
 * This package contains helper classes that could be needed for a translation to Rocq.
 */

/*
directive lean
/- Prelude for Soda types. -/
notation "Zero_ ()" => Nat.zero
notation "Succ_" => Nat.succ
*/

trait MyList
{



/*
 * foldl
 * (fold left)
 */

  private def _tailrec_foldl [A , B ] (sequence : List [A] ) (current : B)
      (next_value : B => A => B) : B =
    sequence match  {
      case Nil => current
      case (head) :: (tail) =>
        _tailrec_foldl [A, B] (tail) (next_value (current) (head) ) (next_value)
    }

  def foldl [A , B ] (sequence : List [A] ) (initial_value : B)
      (next_value : B => A => B) : B =
    _tailrec_foldl [A, B] (sequence) (initial_value) (next_value)

/*
 * length
 */

  def length_fl [A ] (list : List [A] ) : Nat =
    foldl [A, Nat] (list) (Zero_ () ) (
       (accum : Nat) =>
         (elem : A) => Succ_ (accum)
    )

/*
  directive lean
  theorem
    len_fl_accum (A : Type) (list : List (A) )
       : forall (accum : Nat) ,
        _tailrec_foldl (A) (Nat) (list) (accum) (
          fun (accum : Nat) => fun (elem : A) => accum + 1) =
        _tailrec_foldl (A) (Nat) (list) (0) (
          fun (accum : Nat) => fun (elem : A) => accum + 1) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl]
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl
*/

  private def _tailrec_length [A ] (list : List [A] ) (accum : Nat) : Nat =
    list match  {
      case Nil => accum
      case (head) :: (tail) =>
        _tailrec_length [A] (tail) (Succ_ (accum) )
    }

/*
  directive lean
  theorem
    len_tr_accum (A : Type) (list : List (A) )
      : forall (accum : Nat) ,
        _tailrec_length (A) (list) (accum)  = _tailrec_length (A) (list) (0) + accum := by
      induction list with
      | nil =>
        intro n
        rewrite [_tailrec_length, _tailrec_length]
        simp
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_length, _tailrec_length]
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl
*/

  def length_tr [A ] (list : List [A] ) : Nat =
    _tailrec_length [A] (list) (Zero_ () )

  def length_def [A ] (list : List [A] ) : Nat =
    list match  {
      case Nil => Zero_ ()
      case (head) :: (tail) => Succ_ (length_def [A] (tail) )
    }

/*
  directive lean
  theorem
    len_fl_eq_len_def (A : Type) (list : List (A))
      : length_fl (A) (list) = length_def (A) (list) := by
    rewrite [length_fl, foldl]
    induction list with
    | nil =>
      rewrite [_tailrec_foldl, length_def]
      rfl
    | cons head tail ih =>
      rewrite [_tailrec_foldl, len_fl_accum]
      rewrite [ih]
      rewrite [length_def]
      rfl
*/

/*
  directive lean
  theorem
    len_tr_eq_len_def
      : length_tr = length_def := by
    funext A list
    rewrite [length_tr]
    induction list with
    | nil =>
      constructor
    | cons head tail ih =>
      rewrite [_tailrec_length, len_tr_accum]
      rewrite [ih]
      rewrite [length_def]
      rfl
*/

  def length [A ] (list : List [A] ) : Nat =
    length_fl [A] (list)

/*
 * reverse
 */

  private def _tailrec_reverse [A ] (list : List [A] ) (accum : List [A] ) : List [A] =
    list match  {
      case Nil => accum
      case (head) :: (tail) => _tailrec_reverse [A] (tail) ( (head) :: (accum) )
    }

  def reverse_tr [A ] (list : List [A] ) : List [A] =
    _tailrec_reverse [A] (list) (Nil)

  def reverse_fl [A ] (list : List [A] ) : List [A] =
    foldl [A, List [A] ] (list) (Nil) (
       (accum : List [A] ) =>
         (elem : A) =>
          (elem) :: (accum)
    )

/*
  directive lean
  theorem
    rev_fl_accum (A : Type) (list : List (A))
      : forall (current: List (A) ),
        _tailrec_foldl (A) (List (A) ) (list) (current)
          (fun (accum : List (A) ) =>
            fun (elem : A) =>
               (elem) :: (accum)
          ) = _tailrec_reverse (A) (list) (current) := by
      induction list with
      | nil =>
        intro other
        rewrite [_tailrec_foldl,_tailrec_reverse]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_foldl,_tailrec_reverse]
        rewrite [ih ((head) :: (other))]
        rfl
*/

/*
  directive lean
  theorem
    rev_tr_eq_rev_fl
      (A : Type) (list : List (A) )
        : reverse_fl (A) (list) = reverse_tr (A) (list) := by
    rewrite [reverse_fl, reverse_tr, foldl, rev_fl_accum]
    rfl
*/

/*
  directive lean
  theorem
    len_rev_accum (A : Type) (list : List (A))
      : forall (accum : List (A) ),
        length_def (A) (_tailrec_reverse (A) (list) (accum)) =
            length_def (A) (_tailrec_reverse (A) (list) ([])) + length_def (A) (accum) := by
      induction list with
      | nil =>
        intro other
        rewrite [_tailrec_reverse, _tailrec_reverse, length_def, Nat.zero_add]
        rfl
      | cons head tail ih =>
        intro other
        rewrite [_tailrec_reverse, _tailrec_reverse]
        rewrite [ih ((head) :: ([]))]
        rewrite [ih ((head) :: (other))]
        rewrite [length_def, length_def, length_def]
        rewrite [Nat.add_assoc, Nat.add_comm 1]
        rfl
*/

  def reverse [A ] (list : List [A] ) : List [A] =
    reverse_fl [A] (list)

}

case class MyList_ () extends MyList

object MyList {
  def mk : MyList =
    MyList_ ()
}


trait Nat
{

  def   add : Nat => Nat
  def   mul : Nat => Nat

}

case class Nat_ (add : Nat => Nat, mul : Nat => Nat) extends Nat

object Nat {
  def mk (add : Nat => Nat) (mul : Nat => Nat) : Nat =
    Nat_ (add, mul)
}

trait Zero
  extends
    Nat
{



  lazy val add : Nat => Nat =
     a => add_for (a)

  def add_for (a : Nat) : Nat =
    a

  lazy val mul : Nat =>  Nat =
     a => mul_for (a)

  def mul_for (a : Nat) : Nat =
    this

}

case class Zero_ () extends Zero

object Zero {
  def mk : Zero =
    Zero_ ()
}

trait Succ
  extends
    Nat
{

  def   k : Nat

  lazy val t = IntNat .mk

  lazy val add : Nat => Nat =
     a => add_for (a)

  def add_for (a : Nat) : Nat =
    t .from_non_negative ( (t .to_Int (k) + 1) + t .to_Int (a) )

  lazy val mul : Nat =>  Nat =
     a => mul_for (a)

  def mul_for (a : Nat) : Nat =
    t .from_non_negative ( (t .to_Int (k) + 1) * t .to_Int (a) )

}

case class Succ_ (k : Nat) extends Succ

object Succ {
  def mk (k : Nat) : Succ =
    Succ_ (k)
}

trait IntNat
{



  import   soda.lib.NoneSD
  import   soda.lib.OptionSD
  import   soda.lib.SomeSD

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_non_negative (a : Int) (b : Nat) : Nat =
    if ( a <= 0
    ) b
    else _tailrec_from_non_negative (a - 1) (Succ_ (b) )

  def from_non_negative (a : Int) : Nat =
    _tailrec_from_non_negative (a) (Zero_ () )

  def from_Int (a : Int) : OptionSD [Nat] =
    if ( a < 0
    ) NoneSD .mk [Nat]
    else SomeSD .mk [Nat] (from_non_negative (a) )

  def to_Int (a : Nat) : Int =
    a match  {
      case Succ_ (k) => 1 + to_Int (k)
      case _otherwise => 0
    }

}

case class IntNat_ () extends IntNat

object IntNat {
  def mk : IntNat =
    IntNat_ ()
}


trait SimpleList [A ]
{



}

case class SimpleList_ [A] () extends SimpleList [A]

object SimpleList {
  def mk [A] : SimpleList [A] =
    SimpleList_ [A] ()
}

trait nil [A ]
  extends
    SimpleList [A]
{



}

case class nil_ [A] () extends nil [A]

object nil {
  def mk [A] : nil [A] =
    nil_ [A] ()
}

trait cons [A ]
  extends
    SimpleList [A]
{

  def   e : A
  def   s : SimpleList [A]

}

case class cons_ [A] (e : A, s : SimpleList [A]) extends cons [A]

object cons {
  def mk [A] (e : A) (s : SimpleList [A]) : cons [A] =
    cons_ [A] (e, s)
}

trait SeqList
{



  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_reverse [A ] (a : SimpleList [A] ) (b : SimpleList [A] ) : SimpleList [A] =
    a match  {
      case cons_ (e , s) => _tailrec_reverse (s) (cons_ (e , b) )
      case _otherwise => b
    }

  def reverse [A ] (s : SimpleList [A] ) : SimpleList [A] =
    _tailrec_reverse [A] (s) (nil_ [A] () )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_from_Seq [A ] (a : Seq [A] ) (b : SimpleList [A] ) : SimpleList [A] =
    a match  {
      case (e) :: (s) => _tailrec_from_Seq (s) (cons_ (e , b) )
      case _otherwise => b
    }

  def from_Seq [A ] (a : Seq [A] ) : SimpleList [A] =
    reverse (_tailrec_from_Seq (a) (nil_ [A] () ) )

  import scala.annotation.tailrec
        @tailrec  final
  private def _tailrec_to_Seq [A ] (a : SimpleList [A] ) (b : Seq [A] ) : Seq [A] =
    a match  {
      case cons_ (e , s) => _tailrec_to_Seq (s) (b .+: (e) )
      case _otherwise => b
    }

  def to_Seq [A ] (a : SimpleList [A] ) : Seq [A] =
    (_tailrec_to_Seq (a) (Seq [A] () ) ) .reverse

}

case class SeqList_ () extends SeqList

object SeqList {
  def mk : SeqList =
    SeqList_ ()
}

