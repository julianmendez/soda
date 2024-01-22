/- Prelude for Soda types. -/
notation "Boolean" => Bool
notation "None" => Option.none
notation "Some" => Option.some
notation "Nil" => List.nil
notation "Zero_ ()" => Nat.zero
notation "Succ_ " =>  Nat.succ

class MyList

where
  MyList_ ::
    
  deriving DecidableEq

namespace MyList


/-foldl
 (fold left)
-/

private def   _tailrec_foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
       (next_value : B -> A -> B) : B :=
    match sequence with
      | [] => current
      | (head) :: (tail) =>
        _tailrec_foldl ( A ) ( B ) (tail) (next_value (current) (head) ) (next_value)
    


def   foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (initial_value : B)
       (next_value : B -> A -> B) : B :=
    _tailrec_foldl ( A ) ( B ) (sequence) (initial_value) (next_value)


/- length
-/

 def   length_fl ( A : Type ) (list : List ( A ) ) : Nat :=
    foldl ( A ) ( Nat ) (list) (Zero_ () ) (
      fun (accum : Nat) =>
        fun (elem : A) => Succ_ (accum)
    )


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
        rewrite [_tailrec_foldl, _tailrec_foldl]
        simp
      | cons head tail ih =>
        intro n
        rewrite [_tailrec_foldl, _tailrec_foldl]
        have h1 := by exact ih (1)
        have h2 := by exact ih (n + 1)
        rewrite [h1]
        rewrite [h2]
        simp [Nat.add_assoc, Nat.add_comm]

 private def   _tailrec_length ( A : Type ) (list : List ( A ) ) (accum : Nat) : Nat :=
    match list with
      | [] => accum
      | (head) :: (tail) =>
        _tailrec_length ( A ) (tail) (Succ_ (accum) )
    


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
        have h1 := by exact ih (1)
        have h2 := by exact ih (n + 1)
        rewrite [h1]
        rewrite [h2]
        simp [Nat.add_assoc, Nat.add_comm]

 def   length_tr ( A : Type ) (list : List ( A ) ) : Nat :=
    _tailrec_length ( A ) (list) (Zero_ () )


 def   length_def ( A : Type ) (list : List ( A ) ) : Nat :=
    match list with
      | [] => Zero_ ()
      | (head) :: (tail) => Succ_ (length_def ( A ) (tail) )
    


theorem    len_fl_eq_len_def (A : Type) (list : List (A))
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
      simp


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
      simp

 def   length ( A : Type ) (list : List ( A ) ) : Nat :=
    length_fl ( A ) (list)


end MyList

notation "MyList_" => MyList.MyList_
