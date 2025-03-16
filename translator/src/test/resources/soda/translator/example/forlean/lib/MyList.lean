/- Prelude for Soda types. -/
notation "Zero_ ()" => Nat.zero
notation "Succ_" => Nat.succ

class MyList

where
  mk ::
    
  deriving DecidableEq

namespace MyList


/-foldl
 (fold left)
-/

private def   _tailrec_foldl ( A : Type ) ( B : Type ) (sequence : List ( A ) ) (current : B)
      (next_value : B -> A -> B) : B :=
    match sequence with
      | List.nil => current
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

private def   _tailrec_length ( A : Type ) (list : List ( A ) ) (accum : Nat) : Nat :=
    match list with
      | List.nil => accum
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
        rewrite [ih (1)]
        rewrite [ih (n + 1)]
        rewrite [Nat.add_assoc]
        rewrite [Nat.add_comm 1]
        rfl

def   length_tr ( A : Type ) (list : List ( A ) ) : Nat :=
    _tailrec_length ( A ) (list) (Zero_ () )


def   length_def ( A : Type ) (list : List ( A ) ) : Nat :=
    match list with
      | List.nil => Zero_ ()
      | (head) :: (tail) => Succ_ (length_def ( A ) (tail) )
    


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

def   length ( A : Type ) (list : List ( A ) ) : Nat :=
    length_fl ( A ) (list)


/- reverse
-/

private def   _tailrec_reverse ( A : Type ) (list : List ( A ) ) (accum : List ( A ) ) : List ( A ) :=
    match list with
      | List.nil => accum
      | (head) :: (tail) => _tailrec_reverse ( A ) (tail) ( (head) :: (accum) )
    


def   reverse_tr ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    _tailrec_reverse ( A ) (list) (List.nil)


def   reverse_fl ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    foldl ( A ) ( List ( A )  ) (list) (List.nil) (
      fun (accum : List ( A ) ) =>
        fun (elem : A) =>
          (elem) :: (accum)
    )


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

  theorem
    rev_tr_eq_rev_fl
      (A : Type) (list : List (A) )
        : reverse_fl (A) (list) = reverse_tr (A) (list) := by
    rewrite [reverse_fl, reverse_tr, foldl, rev_fl_accum]
    rfl

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

def   reverse ( A : Type ) (list : List ( A ) ) : List ( A ) :=
    reverse_fl ( A ) (list)


end MyList

notation "MyList_" => MyList.mk
