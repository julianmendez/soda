Module TriangularNumberForRocq .

Class TriangularNumberForRocq : Type :=

  mk {
    
} .

Notation "'TriangularNumberForRocq_'" := TriangularNumberForRocq.mk (at level 99) .

Fixpoint   
  _tailrec_get_number (m : nat) (acc : nat) : nat :=
    match m with
      | S_ (k) => _tailrec_get_number (k) (acc .(add) (S_ (k) ) )
      | _otherwise => acc
    end
.

Definition   get_number (n : nat) : nat :=
    _tailrec_get_number (n) (O_ () )
.

End TriangularNumberForRocq .

Import TriangularNumberForRocq .
