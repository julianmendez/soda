Module TriangularNumberForCoq .

Class TriangularNumberForCoq : Type :=

  mk {
    
} .

Notation "'TriangularNumberForCoq_'" := TriangularNumberForCoq.mk (at level 99) .

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

End TriangularNumberForCoq .

Import TriangularNumberForCoq .
