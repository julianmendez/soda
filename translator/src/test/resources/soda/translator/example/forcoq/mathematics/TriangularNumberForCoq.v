Module TriangularNumberForCoq .

  Import   soda.example.forcoq.lib.O_ .
  Import   soda.example.forcoq.lib.S_ .
  Import   soda.example.forcoq.lib.nat .

Fixpoint   @tailrec
   _tailrec_get_number (m : nat) (acc : nat) : nat :=
    match m with
      | S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
      | otherwise => acc
    end
.

 Definition   get_number (n : nat) : nat :=
    _tailrec_get_number (n) ( O_ () )
.

End TriangularNumberForCoq .

Import TriangularNumberForCoq .
