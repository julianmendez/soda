package soda.example.forcoq.mathematics

class TriangularNumberForCoq

  import
    soda.example.forcoq.lib.O_
    soda.example.forcoq.lib.S_
    soda.example.forcoq.lib.nat

Fixpoint   @tailrec
   _tailrec_get_number (m : nat) (acc : nat) : nat :=
    match m with
      case O_ () => acc
      case S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
    end
.

 Definition   get_number (n : nat) : nat :=
    _tailrec_get_number (n) ( O_ () )
.

end
