namespace TriangularNumberForLean

def   @tailrec
   _tailrec_get_number (m : Nat) (acc : Nat) : Nat :=
    match m with
      | Succ_ (k) => _tailrec_get_number (k) (acc.add (Succ_ (k) ) )
      | otherwise => acc
    


 def   get_number (n : Nat) : Nat :=
    _tailrec_get_number (n) (Zero_ () )


end TriangularNumberForLean

open TriangularNumberForLean
