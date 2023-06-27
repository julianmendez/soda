notation:max "Zero_ ()" => Nat.zero

notation:max "Succ_" => Nat.succ

namespace TriangularNumberForLean

 def   _tailrec_get_number (m : Nat) (acc : Nat) : Nat :=
    match m with
      | Succ_ (k) => _tailrec_get_number (k) (acc.add (Succ_ (k) ) )
      | otherwise => acc
    


 def   get_number (n : Nat) : Nat :=
    _tailrec_get_number (n) (Zero_ () )


end TriangularNumberForLean

open TriangularNumberForLean
