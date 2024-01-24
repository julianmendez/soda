notation "Zero_ ()" => Nat.zero
notation "Succ_" => Nat.succ

class TriangularNumberForLean

where
  mk ::
    
  deriving DecidableEq

namespace TriangularNumberForLean


 private def   _tailrec_get_number (m : Nat) (acc : Nat) : Nat :=
    match m with
      | Succ_ (k) => _tailrec_get_number (k) (acc.add (Succ_ (k) ) )
      | otherwise => acc
    


 def   get_number (n : Nat) : Nat :=
    _tailrec_get_number (n) (Zero_ () )


end TriangularNumberForLean

notation "TriangularNumberForLean_" => TriangularNumberForLean.mk
