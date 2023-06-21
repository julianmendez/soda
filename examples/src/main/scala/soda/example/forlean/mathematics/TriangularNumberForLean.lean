namespace TriangularNumberForLean 

  import   soda.example.forlean.lib.O_ 
  import   soda.example.forlean.lib.S_ 
  import   soda.example.forlean.lib.Nat 

def   @tailrec
   _tailrec_get_number (m : Nat) (acc : Nat) : Nat :=
    match m with
      | S_ (k) => _tailrec_get_number (k) (acc .add ( S_ (k) ) )
      | otherwise => acc
    


 def   get_number (n : Nat) : Nat :=
    _tailrec_get_number (n) ( O_ () )


end TriangularNumberForLean 

import TriangularNumberForLean 
