class PairExample

where
  mk ::
    left : Int
    right : Int
  deriving DecidableEq

namespace PairExample


end PairExample

notation "PairExample_" => PairExample.mk

class SwapExample

where
  mk ::
    
  deriving DecidableEq

namespace SwapExample


 def   swap (pair : PairExample) : PairExample :=
    match pair with
      | PairExample_ (a) (b) =>
        PairExample.mk (b) (a)
    


  theorem
    swap_of_swap (pair : PairExample)
      : (swap (swap (pair) ) ) = pair := by
    rewrite [swap, swap]
    simp

end SwapExample

notation "SwapExample_" => SwapExample.mk
