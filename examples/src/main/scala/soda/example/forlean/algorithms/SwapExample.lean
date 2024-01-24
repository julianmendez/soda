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
    PairExample_ (pair.right) (pair.left)


  theorem
    swap_of_swap (x : Int) (y : Int) : (swap (swap (PairExample_ (x) (y) ) ) ) = PairExample_ (x) (y) :=
      by
        constructor

end SwapExample

notation "SwapExample_" => SwapExample.mk
