namespace PairExample

class PairExample where
  PairExample_ ::
    left : Int
    right : Int
  deriving DecidableEq

namespace PairExample


end PairExample

open PairExample

namespace SwapExample

 def   swap (pair : PairExample) : PairExample :=
    PairExample_ (pair.right) (pair.left)


  theorem
    swap_of_swap (x : Int) (y : Int) : (swap (swap (PairExample_ (x) (y) ) ) ) = PairExample_ (x) (y) :=
      by
        constructor

end SwapExample

open SwapExample
