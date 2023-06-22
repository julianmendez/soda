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

 def   left (pair : PairExample) : Int :=
    match pair with
      | (PairExample_ (x , y) ) => x
    


 def   right (pair : PairExample) : Int :=
    match pair with
      | (PairExample_ (x , y) ) => y
    


 def   swap (pair : PairExample) : PairExample :=
    PairExample_ (right (pair) , left (pair) )


theorem    swap_of_swap : forall (pair : PairExample) , (swap (swap (pair) ) ) = pair



    intros p.
    destruct p.
    compute.
    destruct x.
    apply eq_refl.
end

end SwapExample

open SwapExample
