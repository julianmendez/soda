class PairParam ( A : Type ) ( B : Type )

where
  PairParam_ ::
    fst : A
    snd : B
  deriving DecidableEq

namespace PairParam


end PairParam

open PairParam

class PairParamMod

where
  PairParamMod_ ::
    
  deriving DecidableEq

namespace PairParamMod


 def   get_first ( A : Type ) ( B : Type ) (self : PairParam ( A ) ( B ) ) : A :=
    self.fst


 def   get_second ( A : Type ) ( B : Type ) (self : PairParam ( A ) ( B ) ) : B :=
    self.snd


 def   swap ( A : Type ) ( B : Type ) (self : PairParam ( A ) ( B ) ) : PairParam ( B ) ( A ) :=
    PairParam_ (get_second ( A ) ( B ) (self) ) (get_first ( A ) ( B ) (self) )


end PairParamMod

open PairParamMod

class TripleIntStringInt
  
-- extends     PairParam ( Int ) ( String )

where
  TripleIntStringInt_ ::
    fst : Int
    snd : String
    trd : Int
  deriving DecidableEq

namespace TripleIntStringInt


end TripleIntStringInt

open TripleIntStringInt

class TripleIntStringIntMod

where
  TripleIntStringIntMod_ ::
    
  deriving DecidableEq

namespace TripleIntStringIntMod


 def   get_first (self : TripleIntStringInt) : Int :=
    self.fst


 def   get_second (self : TripleIntStringInt) : String :=
    self.snd


 def   get_third (self : TripleIntStringInt) : Int :=
    self.trd


 def   get_pair_param (self : TripleIntStringInt) : PairParam ( Int ) ( String ) :=
    PairParam_ (get_first (self) ) (get_second (self) )


end TripleIntStringIntMod

open TripleIntStringIntMod
