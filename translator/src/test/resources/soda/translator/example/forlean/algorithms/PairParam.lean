class PairParam ( A : Type ) ( B : Type )

where
  mk ::
    fst : A
    snd : B
  deriving DecidableEq

namespace PairParam


end PairParam

notation "PairParam_" => PairParam.mk

class PairParamMod

where
  mk ::
    
  deriving DecidableEq

namespace PairParamMod


 def   get_first ( A : Type ) ( B : Type ) (self : PairParam ( A ) ( B ) ) : A :=
    self.fst


 def   get_second ( A : Type ) ( B : Type ) (self : PairParam ( A ) ( B ) ) : B :=
    self.snd


 def   swap ( A : Type ) ( B : Type ) (self : PairParam ( A ) ( B ) ) : PairParam ( B ) ( A ) :=
    PairParam.mk (get_second ( A ) ( B ) (self) ) (get_first ( A ) ( B ) (self) )


end PairParamMod

notation "PairParamMod_" => PairParamMod.mk

class TripleIntStringInt
  
-- extends     PairParam ( Int ) ( String )

where
  mk ::
    fst : Int
    snd : String
    trd : Int
  deriving DecidableEq

namespace TripleIntStringInt


end TripleIntStringInt

notation "TripleIntStringInt_" => TripleIntStringInt.mk

class TripleIntStringIntMod

where
  mk ::
    
  deriving DecidableEq

namespace TripleIntStringIntMod


 def   get_first (self : TripleIntStringInt) : Int :=
    self.fst


 def   get_second (self : TripleIntStringInt) : String :=
    self.snd


 def   get_third (self : TripleIntStringInt) : Int :=
    self.trd


 def   get_pair_param (self : TripleIntStringInt) : PairParam ( Int ) ( String ) :=
    PairParam.mk (get_first (self) ) (get_second (self) )


end TripleIntStringIntMod

notation "TripleIntStringIntMod_" => TripleIntStringIntMod.mk
