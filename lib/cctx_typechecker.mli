open Stlc

type cctxOut = (livTyp * TypeRequirements.t * TypeConstraints.t)

val ccTypecheck : livTerm -> cctxOut

val unpackTyp : cctxOut -> livTyp
val unpackReq : cctxOut -> TypeRequirements.t
val unpackCst : cctxOut -> TypeConstraints.t

val reqMerge : TypeRequirements.t -> TypeRequirements.t 
               -> (TypeRequirements.t * TypeConstraints.t)
