open Stlc

type cctxOut = (livTyp * livTyp TypR.t * TypC.t)
type livSubst = (livTyp * livTyp) list

val ccTypecheck : livTerm -> cctxOut

val unpackTyp : cctxOut -> livTyp
val unpackReq : cctxOut -> livTyp TypR.t
val unpackCst : cctxOut -> TypC.t

val reqMerge : livTyp TypR.t -> livTyp TypR.t 
               -> (livTyp TypR.t * TypC.t)

val unify : TypC.t -> livSubst
val checkUnify : livTerm -> cctxOut * livSubst
