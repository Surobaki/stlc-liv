open Stlc

type cctxOut = livTyp * livTyp TypR.t * TypC.t
type typCtx = livTyp TypR.t
type livSubst = TyVar.t * livTyp
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = livBinder -> typCtx -> livTyp * TypC.t
type tcOut = livTyp * TypC.t
type linearityBase = B_Linear | B_Mixed | B_Unrestricted

val ccTc : mergeFunction -> mergeFunction -> checkFunction ->
           livTerm -> cctxOut

val unpackTyp : cctxOut -> livTyp
val unpackReq : cctxOut -> typCtx
val unpackCst : cctxOut -> TypC.t

val linSeqMerge : mergeFunction
val linBrMerge  : mergeFunction
val unrMerge    : mergeFunction
val mixSeqMerge : mergeFunction
val mixBrMerge  : mergeFunction
val linCheck    : checkFunction
val unrCheck    : checkFunction
val mixCheck    : checkFunction

val closeSubsts : livSubst list -> livTyp -> livTyp

val resolveConstraints : TypC.t -> livSubst list

val bobTypecheck : mergeFunction -> mergeFunction -> checkFunction ->
                   livTerm -> tcOut
val typecheck : linearityBase -> livTerm -> tcOut

val pp_tcOut : ?verbose:bool -> Format.formatter -> tcOut -> unit
