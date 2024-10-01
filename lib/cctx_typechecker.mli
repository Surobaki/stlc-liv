open Stlc

type cctxOut = (livTyp * livTyp TypR.t * TypC.t)
type typCtx = livTyp TypR.t
type livSubst = TyVar.t * livTyp
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = livBinder -> livTyp -> typCtx -> TypC.t

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
                   livTerm -> (livTyp * TypC.t)
val bobTypecheckSimple : livTerm -> livTyp
