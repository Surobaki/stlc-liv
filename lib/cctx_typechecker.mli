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
val uncMerge    : mergeFunction
val mixSeqMerge : mergeFunction
val mixBrMerge  : mergeFunction
val linCheck    : checkFunction
val uncCheck    : checkFunction

val unify : TypC.t -> livSubst list

val bobTypecheck : mergeFunction -> mergeFunction -> checkFunction ->
                   livTerm -> (livTyp * TypC.t)
val bobTypecheckSimple : mergeFunction -> mergeFunction -> checkFunction ->
                         livTerm -> livTyp
