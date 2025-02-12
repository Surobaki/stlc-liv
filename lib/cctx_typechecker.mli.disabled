open Ast

type cctxOut = typ * typ TypR.t * TypC.t
type typCtx = typ TypR.t
type livSubst = TyVar.t * typ
type mergeFunction = typCtx -> typCtx -> typCtx * TypC.t
type checkFunction = binder -> typCtx -> typ * TypC.t
type tcOut = typ * TypC.t
type linearityBase = B_Linear | B_Mixed | B_Unrestricted

val ccTc : mergeFunction -> mergeFunction -> checkFunction ->
           term -> cctxOut

val unpackTyp : cctxOut -> typ
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

val closeSubsts : livSubst list -> typ -> typ

val resolveConstraints : TypC.t -> livSubst list

val bobTypecheck : mergeFunction -> mergeFunction -> checkFunction ->
                   term -> tcOut
val typecheck : linearityBase -> term -> tcOut

val pp_tcOut : ?verbose:bool -> Format.formatter -> tcOut -> unit
