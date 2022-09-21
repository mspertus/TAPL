(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type kind = 
    KnStar
  | KnArr of kind * kind

type ty =
    TyVar of int * int
  | TyArr of ty * ty
  | TyAbs of string * kind * ty
  | TyApp of ty * ty
  | TyAll of string * kind * ty
  | TyRecord of (string * ty) list
  | TyRef of ty
  | TyString
  | TyUnit
  | TySome of string * kind * ty
  | TyBool
  | TyNat
  | TyId of string
  | TyFloat

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmAscribe of info * term * ty
  | TmTAbs of info * string * kind * term
  | TmTApp of info * term * ty
  | TmString of info * string
  | TmUnit of info
  | TmLoc of info * int
  | TmRef of info * term
  | TmDeref of info * term 
  | TmAssign of info * term * term
  | TmRecord of info * (string * term) list
  | TmProj of info * term * string
  | TmLet of info * string * term * term
  | TmFix of info * term
  | TmPack of info * ty * term * ty
  | TmUnpack of info * string * string * term * term
  | TmFloat of info * float
  | TmTimesfloat of info * term * term
  | TmTrue of info
  | TmFalse of info
  | TmIf of info * term * term * term
  | TmZero of info
  | TmSucc of info * term
  | TmPred of info * term
  | TmIsZero of info * term
  | TmInert of info * ty

type binding =
    NameBind 
  | VarBind of ty
  | TyVarBind of kind
  | TyAbbBind of ty * (kind option)
  | TmAbbBind of term * (ty option)

type command =
    Import of string
  | Eval of info * term
  | Bind of info * string * binding
  | SomeBind of info * string * string * term

(* Contexts *)
type context
val emptycontext : context 
val ctxlength : context -> int
val addbinding : context -> string -> binding -> context
val addname: context -> string -> context
val index2name : info -> context -> int -> string
val getbinding : info -> context -> int -> binding
val name2index : info -> context -> string -> int
val isnamebound : context -> string -> bool
val getTypeFromContext : info -> context -> int -> ty

(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term
val typeShift : int -> ty -> ty
val typeSubstTop: ty -> ty -> ty
val tytermSubstTop: ty -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val printty : context -> ty -> unit
val printkn : context -> kind -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

