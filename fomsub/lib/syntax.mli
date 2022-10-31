(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type kind = 
    KnStar
  | KnArr of kind * kind

type ty =
    TyVar of int * int
  | TyAbs of string * kind * ty
  | TyApp of ty * ty
  | TyTop
  | TyArr of ty * ty
  | TyAll of string * ty * ty

type term =
    TmVar of info * int * int
  | TmAbs of info * string * ty * term
  | TmApp of info * term * term
  | TmTAbs of info * string * ty * term
  | TmTApp of info * term * ty

type binding =
    NameBind 
  | VarBind of ty
  | TyVarBind of ty

type command =
    Import of string
  | Eval of info * term
  | Bind of info * string * binding

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
val maketop: kind -> ty
