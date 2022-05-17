(* module Syntax: syntax trees and associated support functions *)

open Support.Pervasive
open Support.Error

(* Data type definitions *)
type term =
    TmVar of info * int * int
  | TmAbs of info * string * term
  | TmApp of info * term * term

type binding =
    NameBind 

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


(* Shifting and substitution *)
val termShift: int -> term -> term
val termSubstTop: term -> term -> term

(* Printing *)
val printtm: context -> term -> unit
val printtm_ATerm: bool -> context -> term -> unit
val prbinding : context -> binding -> unit

(* Misc *)
val tmInfo: term -> info

