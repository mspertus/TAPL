(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val typeof : context -> term -> ty
val tyeqv : context -> ty -> ty -> bool
val kindof : context -> ty -> kind
val simplifyty : context -> ty -> ty
type store
val emptystore : store
val shiftstore : int -> store -> store 
val eval : context -> store -> term -> term * store
val evalbinding : context -> store -> binding -> binding * store
