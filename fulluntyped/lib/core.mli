(* module Core

   Core typechecking and evaluation functions
*)

open Syntax
open Support.Error

val eval : context -> term -> term 
val evalbinding : context -> binding -> binding 
