open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isval ctx t = match t with
    TmAbs(_,_,_,_) -> true
  | TmTAbs(_,_,_,_) -> true
  | _ -> false

exception NoRuleApplies

let rec eval1 ctx t = match t with
    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmTApp(fi,TmTAbs(_,x,_,t11),tyT2) ->
      tytermSubstTop tyT2 t11
  | TmTApp(fi,t1,tyT2) ->
      let t1' = eval1 ctx t1 in
      TmTApp(fi, t1', tyT2)
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   SUBTYPING  ------------------------ *)

let promote ctx t = match t with
   TyVar(i,_) ->
     (match getbinding dummyinfo ctx i with
         TyVarBind(tyT) -> tyT
       | _ -> raise NoRuleApplies)
 | _ -> raise NoRuleApplies

let rec subtype ctx tyS tyT =
   (=) tyS tyT ||
   match (tyS,tyT) with
     (_,TyTop) -> 
       true
   | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (subtype ctx tyT1 tyS1) && (subtype ctx tyS2 tyT2)
   | (TyVar(_,_),_) -> subtype ctx (promote ctx tyS) tyT
   | (TyAll(tyX1,tyS1,tyS2),TyAll(_,tyT1,tyT2)) ->
        (subtype ctx tyS1 tyT1 && subtype ctx tyT1 tyS1) &&
        let ctx1 = addbinding ctx tyX1 (TyVarBind(tyT1)) in
        subtype ctx1 tyS2 tyT2
   | (_,_) -> 
       false

(* ------------------------   TYPING  ------------------------ *)

let rec lcst ctx tyS =
  try lcst ctx (promote ctx tyS)
  with NoRuleApplies -> tyS

let rec typeof ctx t =
  match t with
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match lcst ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if subtype ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmTAbs(fi,tyX,tyT1,t2) ->
      let ctx = addbinding ctx tyX (TyVarBind(tyT1)) in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX,tyT1,tyT2)
  | TmTApp(fi,t1,tyT2) ->
      let tyT1 = typeof ctx t1 in
      (match lcst ctx tyT1 with
           TyAll(_,tyT11,tyT12) ->
             if not(subtype ctx tyT2 tyT11) then
                  error fi "type parameter type mismatch";
             typeSubstTop tyT2 tyT12
         | _ -> error fi "universal type expected")
