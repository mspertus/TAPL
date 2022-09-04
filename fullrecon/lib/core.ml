open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

exception NoRuleApplies

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_) -> true
  | _ -> false

let rec eval1 ctx t = match t with
    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2' = eval1 ctx t2 in
      TmApp(fi, v1, t2')
  | TmApp(fi,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmApp(fi, t1', t2)
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3
  | TmIf(fi,t1,t2,t3) ->
      let t1' = eval1 ctx t1 in
      TmIf(fi, t1', t2, t3)
  | TmSucc(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmSucc(fi, t1')
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo)
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1
  | TmPred(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmPred(fi, t1')
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo)
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo)
  | TmIsZero(fi,t1) ->
      let t1' = eval1 ctx t1 in
      TmIsZero(fi, t1')
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2 
  | TmLet(fi,x,t1,t2) ->
      let t1' = eval1 ctx t1 in
      TmLet(fi, x, t1', t2) 
  | _ -> 
      raise NoRuleApplies

let rec eval ctx t =
  try let t' = eval1 ctx t
      in eval ctx t'
  with NoRuleApplies -> t

(* ------------------------   TYPING  ------------------------ *)

type constr = (ty * ty) list

let emptyconstr = []
let combineconstr = List.append

let prconstr constr =
  let pc (tyS,tyT) =
    printty_Type false tyS; pr "="; printty_Type false tyT in
  let rec f l = match l with
      [] -> ()
    | [c] -> pc c
    | c::rest -> (pc c; pr ", "; f rest)
  in 
    pr "{"; f constr; pr "}"

type nextuvar = NextUVar of string * uvargenerator
and uvargenerator = unit -> nextuvar

let uvargen =
  let rec f n () = NextUVar("?X" ^ string_of_int n, f (n+1))
  in f 0

let rec recon ctx nextuvar t =
  match t with
      TmVar(fi,i,_) -> 
        let tyT = getTypeFromContext fi ctx i in
        (tyT, nextuvar, [])
    | TmAbs(fi, x, Some(tyT1), t2) ->
        let ctx' = addbinding ctx x (VarBind(tyT1)) in
        let (tyT2,nextuvar2,constr2) = recon ctx' nextuvar t2 in
        (TyArr(tyT1, tyT2), nextuvar2, constr2)
    | TmAbs(fi, x, None, t2) ->
        let NextUVar(u,nextuvar0) = nextuvar() in
        let tyX = TyId(u) in
        let ctx' = addbinding ctx x (VarBind(tyX)) in
        let (tyT2,nextuvar2,constr2) = recon ctx' nextuvar0 t2 in
        (TyArr(tyX, tyT2), nextuvar2, constr2)
    | TmApp(fi,t1,t2) ->
        let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
        let (tyT2,nextuvar2,constr2) = recon ctx nextuvar1 t2 in
        let NextUVar(tyX,nextuvar') = nextuvar2() in
        let newconstr = [(tyT1,TyArr(tyT2,TyId(tyX)))] in
        ((TyId(tyX)), nextuvar', List.concat [newconstr; constr1; constr2])
    | TmLet(fi, x, t1, t2) ->
        if not (isval ctx t1) then 
          let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
          let ctx1 = addbinding ctx x (VarBind(tyT1)) in         
          let (tyT2,nextuvar2,constr2) = recon ctx1 nextuvar1 t2 in
          (tyT2, nextuvar2, constr1@constr2)
        else
          recon ctx nextuvar (termSubstTop t1 t2)
    | TmZero(fi) -> (TyNat, nextuvar, [])
    | TmSucc(fi,t1) ->
        let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
        (TyNat, nextuvar1, (tyT1,TyNat)::constr1)
    | TmPred(fi,t1) ->
        let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
        (TyNat, nextuvar1, (tyT1,TyNat)::constr1)
    | TmIsZero(fi,t1) ->
        let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
        (TyBool, nextuvar1, (tyT1,TyNat)::constr1) 
    | TmTrue(fi) -> (TyBool, nextuvar, [])
    | TmFalse(fi) -> (TyBool, nextuvar, [])
    | TmIf(fi,t1,t2,t3) ->
        let (tyT1,nextuvar1,constr1) = recon ctx nextuvar t1 in
        let (tyT2,nextuvar2,constr2) = recon ctx nextuvar1 t2 in
        let (tyT3,nextuvar3,constr3) = recon ctx nextuvar2 t3 in
        let newconstr = [(tyT1,TyBool); (tyT2,tyT3)] in
        (tyT3, nextuvar3, List.concat [newconstr; constr1; constr2; constr3])

let substinty tyX tyT tyS =
  let rec f tyS = match tyS with
      TyArr(tyS1,tyS2) -> TyArr(f tyS1, f tyS2)
    | TyNat -> TyNat
    | TyBool -> TyBool 
    | TyId(s) -> if s=tyX then tyT else TyId(s)
  in f tyS

let applysubst constr tyT =
  List.fold_left
    (fun tyS (TyId(tyX),tyC2) -> substinty tyX tyC2 tyS)
    tyT (List.rev constr)

let substinconstr tyX tyT constr =
  List.map
    (fun (tyS1,tyS2) ->
       (substinty tyX tyT tyS1, substinty tyX tyT tyS2))
    constr

let occursin tyX tyT =
  let rec o tyT = match tyT with
      TyArr(tyT1,tyT2) -> o tyT1 || o tyT2
    | TyNat -> false
    | TyBool -> false
    | TyId(s) -> (s=tyX)
  in o tyT

let unify fi ctx msg constr =
  let rec u constr = match constr with
      [] -> []
    | (tyS,TyId(tyX)) :: rest ->
        if tyS = TyId(tyX) then u rest
        else if occursin tyX tyS then
          error fi (msg ^ ": circular constraints")
        else
          List.append (u (substinconstr tyX tyS rest)) 
                      [(TyId(tyX),tyS)]
    | (TyId(tyX),tyT) :: rest ->
        if tyT = TyId(tyX) then u rest
        else if occursin tyX tyT then
          error fi (msg ^ ": circular constraints")
        else
          List.append (u (substinconstr tyX tyT rest))
                      [(TyId(tyX),tyT)]
    | (TyNat,TyNat) :: rest -> u rest
    | (TyBool,TyBool) :: rest -> u rest
    | (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) :: rest ->
        u ((tyS1,tyT1) :: (tyS2,tyT2) :: rest)
    | (tyS,tyT)::rest -> 
        error fi "Unsolvable constraints"
  in
    u constr
