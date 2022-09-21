open Format
open Syntax
open Support.Error
open Support.Pervasive

(* ------------------------   EVALUATION  ------------------------ *)

let rec isnumericval ctx t = match t with
    TmZero(_) -> true
  | TmSucc(_,t1) -> isnumericval ctx t1
  | _ -> false

let rec isval ctx t = match t with
    TmString _  -> true
  | TmUnit(_)  -> true
  | TmLoc(_,_) -> true
  | TmPack(_,_,v1,_) when isval ctx v1 -> true
  | TmTAbs(_,_,_,_) -> true
  | TmFloat _  -> true
  | TmTrue(_)  -> true
  | TmFalse(_) -> true
  | t when isnumericval ctx t  -> true
  | TmAbs(_,_,_,_) -> true
  | TmRecord(_,fields) -> List.for_all (fun (l,ti) -> isval ctx ti) fields
  | _ -> false

type store = term list  
let emptystore = []
let extendstore store v = (List.length store, List.append store [v])
let lookuploc store l = List.nth store l
let updatestore store n v =
  let rec f s = match s with 
      (0, v'::rest) -> v::rest
    | (n, v'::rest) -> v' :: (f (n-1,rest))
    | _ -> error dummyinfo "updatestore: bad index"
  in
    f (n,store)
let shiftstore i store = List.map (fun t -> termShift i t) store 

exception NoRuleApplies

let rec eval1 ctx store t = match t with
    TmApp(fi,TmAbs(_,x,tyT11,t12),v2) when isval ctx v2 ->
      termSubstTop v2 t12, store
  | TmApp(fi,v1,t2) when isval ctx v1 ->
      let t2',store' = eval1 ctx store t2 in
      TmApp(fi, v1, t2'), store'
  | TmApp(fi,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmApp(fi, t1', t2), store'
  | TmAscribe(fi,v1,tyT) when isval ctx v1 ->
      v1, store
  | TmAscribe(fi,t1,tyT) ->
      let t1',store' = eval1 ctx store t1 in
      TmAscribe(fi,t1',tyT), store'
  | TmTApp(fi,TmTAbs(_,x,_,t11),tyT2) ->
      tytermSubstTop tyT2 t11, store
  | TmTApp(fi,t1,tyT2) ->
      let t1',store' = eval1 ctx store t1 in
      TmTApp(fi, t1', tyT2), store'
  | TmRef(fi,t1) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmRef(fi,t1'), store')
      else
        let (l,store') = extendstore store t1 in
        (TmLoc(dummyinfo,l), store')
  | TmDeref(fi,t1) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmDeref(fi,t1'), store')
      else (match t1 with
            TmLoc(_,l) -> (lookuploc store l, store)
          | _ -> raise NoRuleApplies)
  | TmAssign(fi,t1,t2) ->
      if not (isval ctx t1) then
        let (t1',store') = eval1 ctx store t1
        in (TmAssign(fi,t1',t2), store')
      else if not (isval ctx t2) then
        let (t2',store') = eval1 ctx store t2
        in (TmAssign(fi,t1,t2'), store')
      else (match t1 with
            TmLoc(_,l) -> (TmUnit(dummyinfo), updatestore store l t2)
          | _ -> raise NoRuleApplies)
  | TmRecord(fi,fields) ->
      let rec evalafield l = match l with 
        [] -> raise NoRuleApplies
      | (l,vi)::rest when isval ctx vi -> 
          let rest',store' = evalafield rest in
          (l,vi)::rest', store'
      | (l,ti)::rest -> 
          let ti',store' = eval1 ctx store ti in
          (l, ti')::rest, store'
      in let fields',store' = evalafield fields in
      TmRecord(fi, fields'), store'
  | TmProj(fi, TmRecord(_, fields), l) ->
      (try List.assoc l fields, store
       with Not_found -> raise NoRuleApplies)
  | TmProj(fi, t1, l) ->
      let t1',store' = eval1 ctx store t1 in
      TmProj(fi, t1', l), store'
  | TmLet(fi,x,v1,t2) when isval ctx v1 ->
      termSubstTop v1 t2, store 
  | TmLet(fi,x,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmLet(fi, x, t1', t2), store' 
  | TmFix(fi,v1) as t when isval ctx v1 ->
      (match v1 with
         TmAbs(_,_,_,t12) -> termSubstTop t t12, store
       | _ -> raise NoRuleApplies)
  | TmFix(fi,t1) ->
      let t1',store' = eval1 ctx store t1
      in TmFix(fi,t1'), store'
  | TmVar(fi,n,_) ->
      (match getbinding fi ctx n with
          TmAbbBind(t,_) -> t,store 
        | _ -> raise NoRuleApplies)
  | TmUnpack(fi,_,_,TmPack(_,tyT11,v12,_),t2) when isval ctx v12 ->
      tytermSubstTop tyT11 (termSubstTop (termShift 1 v12) t2), store
  | TmUnpack(fi,tyX,x,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmUnpack(fi,tyX,x,t1',t2), store'
  | TmPack(fi,tyT1,t2,tyT3) ->
      let t2',store' = eval1 ctx store t2 in
      TmPack(fi,tyT1,t2',tyT3), store'
  | TmTimesfloat(fi,TmFloat(_,f1),TmFloat(_,f2)) ->
      TmFloat(fi, f1 *. f2), store
  | TmTimesfloat(fi,(TmFloat(_,f1) as t1),t2) ->
      let t2',store' = eval1 ctx store t2 in
      TmTimesfloat(fi,t1,t2') , store'
  | TmTimesfloat(fi,t1,t2) ->
      let t1',store' = eval1 ctx store t1 in
      TmTimesfloat(fi,t1',t2) , store'
  | TmIf(_,TmTrue(_),t2,t3) ->
      t2, store
  | TmIf(_,TmFalse(_),t2,t3) ->
      t3, store
  | TmIf(fi,t1,t2,t3) ->
      let t1',store' = eval1 ctx store t1 in
      TmIf(fi, t1', t2, t3), store'
  | TmSucc(fi,t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmSucc(fi, t1'), store'
  | TmPred(_,TmZero(_)) ->
      TmZero(dummyinfo), store
  | TmPred(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      nv1, store
  | TmPred(fi,t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmPred(fi, t1'), store'
  | TmIsZero(_,TmZero(_)) ->
      TmTrue(dummyinfo), store
  | TmIsZero(_,TmSucc(_,nv1)) when (isnumericval ctx nv1) ->
      TmFalse(dummyinfo), store
  | TmIsZero(fi,t1) ->
      let t1',store' = eval1 ctx store t1 in
      TmIsZero(fi, t1'), store'
  | _ -> 
      raise NoRuleApplies

let rec eval ctx store t =
  try let t',store' = eval1 ctx store t
      in eval ctx store' t'
  with NoRuleApplies -> t,store

(* ------------------------   KINDING  ------------------------ *)

let istyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT,_) -> true
  | _ -> false

let gettyabb ctx i = 
  match getbinding dummyinfo ctx i with
    TyAbbBind(tyT,_) -> tyT
  | _ -> raise NoRuleApplies

let rec computety ctx tyT = match tyT with
    TyApp(TyAbs(_,_,tyT12),tyT2) -> typeSubstTop tyT2 tyT12
  | TyVar(i,_) when istyabb ctx i -> gettyabb ctx i
  | _ -> raise NoRuleApplies

let rec simplifyty ctx tyT =
  let tyT = 
    match tyT with
        TyApp(tyT1,tyT2) -> TyApp(simplifyty ctx tyT1,tyT2)
      | tyT -> tyT
  in 
  try
    let tyT' = computety ctx tyT in
    simplifyty ctx tyT' 
  with NoRuleApplies -> tyT

let rec tyeqv ctx tyS tyT =
  let tyS = simplifyty ctx tyS in
  let tyT = simplifyty ctx tyT in
  match (tyS,tyT) with
    (TyArr(tyS1,tyS2),TyArr(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TyAbs(tyX1,knKS1,tyS2),TyAbs(_,knKT1,tyT2)) ->
       ((=) knKS1 knKT1)
      &&
       (let ctx = addname ctx tyX1 in
        tyeqv ctx tyS2 tyT2)
  | (TyApp(tyS1,tyS2),TyApp(tyT1,tyT2)) ->
       (tyeqv ctx tyS1 tyT1) && (tyeqv ctx tyS2 tyT2)
  | (TySome(tyX1,knK1,tyS2),TySome(_,knK1',tyT2)) ->
       (=) knK1 knK1' &&
       let ctx1 = addname ctx tyX1 in
       tyeqv ctx1 tyS2 tyT2
  | (TyBool,TyBool) -> true
  | (TyNat,TyNat) -> true
  | (TyRecord(fields1),TyRecord(fields2)) -> 
       List.length fields1 = List.length fields2
       &&                                         
       List.for_all 
         (fun (li2,tyTi2) ->
            try let (tyTi1) = List.assoc li2 fields1 in
                tyeqv ctx tyTi1 tyTi2
            with Not_found -> false)
         fields2
  | (TyId(b1),TyId(b2)) -> b1=b2
  | (TyVar(i,_), _) when istyabb ctx i ->
      tyeqv ctx (gettyabb ctx i) tyT
  | (_, TyVar(i,_)) when istyabb ctx i ->
      tyeqv ctx tyS (gettyabb ctx i)
  | (TyVar(i,_),TyVar(j,_)) -> i=j
  | (TyAll(tyX1,knK1,tyS2),TyAll(_,knK2,tyT2)) ->
       let ctx1 = addname ctx tyX1 in
       (=) knK1 knK2 && tyeqv ctx1 tyS2 tyT2
  | (TyString,TyString) -> true
  | (TyFloat,TyFloat) -> true
  | (TyUnit,TyUnit) -> true
  | (TyRef(tyT1),TyRef(tyT2)) -> tyeqv ctx tyT1 tyT2
  | _ -> false

let getkind fi ctx i =
  match getbinding fi ctx i with
      TyVarBind(knK) -> knK
    | TyAbbBind(_,Some(knK)) -> knK
    | TyAbbBind(_,None) -> error fi ("No kind recorded for variable "
                                     ^ (index2name fi ctx i))
    | _ -> error fi ("getkind: Wrong kind of binding for variable " 
                     ^ (index2name fi ctx i))

let rec kindof ctx tyT = match tyT with
    TyArr(tyT1,tyT2) ->
      if kindof ctx tyT1 <> KnStar then error dummyinfo "star kind expected";
      if kindof ctx tyT2 <> KnStar then error dummyinfo "star kind expected";
      KnStar
  | TyVar(i,_) ->
      let knK = getkind dummyinfo ctx i
      in knK
  | TyAbs(tyX,knK1,tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind(knK1)) in
      let knK2 = kindof ctx' tyT2 in
      KnArr(knK1,knK2)
  | TyApp(tyT1,tyT2) ->
      let knK1 = kindof ctx tyT1 in
      let knK2 = kindof ctx tyT2 in
      (match knK1 with
          KnArr(knK11,knK12) ->
            if (=) knK2 knK11 then knK12
            else error dummyinfo "parameter kind mismatch"
        | _ -> error dummyinfo "arrow kind expected")
  | TyAll(tyX,knK1,tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind knK1) in
      if kindof ctx' tyT2 <> KnStar then error dummyinfo "Kind * expected";
      KnStar
  | TyRecord(fldtys) ->
      List.iter (fun (l,tyS) ->
             if kindof ctx tyS<>KnStar then error dummyinfo "Kind * expected")
        fldtys;
      KnStar
  | TySome(tyX,knK,tyT2) ->
      let ctx' = addbinding ctx tyX (TyVarBind(knK)) in
      if kindof ctx' tyT2 <> KnStar then error dummyinfo "Kind * expected";
      KnStar
  | _ -> KnStar

let checkkindstar fi ctx tyT = 
  let k = kindof ctx tyT in
  if k = KnStar then ()
  else error fi "Kind * expected"

(* ------------------------   TYPING  ------------------------ *)

let rec typeof ctx t =
  match t with
    TmVar(fi,i,_) -> getTypeFromContext fi ctx i
  | TmAbs(fi,x,tyT1,t2) ->
      checkkindstar fi ctx tyT1;
      let ctx' = addbinding ctx x (VarBind(tyT1)) in
      let tyT2 = typeof ctx' t2 in
      TyArr(tyT1, typeShift (-1) tyT2)
  | TmApp(fi,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match simplifyty ctx tyT1 with
          TyArr(tyT11,tyT12) ->
            if tyeqv ctx tyT2 tyT11 then tyT12
            else error fi "parameter type mismatch"
        | _ -> error fi "arrow type expected")
  | TmAscribe(fi,t1,tyT) ->
     checkkindstar fi ctx tyT;
     if tyeqv ctx (typeof ctx t1) tyT then
       tyT
     else
       error fi "body of as-term does not have the expected type"
  | TmTAbs(fi,tyX,knK1,t2) ->
      let ctx = addbinding ctx tyX (TyVarBind(knK1)) in
      let tyT2 = typeof ctx t2 in
      TyAll(tyX,knK1,tyT2)
  | TmTApp(fi,t1,tyT2) ->
      let knKT2 = kindof ctx tyT2 in
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyAll(_,knK11,tyT12) ->
             if knK11 <> knKT2 then
               error fi "Type argument has wrong kind";
             typeSubstTop tyT2 tyT12
         | _ -> error fi "universal type expected")
  | TmString _ -> TyString
  | TmUnit(fi) -> TyUnit
  | TmRef(fi,t1) ->
      TyRef(typeof ctx t1)
  | TmLoc(fi,l) ->
      error fi "locations are not supposed to occur in source programs!"
  | TmDeref(fi,t1) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRef(tyT1) -> tyT1
        | _ -> error fi "argument of ! is not a Ref")
  | TmAssign(fi,t1,t2) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRef(tyT1) ->
            if tyeqv ctx (typeof ctx t2) tyT1 then
              TyUnit
            else
              error fi "arguments of := are incompatible"
        | _ -> error fi "argument of ! is not a Ref")
  | TmRecord(fi, fields) ->
      let fieldtys = 
        List.map (fun (li,ti) -> (li, typeof ctx ti)) fields in
      TyRecord(fieldtys)
  | TmProj(fi, t1, l) ->
      (match simplifyty ctx (typeof ctx t1) with
          TyRecord(fieldtys) ->
            (try List.assoc l fieldtys
             with Not_found -> error fi ("label "^l^" not found"))
        | _ -> error fi "Expected record type")
  | TmLet(fi,x,t1,t2) ->
     let tyT1 = typeof ctx t1 in
     let ctx' = addbinding ctx x (VarBind(tyT1)) in         
     typeShift (-1) (typeof ctx' t2)
  | TmFix(fi, t1) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
           TyArr(tyT11,tyT12) ->
             if tyeqv ctx tyT12 tyT11 then tyT12
             else error fi "result of body not compatible with domain"
         | _ -> error fi "arrow type expected")
  | TmPack(fi,tyT1,t2,tyT) ->
      checkkindstar fi ctx tyT;
      (match simplifyty ctx tyT with
          TySome(tyY,k,tyT2) ->
            if kindof ctx tyT1 <> k then
              error fi "type component does not have expected kind";
            let tyU = typeof ctx t2 in
            let tyU' = typeSubstTop tyT1 tyT2 in
            if tyeqv ctx tyU tyU' then tyT
            else error fi "doesn't match declared type"
        | _ -> error fi "existential type expected")
  | TmUnpack(fi,tyX,x,t1,t2) ->
      let tyT1 = typeof ctx t1 in
      (match simplifyty ctx tyT1 with
          TySome(tyY,k,tyT11) ->
            let ctx' = addbinding ctx tyX (TyVarBind k) in
            let ctx'' = addbinding ctx' x (VarBind tyT11) in
            let tyT2 = typeof ctx'' t2 in
            typeShift (-2) tyT2
        | _ -> error fi "existential type expected")
  | TmTrue(fi) -> 
      TyBool
  | TmFalse(fi) -> 
      TyBool
  | TmIf(fi,t1,t2,t3) ->
     if tyeqv ctx (typeof ctx t1) TyBool then
       let tyT2 = typeof ctx t2 in
       if tyeqv ctx tyT2 (typeof ctx t3) then tyT2
       else error fi "arms of conditional have different types"
     else error fi "guard of conditional not a boolean"
  | TmZero(fi) ->
      TyNat
  | TmSucc(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of succ is not a number"
  | TmPred(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyNat
      else error fi "argument of pred is not a number"
  | TmIsZero(fi,t1) ->
      if tyeqv ctx (typeof ctx t1) TyNat then TyBool
      else error fi "argument of iszero is not a number"
  | TmFloat _ -> TyFloat
  | TmTimesfloat(fi,t1,t2) ->
      if tyeqv ctx (typeof ctx t1) TyFloat
      && tyeqv ctx (typeof ctx t2) TyFloat then TyFloat
      else error fi "argument of timesfloat is not a number"
  | TmInert(fi,tyT) ->
      tyT

let evalbinding ctx store b = match b with
    TmAbbBind(t,tyT) ->
      let t',store' = eval ctx store t in 
      TmAbbBind(t',tyT), store'
  | bind -> bind,store
