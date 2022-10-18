(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open Fullfomsub.Support.Pervasive
open Fullfomsub.Support.Error
open Fullfomsub.Syntax
open Fullfomsub.Core

module Lexer = Fullfomsub.Lexer
module Parser = Fullfomsub.Parser

let searchpath = ref [""]

let argDefs = [
  "-I",
      Arg.String (fun f -> searchpath := f::!searchpath),
      "Append a directory to the search path"]

let parseArgs () =
  let inFile = ref (None : string option) in
  Arg.parse argDefs
     (fun s ->
       match !inFile with
         Some(_) -> err "You must specify exactly one input file"
       | None -> inFile := Some(s))
     "";
  match !inFile with
      None -> err "You must specify an input file"
    | Some(s) -> s

let openfile infile = 
  let rec trynext l = match l with
        [] -> err ("Could not find " ^ infile)
      | (d::rest) -> 
          let name = if d = "" then infile else (d ^ "/" ^ infile) in
          try open_in name
            with Sys_error m -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let checkbinding fi ctx b = match b with
    NameBind -> NameBind
  | TyVarBind(tyS) ->
  (*  kindof ctx tyS; Removed to compile. Guessing this was a remnant of old diagnostic code *)
      TyVarBind(tyS)
  | TyAbbBind(tyT,None) -> TyAbbBind(tyT,Some(kindof ctx tyT))
  | TyAbbBind(tyT,Some(knK)) ->
      let knK' = kindof ctx tyT in
      if (=) knK knK' then TyAbbBind(tyT,Some(knK))
      else error fi "Kind of binding does not match declared kind"
  | VarBind(tyT) -> VarBind(tyT)
  | TmAbbBind(t,None) -> TmAbbBind(t, Some(typeof ctx t))
  | TmAbbBind(t,Some(tyT)) ->
     let tyT' = typeof ctx t in
     if subtype ctx tyT' tyT then TmAbbBind(t,Some(tyT))
     else error fi "Type of binding does not match declared type"

let prbindingty ctx b = match b with
    NameBind -> ()
  | VarBind(tyT) -> pr ": "; printty ctx tyT 
  | TyVarBind(tyS) -> pr "<: ";printty ctx tyS
  | TyAbbBind(tyT,knK_opt) -> pr ":: ";
      (match knK_opt with
          None -> printkn ctx (kindof ctx tyT)
        | Some(knK) -> printkn ctx knK)
  | TmAbbBind(t, tyT_opt) -> pr ": ";
     (match tyT_opt with
         None -> printty ctx (typeof ctx t)
       | Some(tyT) -> printty ctx tyT)

let rec process_file f ctx =
  if List.mem f (!alreadyImported) then
    ctx
  else (
    alreadyImported := f :: !alreadyImported;
    let cmds,_ = parseFile f ctx in
    let g ctx c =  
      open_hvbox 0;
      let results = process_command ctx c in
      print_flush();
      results
    in
      List.fold_left g ctx cmds)

and process_command ctx cmd = match cmd with
    Import(f) -> 
      process_file f ctx
  | Eval(fi,t) -> 
      let tyT = typeof ctx t in
      let t' = eval ctx t in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      printty ctx tyT;
      force_newline();
      ctx
  | Bind(fi,x,bind) -> 
      let bind = checkbinding fi ctx bind in
      let bind' = evalbinding ctx bind in
      pr x; pr " "; prbindingty ctx bind'; force_newline();
      addbinding ctx x bind'
  | SomeBind(fi,tyX,x,t) ->
     let tyT = typeof ctx t in
     (match lcst ctx tyT with
        TySome(_,tyBound,tyBody) ->
          let t' = eval ctx t in
          let b = match t' with
                    TmPack(_,_,t12,_) -> (TmAbbBind(termShift 1 t12,Some(tyBody)))
                  | _ -> VarBind(tyBody) in
          let ctx1 = addbinding ctx tyX (TyVarBind tyBound) in
          let ctx2 = addbinding ctx1 x b in
          
          pr tyX; force_newline();
          pr x; pr " : "; printty ctx1 tyBody; force_newline();
          ctx2
      | _ -> error fi "existential type expected")
  
let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile emptycontext in
  ()

let () = set_max_boxes 1000
let () = set_margin 67
let res = 
  Printexc.catch (fun () -> 
    try main();0 
    with Exit x -> x) 
  ()
let () = print_flush()
let () = exit res
