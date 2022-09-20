(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open String
open Fullrecon.Support.Pervasive
open Fullrecon.Support.Error
open Fullrecon.Syntax
open Fullrecon.Core

module Lexer = Fullrecon.Lexer
module Parser = Fullrecon.Parser

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

let rec read_til_semi ?(prompt = " > ") () = 
  print_string prompt;
  print_flush();
  let line = read_line() in
    if ends_with ~suffix:";" line then
      line
    else
      line ^ (read_til_semi ~prompt: "+> " ())

let parseString str =
  let lexbuf = Lexer.createFromStr str
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
      print_endline "Parse Error"; print_flush(); fun ctx -> ([], ctx)
in
  Parsing.clear_parser(); result


let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

let alreadyImported = ref ([] : string list)

let prbindingty ctx b = match b with
    NameBind -> ()
  | VarBind(tyT) -> pr ": "; printty tyT 

let rec process_file f (ctx,nextuvar,constr) =
  if (f = "repl") then
    try (
    let text = read_til_semi() in
    let cmds,_ = parseString text ctx in
      process_file "repl" (List.fold_left process_cmds (ctx,nextuvar,constr) cmds))
      with End_of_file -> print_endline ""; (ctx,nextuvar,constr)
      | _ -> process_file "repl" (ctx, nextuvar, constr);
  else if List.mem f (!alreadyImported) then
    (ctx,nextuvar,constr)
  else (
    alreadyImported := f :: !alreadyImported;
    let cmds,_ = parseFile f ctx in
      List.fold_left process_cmds (ctx,nextuvar,constr) cmds)
    
    and process_cmds (ctx,nextuvar,constr) c =  
      open_hvbox 0;
      let results = process_command (ctx,nextuvar,constr) c in
      print_flush();
      close_box ();
      results

and process_command (ctx,nextuvar,constr) cmd = match cmd with
    Import(f) -> 
      process_file f (ctx,nextuvar,constr)
  | Eval(fi,t) -> 
      let (tyT,nextuvar',constr_t) = recon ctx nextuvar t in
      let t' = eval ctx t in
      let constr' = combineconstr constr constr_t in
      let constr'' =
        unify fi ctx "Could not simplify constraints" constr' in
      printtm_ATerm true ctx t'; 
      print_break 1 2;
      pr ": ";
      open_hovbox 0;
      printty (applysubst constr'' tyT);
      close_box();
      force_newline();
      (ctx, nextuvar', constr'')
  | Bind(fi,x,bind) -> 
       pr x; pr " "; prbinding ctx bind; force_newline();
      (addbinding ctx x bind, uvargen, constr)
  
let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile (emptycontext, uvargen, emptyconstr) in
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
