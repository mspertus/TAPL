(* Module Main: The main program.  Deals with processing the command
   line, reading files, building and connecting lexers and parsers, etc. 
   
   For most experiments with the implementation, it should not be
   necessary to change this file.
*)

open Format
open String
open Arith.Support.Error
open Arith.Syntax
open Arith.Core

module Parser = Arith.Parser
module Lexer = Arith.Lexer

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
            with Sys_error _ -> trynext rest
  in trynext !searchpath

let parseFile inFile =
  let pi = openfile inFile
  in let lexbuf = Lexer.create inFile pi
  in let result =
    try Parser.toplevel Lexer.main lexbuf with Parsing.Parse_error -> 
    error (Lexer.info lexbuf) "Parse error"
in
  Parsing.clear_parser(); close_in pi; result

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
      print_endline "Parse Error"; print_flush(); []
in
  Parsing.clear_parser(); result
 
let alreadyImported = ref ([] : string list)

let rec process_file f  =
  if (f = "repl") then
    try (
    let text = read_til_semi() in
    let cmds = parseString text in
      List.iter process_cmds cmds;
      process_file "repl";
      () ) with End_of_file -> print_endline ""; ()   
  else if List.mem f (!alreadyImported) then
    ()
  else (
    alreadyImported := f :: !alreadyImported;
    let cmds = parseFile f in
      List.iter process_cmds  cmds)


and process_cmds  c =  
  open_hvbox 0;
  process_command  c;
  print_flush();

and process_command  cmd = match cmd with
    Import(f) -> 
      process_file f 
  | Eval(_,t) -> 
      let t' = eval t in
      printtm_ATerm true t'; 
      force_newline();
      ()


let main () = 
  let inFile = parseArgs() in
  let _ = process_file inFile  in
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
