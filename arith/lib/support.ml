open Format

module Error = struct

exception Exit of int

type info = FI of string * int * int | UNKNOWN
type 'a withinfo = {i: info; v: 'a}

let dummyinfo = UNKNOWN
let createInfo f l c = FI(f, l, c)

let errf f = 
  print_flush(); 
  open_vbox 0; 
  open_hvbox 0; f(); print_cut(); close_box(); print_newline();
  raise (Exit 1)

let printInfo =
  (* In the text of the book, file positions in error messages are replaced
     with the string "Error:" *)
  function
    FI(f,l,c) ->
      print_string f; 
      print_string ":"; 
      print_int l; print_string "."; 
      print_int c; print_string ":"
  | UNKNOWN ->
      print_string "<Unknown file and line>: "

let errfAt fi f = errf(fun()-> printInfo fi; print_space(); f())

let err s = errf (fun()-> print_string "Error: "; print_string s; print_newline())

let error fi s = errfAt fi (fun()-> print_string s; print_newline())

let warning s =
  print_string "Warning: "; print_string s;
  print_newline()

let warningAt fi s =
  printInfo fi; print_string " Warning: ";
  print_string s; print_newline()

end

(* ---------------------------------------------------------------------- *)

module Pervasive = struct

(* The printing functions call these utility functions to insert grouping
  information and line-breaking hints for the pretty-printing library:
     obox   Open a "box" whose contents will be indented by two spaces if
            the whole box cannot fit on the current line
     obox0  Same but indent continuation lines to the same column as the
            beginning of the box rather than 2 more columns to the right
     cbox   Close the current box
     break  Insert a breakpoint indicating where the line maybe broken if
            necessary.
  See the documentation for the Format module in the OCaml library for
  more details. 
*)

let obox() = open_hvbox 2

let obox0() = open_hvbox 0

let cbox() = close_box()
let break() = print_break 0 0
let pr = Format.print_string

end (* module pervasive *)


