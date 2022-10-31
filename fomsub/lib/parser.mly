/*  
 *  Yacc grammar for the parser.  The files parser.mli and parser.ml
 *  are generated automatically from parser.mly.
 */

%{
open Support.Error
open Support.Pervasive
open Syntax
%}

/* ---------------------------------------------------------------------- */
/* Preliminaries */

/* We first list all the tokens mentioned in the parsing rules
   below.  The names of the tokens are common to the parser and the
   generated lexical analyzer.  Each token is annotated with the type
   of data that it carries; normally, this is just file information
   (which is used by the parser to annotate the abstract syntax trees
   that it constructs), but sometimes -- in the case of identifiers and
   constant values -- more information is provided.
 */

/* Keyword tokens */
%token <Support.Error.info> IMPORT
%token <Support.Error.info> LAMBDA
%token <Support.Error.info> TYPE
%token <Support.Error.info> LEQ
%token <Support.Error.info> TTOP
%token <Support.Error.info> ALL

/* Identifier and constant value tokens */
%token <string Support.Error.withinfo> UCID  /* uppercase-initial */
%token <string Support.Error.withinfo> LCID  /* lowercase/symbolic-initial */
%token <int Support.Error.withinfo> INTV
%token <float Support.Error.withinfo> FLOATV
%token <string Support.Error.withinfo> STRINGV

/* Symbolic tokens */
%token <Support.Error.info> APOSTROPHE
%token <Support.Error.info> DQUOTE
%token <Support.Error.info> ARROW
%token <Support.Error.info> BANG
%token <Support.Error.info> BARGT
%token <Support.Error.info> BARRCURLY
%token <Support.Error.info> BARRSQUARE
%token <Support.Error.info> COLON
%token <Support.Error.info> COLONCOLON
%token <Support.Error.info> COLONEQ
%token <Support.Error.info> COLONHASH
%token <Support.Error.info> COMMA
%token <Support.Error.info> DARROW
%token <Support.Error.info> DDARROW
%token <Support.Error.info> DOT
%token <Support.Error.info> EOF
%token <Support.Error.info> EQ
%token <Support.Error.info> EQEQ
%token <Support.Error.info> EXISTS
%token <Support.Error.info> GT
%token <Support.Error.info> HASH
%token <Support.Error.info> LCURLY
%token <Support.Error.info> LCURLYBAR
%token <Support.Error.info> LEFTARROW
%token <Support.Error.info> LPAREN
%token <Support.Error.info> LSQUARE
%token <Support.Error.info> LSQUAREBAR
%token <Support.Error.info> LT
%token <Support.Error.info> RCURLY
%token <Support.Error.info> RPAREN
%token <Support.Error.info> RSQUARE
%token <Support.Error.info> SEMI
%token <Support.Error.info> SLASH
%token <Support.Error.info> STAR
%token <Support.Error.info> TRIANGLE
%token <Support.Error.info> USCORE
%token <Support.Error.info> VBAR

/* ---------------------------------------------------------------------- */
/* The starting production of the generated parser is the syntactic class
   toplevel.  The type that is returned when a toplevel is recognized is
     Syntax.context -> (Syntax.command list * Syntax.context) 
   that is, the parser returns to the user program a function that,
   when given a naming context, returns a fully parsed list of
   Syntax.commands and the new naming context that results when
   all the names bound in these commands are defined.

   All of the syntactic productions in the parser follow the same pattern:
   they take a context as argument and return a fully parsed abstract
   syntax tree (and, if they involve any constructs that bind variables
   in some following phrase, a new context).
   
*/

%start toplevel
%type < Syntax.context -> (Syntax.command list * Syntax.context) > toplevel
%%

/* ---------------------------------------------------------------------- */
/* Main body of the parser definition */

/* The top level of a file is a sequence of commands, each terminated
   by a semicolon. */
toplevel :
    EOF
      { fun ctx -> [],ctx }
  | Command SEMI toplevel
      { fun ctx ->
          let cmd,ctx = $1 ctx in
          let cmds,ctx = $3 ctx in
          cmd::cmds,ctx }

/* A top-level command */
Command :
    IMPORT STRINGV { fun ctx -> (Import($2.v)),ctx }
  | Term 
      { fun ctx -> (let t = $1 ctx in Eval(tmInfo t,t)),ctx }
  | LCID Binder
      { fun ctx -> ((Bind($1.i,$1.v,$2 ctx)), addname ctx $1.v) }
  | UCID TyBinder
      { fun ctx -> ((Bind($1.i, $1.v, $2 ctx)), addname ctx $1.v) }

/* Right-hand sides of top-level bindings */
Binder :
    COLON Type
      { fun ctx -> VarBind ($2 ctx)}

/* All kind expressions */
Kind :
    ArrowKind
      { $1 }

ArrowKind :
    AKind DARROW ArrowKind  { fun ctx -> KnArr($1 ctx, $3 ctx) }
  | AKind
           { $1 }

/* All type expressions */
Type :
    ArrowType
                { $1 }
  | LAMBDA UCID OKind DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAbs($2.v, $3 ctx, $5 ctx1) }
  | ALL UCID OType DOT Type
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TyAll($2.v,$3 ctx,$5 ctx1) }

/* Atomic types are those that never need extra parentheses */
AType :
    LPAREN Type RPAREN  
           { $2 } 
  | UCID 
      { fun ctx ->
          TyVar(name2index $1.i ctx $1.v, ctxlength ctx) }
  | TTOP
      { fun ctx -> TyTop }
  | TTOP LSQUARE Kind RSQUARE
      { fun ctx -> maketop ($3 ctx) }

/* An "arrow type" is a sequence of atomic types separated by
   arrows. */
ArrowType :
    AppType ARROW ArrowType
     { fun ctx -> TyArr($1 ctx, $3 ctx) }
  | AppType
            { $1 }

Term :
    AppTerm
      { $1 }
  | LAMBDA LCID COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmAbs($1, $2.v, $4 ctx, $6 ctx1) }
  | LAMBDA USCORE COLON Type DOT Term 
      { fun ctx ->
          let ctx1 = addname ctx "_" in
          TmAbs($1, "_", $4 ctx, $6 ctx1) }
  | LAMBDA UCID OType DOT Term
      { fun ctx ->
          let ctx1 = addname ctx $2.v in
          TmTAbs($1,$2.v,$3 ctx,$5 ctx1) }

AppTerm :
    ATerm
      { $1 }
  | AppTerm ATerm
      { fun ctx ->
          let e1 = $1 ctx in
          let e2 = $2 ctx in
          TmApp(tmInfo e1,e1,e2) }
  | AppTerm LSQUARE Type RSQUARE
      { fun ctx ->
          let t1 = $1 ctx in
          let t2 = $3 ctx in
          TmTApp(tmInfo t1,t1,t2) }

/* Atomic terms are ones that never require extra parentheses */
ATerm :
    LPAREN Term RPAREN  
      { $2 } 
  | LCID 
      { fun ctx ->
          TmVar($1.i, name2index $1.i ctx $1.v, ctxlength ctx) }

AKind :
    STAR { fun ctx -> KnStar }
  | LPAREN Kind RPAREN  { $2 } 

OKind :
  /* empty */
     { fun ctx -> KnStar}
| COLONCOLON Kind 
     { $2 }

TyBinder :
    /* empty */
      { fun ctx -> TyVarBind(TyTop) }
  | COLONCOLON Kind
      { fun ctx -> TyVarBind(maketop ($2 ctx)) }
  | LEQ Type
      { fun ctx -> TyVarBind($2 ctx) }

AppType :
    AppType AType { fun ctx -> TyApp($1 ctx,$2 ctx) }
  | AType { $1 }

OType :
   /* empty */
      { fun ctx -> TyTop}
 | LEQ Type 
      { $2 }
 | COLONCOLON Kind
      { fun ctx -> maketop ($2 ctx) }


/*   */
