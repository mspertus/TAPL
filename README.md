# TAPL
Modify implementations for Pierce' [Types and Programming Languages](https://www.cis.upenn.edu/~bcpierce/tapl/)
to be usable in a REPL
and reorganize them as Dune projects.

There is one directory for each implementation at https://www.cis.upenn.edu/~bcpierce/tapl/. Be sure
to change into that directory before running the below commands.

## Prerequisites
The usage instructions below assume you have installed both ocaml and dune 3. You can either 
**Option 1:** Install them on your local computer. I followed the instructions [here](https://ocaml.org/docs/up-and-running)) 
**Option 2:** Launch a preconfigured development using the [devfile](https://devfile.io/) in the repo. I use [CodeCatalyst](codecatalyst.aws/),
which has built-in support for creating cloud-hosted development environments from devfiles. See [Running in CodeCatalyst](#running-in-codecatalyst) below for
instructions.

## Usage
Go into the directory for the implementation (e.g., `arith`) and build

```
dune build . --profile release
```

Run the supplied tests 
```
dune exec arith test.f --profile release
```

Run the REPL
```
$ dune exec arith repl --profile release
> true;
true
> if true;
Parse Error
> if true then false else true;
false
> iszero(pred(pred(2)));
true
> ^D
$
```
Here's an example for `fulluntyped`, the full untyped lambda calculus
```
$ dune build . --profile release
$ dune exec fulluntyped repl --profile release
> "hello";
"hello"
> if false then true else false;
false
> (lambda x. x) (lambda x. x x);
(lambda x'. x' x')
> ^D
$
```
Some of the implementations are supplemented with files containing useful definitions from the textbook. For example `fulluntyped/church.f` contains implementations related to Church booleans and numerals.
```
$ dune exec fulluntyped repl --profile release
> import "church.f";
tru = lambda t. lambda f. t
fls = lambda t. lambda f. f
test = lambda l. lambda m. lambda n. l m n
and = lambda b. lambda c. b c fls
pair = lambda f. lambda s. lambda b. b f s
fst = lambda p. p tru
snd = lambda p. p fls
c0 = lambda s. lambda z. z
c1 = lambda s. lambda z. s z
c2 = lambda s. lambda z. s (s z)
scc = lambda n. lambda s. lambda z. s (n s z)
> scc c0;
(lambda s. lambda z. s ((lambda s'. lambda z'.z') s z))
> scc c0 fst (pair 1 2);
1
> scc c0 snd (pair 1 2);
2
```

## Turning an implementation into a dune project
**Warning: This section can safely be ignored. I have already performed these steps in the repo.**
However, in case it is useful as a reference, here are the steps due to Jordan Merrick (thanks!) that
I follow to change the implementations on the book website to Dune projects (this
does not include any semantic changes like adding a REPL, which tend to be specific to the particular
implementation).

**Note:** Due to Ocaml's casing conventions, in the below, `name` denotes the implementation name,
like `fullsimple`, and `Name` denotes the implementation name with leading capitalization, like
`Fullsimple`.

### Create a dune project
```dune init project name```

### Move the `.ml` files from their original location to the new project
```
mv orig/main.ml name/bin/
mv orig/*.ml* name/lib/
mv test.f name
```
### Tell dune about the lexer and parser
Add the following lines to the bottom of `name/lib/dune`
```
(ocamllex lexer)
(ocamlyacc parser)
```
Add the following lines to `name/bin/main.ml` below the imports
``` ocaml
module Lexer = Name.Lexer
module Parser = Name.Parser
```

### Adjust the imports in main.ml
This change only needs to be made in `main.ml`.
Some imports will be local, now from `lib`. These should have `Name.` prepended to the imports.
For example, `open Support` should be changed to `open Name.Support`. 

### Enjoy
Build and test to confirm everything is good to go

```  
dune build . --profile release
dune exec name test.f --profile release
```
## Running in CodeCatalyst
