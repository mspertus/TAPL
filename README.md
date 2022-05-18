# TAPL
Modify implementations for Pierce' Types and Programming Languages to be usable in a REPL
and reorganize them as Dune projects.

There is one directory for each implementation at https://www.cis.upenn.edu/~bcpierce/tapl/.
(So far there is only `arith` and `fulluntyped`). 

## Usage
Go into the directory for the implementation (e.g., `arith`) and build

```dune build . --profile release```

Run the supplied tests 
```dune exec arith test.f```

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
> import "test.f"
> import "test.f";
"hello"
x
x
true
/* Snip */
120.
> if false then true else false;
false
> (lambda x. x) (lambda x. x x);
(lambda x'. x' x')
> ^D
$
```
