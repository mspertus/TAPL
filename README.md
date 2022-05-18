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
