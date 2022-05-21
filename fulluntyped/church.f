tru = lambda t. lambda f. t;
fls = lambda t. lambda f. f;
test = lambda l. lambda m. lambda n. l m n;
and = lambda b. lambda c. b c fls;
pair = lambda f. lambda s. lambda b. b f s;
fst = lambda p. p tru;
snd = lambda p. p fls;
c0 = lambda s. lambda z. z;
c1 = lambda s. lambda z. s z;
c2 = lambda s. lambda z. s (s z);
scc = lambda n. lambda s. lambda z. s (n s z);
plus = lambda m. lambda n. lambda s. lambda z. m s (n s z);
times = lambda m. lambda n. m (plus n) c0;
iszro = lambda m. m (lambda x. fls) tru;
zz = pair c0 c0;
ss = lambda p. pair (snd p) (plus c1 (snd p));
prd = lambda m. fst (m ss zz);

/* Exercise 5.2.5 */
minus = lambda m. lambda n. n prd m;

/* Exercise 5.2.7 */
equal = lambda m. lambda n. and (iszro (minus m n)) (iszro (minus n m));

realbool = lambda b. b true false;
churchbool = lambda b. if b then tru else false;
realnat = lambda m. m (lambda x. succ x) 0;
realeq = lambda m. lambda n. (equal m n) true false;

/* Uncommenting divergent combinator omega results in stack overflow (as expected) */
/* omega = (lambda x. x x) (lambda x. x x); */
fix = lambda f. (lambda x. f (lambda y. x x y)) (lambda x. f (lambda y. x x y));

/* Factorial example in section 5.2.9 */
g = lambda fct. lambda n. if realeq n c0 
  then c1
  else (times n (fct (prd n)));

factorial = fix g;
