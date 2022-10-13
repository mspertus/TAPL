/* Examples for testing */

 "hello";

lambda X. lambda x:X. x; 
(lambda X. lambda x:X. x) [All X.X->X]; 

 {*All Y.Y, lambda x:(All Y.Y). x} as {Some X,X->X};


lambda x:Bool. x;
(lambda x:Bool->Bool. if x false then true else false) 
  (lambda x:Bool. if x then false else true); 

lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0); 

{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 


{*Nat, {c=0, f=lambda x:Nat. succ x}}
  as {Some X, {c:X, f:X->Nat}};
let {X,ops} = {*Nat, {c=0, f=lambda x:Nat. succ x}}
              as {Some X, {c:X, f:X->Nat}}
in (ops.f ops.c);


T = Nat->Nat;
lambda f:T. lambda x:Nat. f (f x);


unit;

lambda x:Top. x;
 (lambda x:Top. x) (lambda x:Top. x);
(lambda x:Top->Top. x) (lambda x:Top. x);


lambda X<:Top->Top. lambda x:X. x x; 


if true then {x=true,y=false,a=false} else {y=false,x={},b=false};

timesfloat 2.0 3.14159;

let x=true in x;

(lambda r:{x:Top->Top}. r.x r.x) 
  {x=lambda z:Top.z, y=lambda z:Top.z}; 


lambda x:A. x;
