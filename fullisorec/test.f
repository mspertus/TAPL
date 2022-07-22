/* Examples for testing */

 "hello";

unit;

timesfloat 2.0 3.14159;

let x=true in x;

lambda x:Bool. x;
(lambda x:Bool->Bool. if x false then true else false) 
  (lambda x:Bool. if x then false else true); 

lambda x:Nat. succ x;
(lambda x:Nat. succ (succ x)) (succ 0); 

{x=true, y=false}; 
{x=true, y=false}.x;
{true, false}; 
{true, false}.1; 


 
lambda x:<a:Bool,b:Bool>. x;



Counter = Rec P. {get:Nat, inc:Unit->P}; 
p = 
let create = 
  fix 
    (lambda cr: {x:Nat}->Counter.
      lambda s: {x:Nat}.
        fold [Counter]
          {get = s.x,
           inc = lambda _:Unit. cr {x=succ(s.x)}})
in
  create {x=0};
p1 = (unfold [Counter] p).inc unit;
(unfold [Counter] p1).get;


T = Nat->Nat;
lambda f:T. lambda x:Nat. f (f x);


lambda x:A. x;
