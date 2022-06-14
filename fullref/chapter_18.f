/* 18.2 Objects */
c = let x = ref 1 in
  { get = lambda _:Unit. !x,
    inc = lambda _:Unit. x:= succ(!x)};
c.inc unit;
c.get unit;
(c.inc unit; c.inc unit; c.get unit);

Counter = {get:Unit->Nat, inc:Unit->Unit};

inc3 = lambda c:Counter.
  (c.inc unit; c.inc unit; c.inc unit);
(inc3 c; c.get unit);

/* 18.3 Object Generators */
newCounter =
  lambda _:Unit. let x = ref 1 in
    { get = lambda _:Unit. !x,
      inc = lambda _:Unit. x:=succ(!x)};

/* 18.4 Subtyping */
ResetCounter = {get:Unit->Nat, inc:Unit->Unit, reset:Unit->Unit};
newResetCounter =
  lambda _:Unit. let x = ref 1 in
    {get = lambda _:Unit. !x,
     inc = lambda _:Unit. x:=succ(!x),
     reset = lambda _:Unit. x:=1};
rc = newResetCounter unit;
(inc3 rc; rc.reset unit; inc3 rc; rc.get unit);

/* 18.5 Grouping Instance Variables */
c = let r = {x = ref 1} in
  {get = lambda _:Unit. !(r.x),
   inc = lambda _:Unit. r.x:=succ(!(r.x))};

CounterRep = {x: Ref Nat};

/* 18.6 Simple Classes */
counterClass =
  lambda r:CounterRep.
    {get = lambda _:Unit. !(r.x),
     inc = lambda _:Unit. r.x:=succ(!(r.x))};

resetCounterClass =
  lambda r:CounterRep.
    let super = counterClass r in
      {get = super.get,
       inc = super.inc,
       reset = lambda _:Unit. r.x:=1};

newResetCounter =
  lambda _:Unit. let r = {x=ref 1} in resetCounterClass r;
