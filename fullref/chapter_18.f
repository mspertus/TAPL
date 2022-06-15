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

/* 18.7 Adding instance variables */
BackupCounter = {get:Unit->Nat, inc:Unit->Unit,
  reset:Unit->Unit, backup:Unit->Unit};

BackupCounterRep = {x: Ref Nat, b: Ref Nat};

backupCounterClass =
  lambda r:BackupCounterRep.
    let super = resetCounterClass r in
      {get = super.get,
       inc = super.inc,
       reset = lambda _:Unit. r.x:=!(r.b),
       backup = lambda _:Unit. r.b:=!(r.x)};

/* 18.8 Calling Superclass Methods */
funnyBackupCounterClass =
  lambda r:BackupCounterRep.
    let super = backupCounterClass r in
      {get = super.get,
       inc = lambda _:Unit. (super.backup unit; super.inc unit),
       reset = super.reset,
       backup = super.backup};

/* 18.9 Classes with Self */
SetCounter = {get:Unit->Nat, set:Nat->Unit, inc:Unit->Unit};

setCounterClass =
  lambda r:CounterRep.
    fix
      (lambda self: SetCounter.
        {get = lambda _:Unit. !(r.x),
         set = lambda i:Nat. r.x:=i,
         inc = lambda _:Unit. self.set (succ (self.get unit))});

newSetCounter =
  lambda _:Unit. let r = {x=ref 1} in
    setCounterClass r;

/* 18.10 Open Recursion through Self */
setCounterClass =
  lambda r:CounterRep.
    lambda self: SetCounter.
      { get = lambda _:Unit. !(r.x),
        set = lambda i:Nat. r.x:=i,
        inc = lambda _:Unit. self.set (succ(self.get unit))};

newSetCounter =
  lambda _:Unit. let r = {x=ref 1} in
    fix (setCounterClass r);

InstrCounter = {get:Unit->Nat, set:Nat->Unit,
  inc:Unit->Unit, accesses:Unit->Nat};

InstrCounterRep = {x:Ref Nat, a: Ref Nat};

instrCounterClass = 
  lambda r:InstrCounterRep.
    lambda self: InstrCounter.
      let super = setCounterClass r self in  
        {get = super.get,
         set = lambda i:Nat. (r.a:= succ(!(r.a)); super.set i),
         inc = super.inc,
         accesses = lambda _:Unit. !(r.a)};

/* 18.11 Open Recursion and Evaluation */
/* Unsuccessful attempt at creating instrCounter instances */
newInstrCounter = 
    lambda _:Unit. let r = {x=ref 1, a=ref 0} in
        fix (instrCounterClass r);

/* Uncomment the following to show instance creation never completes */
/* ic = newInstrCounter unit; */

/* Resolve with dummy lambda abstraction to suppress premature evaluation */
setCounterClass =
    lambda r:CounterRep.
    lambda self: Unit->SetCounter.
        lambda _:Unit.
            {get = lambda _:Unit. !(r.x),
             set = lambda i:Nat. r.x:=i,
             inc = lambda _:Unit. (self unit).set(succ((self unit).get unit))};

newSetCounter =
    lambda _:Unit. let r = {x = ref 1} in
        fix (setCounterClass r) unit;

instrCounterClass =
    lambda r:InstrCounterRep.
    lambda self: Unit->InstrCounter.
        lambda _:Unit.
            let super = setCounterClass r self unit in
                {get = super.get,
                 set = lambda i:Nat. (r.a:=succ(!(r.a)); super.set i),
                 inc = super.inc,
                 accesses = lambda _:Unit. !(r.a)};

newInstrCounter =
    lambda _:Unit. let r = {x=ref 1, a=ref 0} in
        fix (instrCounterClass r) unit;

/* Now this works */
ic = newInstrCounter unit;
(ic.set 5; ic.accesses unit);
(ic.inc unit; ic.get unit);
ic.accesses unit;

/* 18.12 A More Efficient Implementation */
setCounterClass =
    lambda r:CounterRep. lambda self: Ref SetCounter.
        {get = lambda _:Unit. !(r.x),
         set = lambda i:Nat. r.x:= i,
         inc = lambda _:Unit. (!self).set (succ ((!self).get unit))};

dummySetCounter =
    {get = lambda _:Unit. 0,
     set = lambda i:Nat. unit,
     inc = lambda _:Unit. unit};

newSetCounter =
    lambda _:Unit.
        let r = {x=ref 1} in
        let cAux = ref dummySetCounter in
        (cAux := (setCounterClass r cAux); !cAux);

/* Uncomment this to see how this approach can run into type mismatches */
/*
instrCounterClass =
    lambda r:InstrCounterRep. lambda self: Ref InstrCounter.
    let super = setCounterClass r self in
    {get = super.get,
     set = lambda i:Nat. (r.a:=succ(!(r.a)); super.set i),
     inc = super.inc,
     accesses = lambda _:Unit. !(r.a)};
*/

/* Regain covariance by using Source to indicate the class only reads from 
   the method pointer*/
setCounterClass =
    lambda r:CounterRep. lambda self: Source SetCounter.
        {get = lambda _:Unit. !(r.x),
         set = lambda i:Nat. r.x:=i,
         inc = lambda _:Unit. (!self).set (succ ((!self).get unit))};

instrCounterClass =
    lambda r:InstrCounterRep. lambda self: Source InstrCounter.
    let super = setCounterClass r self in
    {get = super.get,
     set = lambda i:Nat. (r.a:=succ(!(r.a)); super.set i),
     inc = super.inc,
     accesses = lambda _:Unit. !(r.a)};

dummyInstrCounter =
    {get = lambda _:Unit. 0,
     set = lambda i:Nat. unit,
     inc = lambda _:Unit. unit,
     accesses = lambda _:Unit. 0};

newInstrCounter =
    lambda _:Unit.
        let r = {x=ref 1, a=ref 0} in
        let cAux = ref dummyInstrCounter in
        (cAux := (instrCounterClass r cAux); !cAux);

ic = newInstrCounter unit;
(ic.set 5; ic.accesses unit);
(ic.inc unit; ic.get unit);
ic.accesses unit;