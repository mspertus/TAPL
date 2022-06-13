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
      inc = lambda _:Unit. x:= succ(!x)};
