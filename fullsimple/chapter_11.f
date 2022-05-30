/* Example showing how to use a variant */
A = < x: Nat, y: Float>;
a = < x = 3 > as A;
case a of
    <x = xx> ==> "Has Nat"
  | <y = yy> ==> "Has Float";

/* Records */
B = { x: Nat, y : String };
f = lambda x : B. x.x;
b = { x = 3, y = "three" };
f b;

/* Variant of records */
V = < n : B, f : { l: Float, r : String } >;
v = <n = b> as V;
case v of
    <n = nn> ==> nn.y
  | <f = ff> ==> ff.r;
