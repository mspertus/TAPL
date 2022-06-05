/* Example from section 15.2, The Subtype Relation */
Rx = {x: Nat};
f = lambda r:Rx. r.x;
Rxy = {x: Nat, y: Nat};
xy = {x=0, y=1};
f xy;

xy as Rx;
/* Uncomment the following line to see it is a type error */
/* {x = 0} as Rxy; */

