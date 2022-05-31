/* Some examples from the text */

/* 13.1 Basics */
r = ref 5;
!r;
r := 7;
!r;

/* 13.1 Side Effects and Sequencing */
(r:=succ(!r); !r);
(lambda _:Unit. !r)( r := succ(!r));
(r:=succ(!r); r:=succ(!r); r:=succ(!r); r:=succ(!r); !r);

/* 13.1 References and Aliasing */
s = r;
s := 82;
!r;

/* 13.1 Shared State */
c = ref 0;

incc = lambda x: Unit. (c := succ (!c); !c);
decc = lambda x: Unit. (c := pred (!c); !c);

incc unit;
decc unit;

o = {i = incc, d = decc};

/* 13.1 References to Compound Types */
NatArray = Ref (Nat->Nat);
newarray = lambda _:Unit. ref (lambda n:Nat.0);
lookup = lambda a:NatArray. lambda n:Nat. (!a) n;

/* I had to add this for update. Note that this equal,
   which works on Nats, differs from the one in fulluntyped,
   which works on Church numerals                           */
eql = lambda eq:Nat -> Nat -> Bool. lambda m:Nat. lambda n:Nat.
    if iszero m then iszero n
    else if iszero n then false
    else eq (pred m) (pred n);


equal = fix eql;

update = lambda a:NatArray. lambda m:Nat. lambda v:Nat.
    let oldf = !a in
    a := (lambda n:Nat. if equal m n then v else oldf n);

arr = newarray unit;
update arr 3 5;
arr 2;
arr 3;

