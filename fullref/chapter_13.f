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
