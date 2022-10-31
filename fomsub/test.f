/* Examples for testing */

 lambda x:Top. x;
  (lambda x:Top. x) (lambda x:Top. x);
 (lambda x:Top->Top. x) (lambda x:Top. x);
 

lambda X. lambda x:X. x; 
(lambda X. lambda x:X. x) [All X.X->X]; 

lambda X<:Top->Top. lambda x:X. x x; 

