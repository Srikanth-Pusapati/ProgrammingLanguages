family(F)-->name(N),desc([D|Xs]),{F=..[D,N|Xs]}.

name(marie)-->['Marie'].
name(helen)-->['Helen'].
name(miranda)-->['Miranda'].
name(joe)-->['Joe'].
name(bill)-->['Bill'].
name(paul)-->['Paul'].
name(tom)-->['Tom'].

desc([D])-->[is],gender(D).
desc([D,N])-->[is],rel(D),[of],name(N).

gender(female)-->[female].
gender(male)-->[male].

rel(parent_of)-->[parent].
rel(sibling_of)-->[sibling].
rel(brother_of)-->[brother].
rel(sister_of)-->[sister].
rel(mother_of)-->[mother].
rel(father_of)-->[father].
rel(gp_of)-->[grandparent].
rel(cousin_of)-->[cousin].

data:-maplist(entry,
  ["Marie is female",
   "Helen is female",
   "Miranda is female",
   "Joe is male",
   "Bill is male",
   "Paul is male",
   "Tom is male",	
   "Helen is parent of Joe",
   "Bill is parent of Joe",
   "Helen is parent of Marie",
   "Bill is parent of Marie",
   "Mike is parent of Bill",
   "Tom is parent of Helen",
   "Miranda is parent of Paul",
   "Mike is parent of Miranda"
  ]).

entry(X):-
  split_string(X," ","",R),maplist(atom_string,Xs,R),
  family(Tree,Xs,[]),assert(db(Tree)),
  fail
; writeln(done).

ques(_Who)-->['Who'].
query(T)-->ques(Who),desc([D|Xs]),{T=..[D,Who|Xs]}.

query_db(Query,QueryTree):-
  query(QueryTree,Query,[]),
  QueryTree.
  
query_db(Q):- 
  split_string(Q," ","",R),
  maplist(atom_string,Query,R),
  forall(
    query_db(Query,QueryTree),
    writeln(QueryTree)
  ).

parent_of(X,S):-db(parent_of(X,S)).
sibling_of(X,S):-db(parent_of(N,X)),db(parent_of(N,S)),X\==S.
brother_of(B,X):-sibling_of(B,X),db(male(B)).
sister_of(B,X):-sibling_of(B,X),db(female(B)).
mother_of(M,X):-db(parent_of(M,X)),db(female(M)).
father_of(M,X):-db(parent_of(M,X)),db(male(M)).
gp_of(GP,X):-db(parent_of(N,X)),db(parent_of(GP,N)).
cousin_of(X,C):-
  gp_of(GP,X),
  gp_of(GP,C),
  X\==C,
  not(sibling_of(X,C)).
