?- listing(db).
:- dynamic db/1.

db(female(marie)).
db(female(helen)).
db(female(miranda)).
db(male(joe)).
db(male(bill)).
db(male(paul)).
db(male(tom)).
db(parent_of(helen, joe)).
db(parent_of(bill, joe)).
db(parent_of(helen, marie)).
db(parent_of(bill, marie)).
db(parent_of(tom, helen)).
db(parent_of(miranda, paul)).

true.

?- query_db("Who is parent of Joe").
parent_of(helen,joe)
parent_of(bill,joe)
true.

?- query_db("Who is sister of Joe").
sister_of(marie,joe)
true.

?- 

Sample Input/Output

