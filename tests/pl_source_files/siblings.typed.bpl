% -*- typed bli-prolog -*-

type person.

mother_child: person, person.
father_child: person, person.
sibling:      person, person.
parent_child: person, person.

mother_child(marge, bart).
mother_child(marge, lisa).
mother_child(marge, maggie).
mother_child(mona, homer).
mother_child(mona, jay).

father_child(homer, bart).
father_child(homer, lisa).
father_child(homer, maggie).
father_child(abe, homer).
father_child(abe, herb).
father_child(abe, abbie).

sibling(X, Y) :- parent_child(Z, X), parent_child(Z, Y).

parent_child(X, Y) :- father_child(X, Y).
parent_child(X, Y) :- mother_child(X, Y).

