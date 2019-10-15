% -*- typed bli-prolog -*-

type person.

rel mother_child: person, person.
rel father_child: person, person.
rel sibling:      person, person.
rel parent_child: person, person.

% Note that this should not compile unless we first declare
% that all of the entities in the list below are people.

marge: person.
bart: person.
lisa: person.
maggie: person.
mona: person.
homer: person.
jay: person.
abe: person.
abbie: person.

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

