% Typed bpl file

type person.

nate: person.
annabelle: person.

rel likes: person, person.
rel narcissist: person.

likes(nate, annabelle).

narcissist(X) :- likes(X, X).