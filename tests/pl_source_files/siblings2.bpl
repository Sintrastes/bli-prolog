% -*- prolog -*-

member: 2
mother_children: 2
mother_child: 2
father_child: 2
sibling: 2
parent_child: 2

member(X, [X|_]).
member(X, [_|T]) :- member(X,T).

mother_children(marge, [bart, lisa, maggie]).
mother_children(mona, [homer, jay]).
mother_child(X, Y) :- mother_children(X, C), member(Y, C).

father_child(homer, bart).
father_child(homer, lisa).
father_child(homer, maggie).
father_child(abe, homer).
father_child(abe, herb).
father_child(abe, abbie).

sibling(X, Y) :- parent_child(Z, X), parent_child(Z, Y).

parent_child(X, Y) :- father_child(X, Y).
parent_child(X, Y) :- mother_child(X, Y).
