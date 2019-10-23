%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% _*_ closures.bpl _*_
%
% Author: Nathan Bedell
% Email:  nbedell@tulane.edu
%
% Definitions for reflexive, symmetric, and transitive
% closures of relations as higher-order predicates.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rel reflexive_closure:  pred, pred.
rel symmetric_closure:  pred, pred.
rel transitive_closure: pred, pred.

rel posetal_closure: pred, pred.
rel equiv_closure: pred, pred.

reflexive_closure(P)(X,X).
reflexive_closure(P)(X) :- P(X).

symmetric_closure(P)(Y,X)
  :- P(X,Y).
symmetric_closure(P)(X)
  :- P(X).

transitive_closure(P)(X,Z) 
  :- P(X,Y), P(Y,Z).
transitive_closure(P)(X)
  :- P(X).

% The least equivalence relation containing
% a given relation.
% Note: The application operator syntax
% is very useful here. This would be pretty ugly
% without it.
%equiv_closure(P, 
%   reflexive_closure 
% $ transitive_closure 
% $ symmetric_closure(P)
%).
%
%posetal_closure(P, 
%   reflexive_closure $ transitive_closure(P)
%).
