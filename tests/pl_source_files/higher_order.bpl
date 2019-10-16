%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% _*_ higher_order.bpl _*_
%
% Author: Nathan Bedell
% Email:  nbedell@tulane.edu
%
% Higher order rules example in bli prolog.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%% schema %%%%%

  type  person.
  nate: person.

  rel a.
  rel b.
  
%%%% program %%%%

  rel thinks: person, rule.
  rel according_to: person, pred.
  rel rational: person.  

  according_to(Person, P)
    :- with_rule(Rule, P), according_to(Person, Rule).
  
  % Something is true according to a person
  % if that person is rational, and that thing
  % can be proven given our current knowledge.
  according_to(Person, P)
    :- P, rational(Person).

%%%% facts %%%%

  rational(nate).
  a.
  b :- a.
  
% e.x:
% according_to(nate, b)
%   > True.

% b is true for an arbitrary rational person.
% {according_to(X, b) :- rational(X)}.
%    > True.

% Note: The above example tries to check the validity of a rule.
% This is a specail feature, and I'm not entirely sure how to implement it.
%
% Likely this should be limited, as in general the validity of horn clauses
% is undecidable.
% 