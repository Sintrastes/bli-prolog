%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% _*_ user_defined_lists.bpl _*_
%
% Author: Nathan Bedell
% Email:  nbedell@tulane.edu
%
% An example showing how to define simple
% algebraic data types using bedelibry prolog.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Bedelibry prolog does not currently
% support polymorphic datatypes.

datatype intlist where
  constructor 'Null.
  constructor 'Cons: int.

%
% * Examples:
% 
% intlist('Cons(1,'Null)).
%   > True.
%
% intlist('Cons(2,'Cons(1,'Null))).
%   > True.
%
%

% However, we can (as long as it is enabled) use
% datatypes with ad hoc polymorphism, i.e:

datatype stringlist where
  constructor 'Null.
  constructor 'Cons: string.

% Thus, we have:

%
% * Ad hoc datatype polymorphism example:
%
% intlist('Null).
%  > True.
%
% stringlist('Null).
%  > True.
%