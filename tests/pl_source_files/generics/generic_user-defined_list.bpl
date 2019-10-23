%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% _*_ generic_user-defined_lists.bpl _*_
%
% Author: Nathan Bedell
% Email: nbedell@tulane.edu
%
% An example illustrating how parametric polymorphism
% works with a user-defined list datatype. 
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

datatype my_list[A] where
  constructor 'Nil
  constructor 'Cons: A.

%
% Note that the type variables used in
% the constructors of a polymorphic
% datatype must be a subset of the
% type variables used in the definition.
%
% So, for instance, the following does not compile:
%
% datatype my_list[A] where
%   constructor 'Nil.
%   constructor 'Cons: B.
%
