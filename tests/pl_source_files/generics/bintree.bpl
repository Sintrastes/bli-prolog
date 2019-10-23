%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% _*_ bintree.bpl _*_
%
% Author: Nathan Bedell
% Email:  nbedell@tulane.edu
%
% Simple example of a generic binary tree type in 
% Bli Prolog.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

datatype bintree[A] where
  constructor 'Node: A.
  constructor 'Branch: bintree[A], bintree[A].