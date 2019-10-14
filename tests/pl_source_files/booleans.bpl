
datatype bool where
  constructor 'True.
  constructor 'False.

% functional notation
if_then_else: bool -> A -> A -> A.

% functional notation.
if_then_else('True, X, Y) = X.
if_then_else('False, X, Y) = Y.