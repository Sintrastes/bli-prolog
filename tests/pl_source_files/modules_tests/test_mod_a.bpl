
using module_b.
% rel b.

using module_c.
% rel c.

% If this line is uncommented, 
% there will be an error, since this is already
% defined in module_b.

rel a.

a.
b :- a.