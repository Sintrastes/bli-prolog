
i : entity
killed : entity <- prop -> entity.
the : entity <- entity.
man : person.

% Two different senses of with
with : prop   -> prop <- item.
with : person <- prop -> item.

spoon : item.

% i killed : prop -> entity.
% the man : entity.
% i killed the man : prop.
% with (the spoon) : prop -> prop
% I killed the man with the spoon : prop.
% with(killed(i, the(man)), spoon).