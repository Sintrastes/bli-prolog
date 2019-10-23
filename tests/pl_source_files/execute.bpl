
% An example program to test our runtime.

type person.
nate: person.
test: person.

rel putStrLn: string.

?- { 
  putStrLn("Hello, world!").
  putStrLn("I am going to make a query!").
  person(X). 
}