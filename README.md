What Is It?
===========

An implementation of a dialect of pure prolog, called *bedelibry prolog* (or bli prolog), based on the [pure-prolog](https://github.com/kfl/pure-prolog) implementation by Ken Friis Larsen. Created for use in [bedelibry](https://github.com/Chinchillord/Bedellibrary).

How to install
==============

bli-prolog requires that you have the relevant version of the haskell platform installed on your system. For instance, on Ubuntu, if you have not already done so, you should first run:

~~~
  $ sudo apt install ghc cabal-install
~~~ 

~~~
  $ git clone https://github.com/Sintrastes/bli-prolog
  $ cd bli-prolog
  $ cabal install
~~~

optionally, bli-prolog may be installed with stack.

Language Features
=================

Bedelibry prolog has some features which differentiate it from pure prolog (i.e. prolog without any of its imperative features). We list some of these features below:

Existential Quantification:
--------------------------

TODO: Explain this feature.
~~~
  \Y. programming_language(X), name_of(X,Y).   
~~~ 

For the fans of unicode, we also offer some alternate syntax options for this:
~~~
  λY. programming_language(X), name_of(X,Y).   
  ΛY. programming_language(X), name_of(X,Y).
~~~
Implicit Predication:
--------------------

For many of the intended use cases of bedelibry, we wish to apply
(i.e. store as a fact) that some predicate holds of a given resource.
If it is clear from context what that resource is, when writing the predicate,
explicit arguments can be omitted. For example, if `programming_language` is
a predicate, the following
~~~
  programming_language.
~~~
is syntatic sugar for
~~~
  programming_language(X).
~~~

This also works for n-ary relations. For instace, using the binary predicate `name`
~~~
  name("bob")
~~~
is syntatic sugar for
~~~
  name(X, "bob")
~~~

Schema
------
Predicates in bli prolog must adhere to a given *schema*, which is a .bsch (bli schema) file, consiting of rows of the form:
~~~
  [predicate_identifier]: [arity]
~~~
For example, in the implicit predication example we looked at above, we could have used a schema containing the lines:
~~~
  name: 2
  programming_language: 1
~~~

Entity Management
-----------------
TODO: Explain this feature

Assertions
----------
Assertions in bli prolog are made by terminating queries with a "!" instead of a ".". This works for both rules and simple facts.
~~~
?- siblings(X,Y) :- child(X,Z), child(Y,Z)!
?- child(bob,  susan)!
?- child(mark, susan)!
?- siblings(bob, mark).
true.
~~~
This should also work for declaring new predicates in the same format as a schema file.
~~~
?- programming_language: 1!
~~~

Side-effects
------------

Although bli prolog is built ontop on pure Prolog, not all of our predicates are side-effect free.

In particular, when run in assertion mode (explained above), predicates declared in the schema as a 
*type*, i.e.
~~~
  programming_language: type
~~~
will attempt to declare new entities of a given type, storing this entity data in the configured bedelibry server, if an entity with such an id does not already exist. For instance:
~~~
  ?- programming_language(nim).
  false.
  ?- programming_language(nim)!
  OK. Added entity "nim" to list of programming languages.
  ?- programming_language(nim).
  true.
~~~
However, if we try to assert this again:
~~~
  ?- programming_language(nim).
  FAIL. Entity "nim" already exists in the configured server.
~~~

Note however, that in bli prolog, the only side effects that can occur are when *asserting* predicates. However, it is possible that more side effects can occur than purely asserting some predicate. For example, asserting a new entity of a given type also has the side effect of storing this assertion on the bedelibry server, if it has been configured to run with bli prolog.

Interface
=========

The help message you get by calling `bli-prolog -?`:

~~~
bli-prolog interpreter v0.1, (C) Nathan Bedell 2019

options [OPTIONS] [GOALSTRING]


Common flags:
  -s --search=SEARCH    Specify whether to use DFS, BFS, or Limited
  -p --program=FILE     Prolog file with clauses
  -l --limit=INT        Limit the number of solutions found
  -d --depth=INT        Maximum depth to traverse when using limited search
  -i --info=ANALYSIS    Don't interpret program, only analyse it
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
~~~

Valid values for analysis are `External`, `Uses`, and `Interface`. If
you want see both which predicates are declared (the interface) and
which predicates are called (the uses) in the file `myprolog.pl` then
use the command:

~~~
  $ bli-prolog -p myprolog.pl -i interface -i uses
~~~

This will first print the interface (one predicate per line), then a
blank line, and finally the uses (one predicate per line).

If you just want a list of the external (or build-in) predicates that
are used, then use `-i external`.

Note: To use the REPL, simply supply "repl" as the goalstring. 

Todo
----

See [todo list](todo.md).