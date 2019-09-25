What Is It?
===========

An implementation of a dialect of pure prolog, called *bedelibry prolog* (or bli prolog), based on the [pure-prolog](https://github.com/kfl/pure-prolog) implementation by Ken Friis Larsen. Created for use in [bedelibry](https://github.com/Chinchillord/Bedellibrary).

Bedelibry prolog has some features which differentiate it from pure prolog (i.e. prolog without any of its imperative features). We list some of these features below:

  * Existential Quantification:
    * TODO: Explain this feature.
      ~~~
          \Y. programming_language(X), name_of(X,Y)    
      ~~~ 
  * Implicit Predication:
    * For many of the intended use cases of bedelibry, we wish to apply
      (i.e. store as a fact) that some predicate holds of a given resource.
      If it is clear from context what that resource is, when writing the predicate,
      explicit arguments can be omitted. For example, if `programming_language` is
      a predicate, the following
      ~~~
        programming_language.
      ~~~
      is syntatic sugar for
      ~~~
        programming_langugae(X).
      ~~~
      
      This also works for n-ary relations. For instace, using the binary predicate `name`
      ~~~
        name("bob")
      ~~~
      is syntatic sugar for
      ~~~
        name(X, "bob")
      ~~~


Interface
---------

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


How to install
--------------

~~~
  $ git clone https://github.com/kfl/pure-prolog.git
  $ cd pure-prolog
  $ stack install
~~~

Todo
----

  * Modify command line interface to suit my purposes
  * Write a REPL.