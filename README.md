What Is It?
===========

An implementation of a dialect of pure prolog, called *bedelibry prolog* (or bli prolog), based on the [pure-prolog](https://github.com/kfl/pure-prolog) implementation by Ken Friis Larsen. Created for use in [bedelibry](https://github.com/Chinchillord/Bedellibrary).

WARNING: The following readme is not meant to be documentation for bedelibry prolog at this stage in its development. This is merely meant to be an overview of what the language might look like once it is more fully implemented. Bedelibry is still in pre-release, and the API (either as presented below, or in the software itself) is liable to change as development progresses.

Checklist to 1.0
----------------

- [x] Implement type-checking of files.
- [ ] Implement higher-order predicates.
- [ ] Implement full Haskell interop.
- [ ] Implement language feature enabling/disabling.
- [ ] Write a simple REST API.

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

Bedelibry prolog has some features which differentiate it from pure prolog (i.e. prolog without any of its imperative features). Many of these features of the language can be enabled or disabled by editing the list `defaultLanguageOptions` in `Features.hs` before building. We list some of these features below:

Explicit Variable Binding:
--------------------------

In a conventional pure prolog system, if you had two predicates `programming_language(X)`, and `name_of(X,Y)`, and you wanted to get a listing of all of the possible names for terms that have been declared to be `programming_language`s, you might first try the query
~~~
  programming_language(X), name_of(X,Y).
~~~
However, this gives us too much information, and returns the pair of both the identifier for the programming language bound to the free variable `X`, together with its name bound to the free variable `Y`. In this scenario, we only care about the term bound to `Y`. Bedelibry prolog gives a simple solution to this problem, allowing the user to make queries of the form:
~~~
  \Y. programming_language(X), name_of(X,Y).   
~~~
This notation is intentionally similar to that of a lambda abstraction, although the semantics are slightly different. One can think of the above query as being an annonymous version of a horn clause, e.x. as follows:
~~~
  name_of_programming_language(Y) :- programming_language(X), name_of(X,Y). 
  name_of_programming_language(Y).
~~~

For the fans of unicode, we also offer some alternate syntax options for this:
~~~
  λY. programming_language(X), name_of(X,Y).   
  ΛY. programming_language(X), name_of(X,Y).
~~~

This syntax also allows the user to specify that they do *not* want their query to return any results. This will simply return success/fail depending on if the given formula is satisfiable.
~~~
  \. programming_language(X), name_of(X, "haskell").
    True.
~~~
Another feature of this syntax is that the user may additionally specify a uniqueness constraint on each one of the bound variables, and then only one solution will be returned matching that variable.
~~~
  \X!. programming_language(X).
    X = bli_prolog.
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
Predicates in bli prolog must adhere to a given *schema*, which is a .bsc (bli schema) file. This works by declaring a set of *types*, *relations*, and *entities*. To declare a new type in a bli schema file, you can use the syntax:

~~~
  type [name_of_type].
~~~

And given such a type, the user can declare new *entities* of a given type by using the syntax:

~~~
  [entity_identifier]: [type_name].
~~~

Finally, the user can declare relations between entities of different types by using the following syntax:

~~~
  rel [predicate_identifier]: [comma seperated list of types].
~~~

Note that nullary predicates can also be declared by using the notation:
~~~
  rel p.
~~~

Bedelibry prolog also supports the following prolog inspired notation:

~~~
  rel p/2.
~~~

which is simply syntatic sugar for
~~~
  rel p: entity, entity.
~~~

Literals
--------

Bedelibry Prolog has support for a number of different types of literals. For example, string literals must be surrounded by double quotes and have type `string`. Integer literals are unquoted, and have type `int`. These are all builtin types, and do not need to be declared like in the examples above.

Bedelibry Prolog also supports list literals, which have type `list[TYPE]`. For example, the literal `[1,2,3]` has type `list[int]`.

Finally, Bedelibry Prolog also has support for date and datetime literals, following the ISO 8601 standard. For example:

~~~
  '2019-10-09 : date.
  '2019-10-09:05:36:44 : datetime.
~~~

Both of these are subtypes of a more generic type `period` used for more general periods of time, using natural extensions of the iso 8601 format:

~~~
  '2019-10 : period.
  '2020    : period.
~~~

Note: The ' here is to disambiguate between time periods and numbers.

Being able to work with such datatypes simply and naturally is one of the things which is 
important when working with bedelibry. For example, when making a query about a remark that was made
at a certain time.

In the future, we may also implement binary operators for operating on periods, for example:
~~~
  2019-10 \/ 2019-11 \/ 2019-12 : period.
~~~

Aliases
-------

In bedelibry prolog, aliases can be used to provide different names to
pre-existing relations, types, and entities. For example:

~~~
  %
  % aliases.bpl
  %

  using equality.

  person: type.
  nate: person.
  
  alias me nate.

  equals(me, nate).
    True.
~~~

In the bedelibry prolog REPL, new aliases can be added by invoking the `:alias` command.

Datatypes
---------

Bedelibry has support for custom datatypes by allowing the user to define their own constructors, and assign these constructors to types. For example, important for use in the rest of bedelibry is the url datatype:

~~~
  datatype url with
    constructor 'Url: string.
~~~

Which can be used as follows:

~~~
  'Url("http://github.com/Sintrases/bli-prolog").
~~~

This can also be used to construct enums in bedelibry prolog, for example:
~~~
  datatype bool with
    constructor 'True.
    constructor 'False.
~~~

Refinement Types
----------------

Note: I may use a different syntax for this later.

~~~
type my_type = my_type2(X), p(X).
~~~

Assertions
----------
Assertions in bli prolog are made by terminating queries with a "!" instead of a ".". This works for both rules and simple facts.
~~~
?- siblings(X,Y) :- child(X,Z), child(Y,Z)!
?- child(bob,  susan)!
?- child(mark, susan)!
?- siblings(bob, mark).
True.
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
  False.
  ?- programming_language(nim)!
  OK. Added entity "nim" to list of programming languages.
  ?- programming_language(nim).
  True.
~~~
However, if we try to assert this again:
~~~
  ?- programming_language(nim)!
  FAIL. Entity "nim" already exists in the configured server.
~~~

Note however, that in bli prolog, the only side effects that can occur are when *asserting* predicates. However, it is possible that more side effects can occur than purely asserting some predicate. For example, asserting a new entity of a given type also has the side effect of storing this assertion on the bedelibry server, if it has been configured to run with bli prolog.

Note that if the user tries to query an entity (which is different from a nullary predicate), then the output from bedelibry prolog is slightly different.
~~~
  ?- programming_language(nim).
  True.
  ? nim.
  OK. "nim" is an entity of type "programming_language".
~~~

Note: We may consider adding an `io` or `proc` type in the future to allow for more general side-effects that the user can configure to run with Haskell.

First-class rules
-----------------

Note: This is an experimental feature. The following is only an example
of how this might work in the future, if implemented.

~~~prolog
  %
  % first_class_rules.bpl
  %

  % Brings with_rule into score, enables
  % the use of first-class rules.
  using firstclass_rules.

  % brings bag_of into scope, enables working with lists.
  using lists.

  type programming_langauge.
  type person.
  rel cool: programming_language.
  rel functional: programming_language.
  rel thinks: person, rule.
  rel according_to: person, prop.
    
  nate: person.
  haskell: programming_language.
  functional(haskell).
  
  thinks(nate, 
    cool(X) :-
      functional(X), programming_language(X).
  ).
  
  % Here, with_rules is a special predicate which attempts to 
  % solve the predicate P with respect to the rules in the list Rules.
  according_to(Person, P)
    :- with_rules(Rules, P),
       bagof(\Rule. thinks(Person, Rule), Rules).

  according_to(nate, cool(haskell)).
    True.
~~~

Metainterpreters
----------------

The features of bedelibry prolog makes it easy and intuitive to make meta-interpreters for bedelibry prolog. The most basic metainterpreter is the built-in `interp`, which can be thought of as:

~~~
proc interp.
~~~

`interp` prompts the user to make a query, and displays the result the same way
they would be displayed in the blipl repl. In other words, `interp` gives the user
first-class access to the `?- ` query prompt.

More advanced than this is `minterp`, which is a procedure which
takes a higher-order predicate as input. For example in the example above,
`according_to(nate)` is a higher-order predicate. `minterp` is declared as follows:

~~~
proc minterp: pred -> pred.
~~~

To see how this works, look at the following example:

~~~

using interpreters.
using first_class_rules.

?- {
  minterp(according_to(nate)).
}

%
% (output)
%
% ?- cool(haskell).
%   > True.
%
~~~

More advanced meta-interpreters can also be constructed with generics:

~~~
proc minterp_with_result: pred -> pred, string -> Res.
proc abstract_minterp: pred -> pred, Req -> Res.
~~~

These functions allow us to construct metainterpreters that do more than simply read from
user input and print queries to the screen. We can also have more general input and return
types.

Note: To have a truly advanced metainterpreter that is able to have
      meaningful interactions with the user, we need to also allow
      for statefulness. 
      
      Rather than using something like the state monad, we should have a
      more user-friendly approach, while still keeping the state declared in the type.
      
      This should work something more like inheriting from the App class in Scala bringing
      in `args` into scope. Except rather than using traditional OOP, we use an approach
      amenable to our logical/functional paradigm.

Structural subtyping
--------------------

Structural subtyping in Bli Prolog can be used both with entitiy types, and datatypes.

Unlike in Haskell, where constructors have to belong to a unique algebraic data type, in Bli Prolog, the type of a constructor can be inferred from context, or specifically stated with type annotations. For example, compare the following, which is invalid haskell:

~~~haskell
  -- Ok
  data MyType1 = A | B | C
 
  -- Will not compile with MyType1 in the same scope
  data MyType2 = A | B | C | D
~~~

with the following, which is valid Bli Prolog:

~~~prolog
  % Ok.
  datatype my_type_1 where
    constructor 'A
    constructor 'B
    constructor 'C

  % All good!
  datatype my_type_2 where
    constructor 'A
    constructor 'B
    constructor 'C
    constructor 'D
~~~

If enabled, Bli Prolog will automatically deduce from the above that my_type_1 is a subtype of my_type_2, which we write `my_type_1 <: my_type_2`, which means that in any context (for instance, a relation) in which we expect a term of type `my_type_2`, we can also supply a term of type `my_type_1`.

Structural subtyping can also be enabled for entity types. For instance:

~~~ prolog
  structural type duck with
      proc quack: duck.
      proc walk: duck.
  
  type unidentified_entity.
  
  proc quack: unidentified_entity.
  proc walk: unidentified_entity.
  
  bob: unidentified_entity.
  
  ?- { duck(bob). }
    > True.
~~~

Note: This feature is similar to typeclasses in Haskell, and what are sometimes called traits or interfaces in other languages.

Command line interface
=========

Once installed, you may run `bli-prolog --help` to get a help screen explaining the usage of bli-prolog's command line arguments:

~~~
bli-prolog interpreter v0.3, (C) Nathan Bedell 2019

options [OPTIONS] [GOALSTRING]

Common flags:
     --search=SEARCH    Specify wether to use DFS, BFS, or Limited
     --program=FILE     Prolog file with clauses
     --schema=ITEM      Schema file
  -l --limit=INT        Limit the number of solutions found
  -d --depth=INT        Maximum depth to traverse when using limited search
  -v --verbose          Specify whether or not to use verbose output (on by
                        default)
  -j --json             Specify whether or not json output formatting is used
                        for queries.
     --server           Starts a REST server for processing bli prolog
                        queries if set.
     --port=INT         Port number to start the server.
  -? --help             Display help message
  -V --version          Print version information
     --numeric-version  Print just the version number
~~~

If no goalstring is supplied, bli-prolog will run as a REPL with the following prompt:
~~~

  |      |            |
  |      |  .         |
  |---|  |     |---|  |
  |   |  |  |  |   |  |
  |---|  |  |  |---|  |
               |
               |
bli-prolog interpreter v0.3, (C) Nathan Bedell 2019
Type ":h" for help, or ":exit" to quit.
?- 
~~~

EDSL
----

Through the use of the quasiquoters found in `Data.Prolog.TemplateHaskell`, bli prolog can also be used as an EDSL in Haskell. The easiest way to do this is via the `query` function
~~~
query :: Bli (BliTerm (Query a)) -> Bli (ToHsData a)
~~~
which uses the type family `ToHsData` to convert from the internal representation of Bli terms to a native type in Haskell. For instance: `[bli| name(haskell, X). |]` has type `Bli (BliTerm (Query StringLiteral))`, and `query [bli| name(haskell, X). |]` has type `String`.

Haskell Interop
---------------

Bedelibry prolog can also provide Haskell interoperability going the other way. To do this, the user can make a declaration as follows:

~~~
  extern my_haskell_function: string -> proc in MyHaskellModule.
~~~

which will look for a Haskell function `my_haskell_function :: String -> IO ()` in `MyHaskellModule`.

Todo
----

See [todo list](todo.md).