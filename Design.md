
Design Notes
------------

Eventually I'd like to incorporate these notes into more complete documentation about Bedelibry Prolog,  but for now, this is a place that I can keep track of some of my ideas on the subject.

Influences
----------

Below is a list of programming languages which have inspired the design of
Bli Prolog in some way:

 * Prolog:
    * Logic programming
    * Syntax of the language.
    * Some built-in predicates such as `bagof`.
 * Lambda-Prolog: 
    * Syntax for type/relation declarations. (Bli Prolog's syntax was loosely inspired by this)
 * Haskell: 
    * Bedelibry Prolog uses the `$` operator for application of predicates.
    * Algebraic data types
    * Typeclasses
    * Syntax for partial application.
    * Strict seperation of pure code from IO actions.
 * Scala:
    * The behavior of classes such as "App" as an alternative to using StateT/ReaderT in Haskell
      as a solution to dependency injection.
    * Case classes, and the whole object-oriented design of Scala as being something like a 
      more flexible version of algebraic data types.
 * [Kitten](https://kittenlang.org/intro/):
   * I'd like to implement something similar to Kitten's system of *permissions*, which are, in turn, similar to
     Java's [checked exceptions](https://en.wikibooks.org/wiki/Java_Programming/Checked_Exceptions).