
Todo
====
  * Implement schemas with types.
      * Refactor the Bli monads so that the set of types is stored seperately
        from the set of relation declarations.
  * Integrate implicit predication with REPL.
      * Write a contextual parser for implicit predications.
  * Implement side-effects with asserting types. (first need to configure the entity server).
     * Note: I think that for assertions that produce a side effect (including adding a new entity
             to the schema -- either in memory, or part of the bedelibry server -- we should use
             an alternative syntax for assertions. Maybe !!. So, e.x. person(nate)!! will add a new
             entity to the schema of type person.
  * Check to see if my use of the Bli monad with the warp server works,
    or if I need to use IORefs or something else to get it to work.
  * Refactor the main codebase to use the BliSet (or some related) interface.
  * Add test cases for parsing files so that I can work on refactoring some of our parsing code.
  * Refactor isBliCommandValid to avoid code duplication.

Notes
-----
  * Consider implementing string and list literals.
    * Note that we'll probably want to implement a system of literals that is also
      capable of dealing with data entities. E.x. 'Url("file:/home/nate/pdfs/my.pdf"). Where we use a tick
      to distinguish between variables and data entity constructors. 
    * Note that for content-addressable entities (i.e. their entity is determined by their content, not their name), we 
      could use something else, like 'ContentOf('Url("file:/home/nate/pdfs/my.pdf")).
  * In the future, I might allow users to make multiple schema declarations, and assertions on the same line, for brevity.
      * Related: Fix issue where data can be entered beyond the "." or the "!" in the repl.
  * I think we'll probably want to use an alternative syntax for implicit predication.
    While we could keep the behavior of plain lambda queries, we could also 
    allow for the application of lambda queries (predicates) to terms.
  * Note: "rel p: 2", or even "rel p arity 2" could by syntatic sugar for
    "rel p: entity, entity", where "entity" is a catchall type which typechecks with
    any input. Kind of like "Any" in some OOP languages.
  * It might be fun to add fuzzy string matching support.