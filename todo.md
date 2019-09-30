
Todo
====
  * Implement schemas with types.
  * Implement side-effects with asserting types. (first need to configure the entity server).
     * Note: I think that for assertions that produce a side effect (including adding a new entity
             to the schema -- either in memory, or part of the bedelibry server -- we should use
             an alternative syntax for assertions. Maybe !!. So, e.x. person(nate)!! will add a new
             entity to the schema of type person.
  * Integrate implicit predication with REPL.
  * Check to see if my use of the Bli monad with the warp server works,
    or if I need to use IORefs or something else to get it to work.
  * Refactor the main codebase to use the BliSet (or some related) interface.
  * Add test cases for parsing files so that I can work on refactoring some of our parsing code.
  * Refactor isBliCommandValid to avoid code duplication.

Notes
-----

  * In the future, I might allow users to make multiple schema declarations, and assertions on the same line, for brevity.
      * Related: Fix issue where data can be entered beyond the "." or the "!" in the repl.
  * I think we'll probably want to use an alternative syntax for implicit predication.
    While we could keep the behavior of plain lambda queries, we could also 
    allow for the application of lambda queries (predicates) to terms.
  * It might be fun to add fuzzy string matching support.