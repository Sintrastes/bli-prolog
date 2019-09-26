
Todo
====

  * Implement schemas with types.
  * Implement configuration with bedelibry server, using data from ~/.bedelibry/config.yaml by default. (same as with bli tool)
    * Implement side-effects with asserting types. (first need to configure the entity server).
     * Note: I think that for assertions that produce a side effect (including adding a new entity
             to the schema -- either in memory, or part of the bedelibry server -- we should use
             an alternative syntax for assertions. Maybe !!. So, e.x. person(nate)!! will add a new
             entity to the schema of type person.
  * Integrate implicit predication with REPL.
     * Note: I think we'll probably want to use an alternative syntax for this.
       While we could keep the behavior of plain lambda queries, we could also 
       allow for the application of lambda queries (predicates) to terms.
       Although, we may want to consider using different binders for this.
  * Implement the :export and :load commands for the REPL.
  * Fix error messages in the repl for using a predicate with invalid arity.