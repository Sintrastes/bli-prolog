
Todo
====
  * Make our rel syntax just syntatic sugar for our more general functional syntax.
  * Implement scoped assertions.
  * Implement a module system like in the example files.
     * The idea is pretty simple, a module is just an alias to a file somehwere 
       where there exists a bpl source file that we want to splice in at the import
       point. Thus, we can have a ~/.bedelibry/modules.yaml configuration file with the format:
          modules:
             - name: lists 
               file_path: ~/code/bedebliry/lists.bpl
              
             - name: people
               file_path: ~/code/bedelibry/lists.bpl
     * For fun: Implement a lazy module system, where modules are only used
       when needed in the file. 

  * Integrate implicit predication with REPL.
      * Write a contextual parser for implicit predications.
  * Implement side-effects with asserting types. (first need to configure the entity server).
     * Note: I think that for assertions that produce a side effect (including adding a new entity
             to the schema -- either in memory, or part of the bedelibry server -- we should use
             an alternative syntax for assertions. Maybe !!. So, e.x. person(nate)!! will add a new
             entity to the schema of type person.
  * Check to see if my use of the Bli monad with the warp server works,
    or if I need to use IORefs or something else to get it to work.
  * Add test cases for parsing files so that I can work on refactoring some of our parsing code.
  * Modify BliResult to take into account the type-specific errors that I want to consider.
  * Make sure the typed bli command parsers are taking into account reserved words (like type, entity, rel) correctly.
      * Done, but need to add test cases for this.
      * Also, this doesn't currently work correctly.
  * Modify our test cases to take into account the new parsers.
  * Add new test cases for the new BliResult options
  * Make :ls commands give a specific output when the relevant store is empty.
     * Done, but should add test cases for this.
  * Refactor the server code to use servant.
  * Fix the typechecking of rules.
  * Refactor Ast so Atom is a an ADT, not just an alias for String.
    
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
      * Note: We could also use the prolog rel likes/2. notation as syntatic sugar
        for rel likes: entity, entity.
  * Note: There are several different strategies that we could use for parsing
    bli prolog files -- i.e. do we want to have a declaration before use
    policy, do we require entity declarations, relation declarations, etc... to be at the
    top of the file? 
      * I think what I want to (ideally) do here is to use the most
        lax parsing strategy possible, but allow the user to supply flags
        that allow for different parsing strategies (which might be more efficent
        for larger files).
  * It might be fun to add fuzzy string matching support.