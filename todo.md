
Todo
====
  * Fix the issue in parsing data declarations where constructors with
    arguments have to be declared after constructors without arguments.
  * Implement typechecking of predicates.
  * Implement a system for parsing infix operators with different infix declarations.
  * Improve the module system by adding "module X." and "module X exporting ... . declarations at the top of files, and make it so that a file only parses if is a properly declared module, or if it contains some executable code at the bottom of the file.
  * Refactor compiler code.
  * Refactor RelDecl, EntityDecl, etc... to be newtypes, and
        make them the arguments to the constructors of other datatypes.
  * Make the compiler check that a file in fact contains
    exectuable code before attempting to compile a .bpl file. (i.e. check to
    see if it is infact a schema or not).
  * Make sure that typechecking works properly with the module system.
  * Make our rel syntax just syntatic sugar for our more general functional syntax.
  * Implement scoped assertions.
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
    
Notes
-----
    * Note: Once we implement better Haskell interop (in the direction of using Bli Prolog in Haskell),
      it might be fun to implement some small part of the Bli Prolog runtime in Bli Prolog -- for instance, subtypes could be implemented using higher-order predicates in bedelibry-prolog.
    * Note that for content-addressable entities (i.e. their entity is determined by their content, not their name), we could use something else, like 'ContentOf('Url("file:/home/nate/pdfs/my.pdf")).
  * In the future, I might allow users to make multiple schema declarations, and assertions on the same line, for brevity.
  * I think we'll probably want to use an alternative syntax for implicit predication.
    While we could keep the behavior of plain lambda queries, we could also 
    allow for the application of lambda queries (predicates) to terms.
  * Note: There are several different strategies that we could use for parsing
    bli prolog files -- i.e. do we want to have a declaration before use
    policy, do we require entity declarations, relation declarations, etc... to be at the
    top of the file? 
      * I think what I want to (ideally) do here is to use the most
        lax parsing strategy possible, but allow the user to supply flags
        that allow for different parsing strategies (which might be more efficent
        for larger files).
  * It might be fun to add fuzzy string matching support.
  * Note: It would be fun to do something with unicode operators ♢ and ☐. For instance, we could
    allow these as predicates.
       * However, we can also use ♢ to encode important facts about subtyping relations.
         For instance, ♢(person <: animal) "It is possible that person is a subtype of animal."
         encodes the ambiguity that although *strictly speaking*, people are animals,
         usually people use *person* in the strict sense.
       * Idea: We can refer to a proposition/type by:
           modal_proposition :: ♢(person <: animal).
         And then later we can declare exactly in what contexts 
         the possible worlds semantics of this proposition may be resolved.