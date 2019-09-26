
Todo
====

  * Implement assertions.
  * Implement schemas with types.
  * Implement configuration with bedelibry server, using data from ~/.bedelibry/config.yaml by default. (same as with bli tool)
    * Implement side-effects with asserting types. (first need to configure the entity server).
  * Integrate implicit predication with REPL.
  * Make sure bli prolog command parser does not allow the user to quantify over terms
    with free variables not in the body. (e.x. dissallow \X -> P(Y))