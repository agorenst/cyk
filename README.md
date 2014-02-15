cyk
===

A simple C++ implementation of the CYK algorithm.

This is partly for fun, partly to try out some nice C++11 features (though I'm stuck on G++ 4.4, so I don't have them all), and partly for education.
Working examples of algorithms are really fun to play with, to understand how the algorithm works.

Note: the CFG file *must* be in CNF form already: we do not check otherwise.
Also note the very basic format of the CFG file: first symbol of the line is the nonterminal, and the next symbols are the RHS of the production.
Terminals are exactly those symbols with no LHS.
But there is no strict enforcement: thus, it may be possible to have nonterminals in a "final" string.
One day all this may be addressed.

Current plans: 
1) put in a parse tree extraction method.
2) Make a smarter dyn prog table class: right now it is a big mess of vectors and such.
3) Divide up files so they are more modular.


Recently I also added a Haskell implementation.
It differs substantively from what I found on the internet (though perhaps I simply missed similar implementations).
Rather than being super-Haskell-y, this code tries to succinctly express the same algorithm using some of the syntactic features of Haskell, namely recursive array declarations and list comprehensions.
