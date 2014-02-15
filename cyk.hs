import Data.Array -- for Array
import Data.List -- for nub

{-
Our CFG does not produce \epsilon, and we assume it is in Chomsky Normal Form
to begin with. Hence our only productions are of the form
A -> B C, a nonterminal to two nonterminals
and
A -> a, a nonterminal to a terminal.
-}
data Production = NTprod String String String | Tprod String String

{-
A CFG is just a straightforward list representation.
-}
type CFG = [Production]

{-
A helper function: get the left-hand side symbol of a production.
-}
prodLHS (NTprod a _ _) = a
prodLHS (Tprod a _) = a

{-
Given a RHS of a nonterminal production (i.e., the B C in A -> B C),
output all the As which produce that RHS.
-}
ntGens :: CFG -> (String, String) -> [String]
ntGens cfg (a, b) = map prodLHS (filter filterFunc cfg)
    where filterFunc (NTprod _ x y) = x == a && y == b
          filterFunc _ = False

{-
Given a terminal (e.g. a), output all nonterminals which directly
produce that terminal.
-}
termGens :: CFG -> String -> [String]
termGens cfg b = map prodLHS (filter filterFunc cfg)
    where filterFunc (Tprod _ y) = y == b
          filterFunc _ = False

{-
OK, everything above is more-or-less "standard" CFG functions,
there's nothing particularly natural to implementing CYK.
Here's the full algorithm!
-}


{- here's our dynamic programming -}
cykMatrix :: CFG -> String -> Array (Int, Int) [String]
cykMatrix cfg s =
    let termGens' = termGens cfg
        ntGens' = ntGens cfg
        n = length s
        m = array ((0,0), (n-1,n-1))
                ([((i,i), termGens' [s!!i])     | i <- [0..n-1]] ++  -- base case
                 [((i,j), ["deleted"]) | i <- [0..n-1], j <- [0..i-1]] ++ -- trash, but we need a fully defined array (???)
                 [((r, r+l), generators r l) | l <- [1..n-1], r <- [0..n-l-1]]) -- The main recursion! See "generators" in "where" clause.
                    {- This is the magic function: observe the list comprehension! -}
                    where generators :: Int -> Int -> [String]
                          generators s l =
                            nub $ concat [ntGens' (a,b) | t <- [0..l-1], a <- m!(s,s+t), b <- m!(s+t+1,s+l)]
                            -- fix t: the innner list is the cross-prod of m!(s,s+t) and m!(s+t+1,s+l)
                            -- we just combine all the for loops there. We have a big list of potential RHS productions.
    in m


{- with all dynamic programming, the magic is in the array.
   Now we just do our final step: -}
cyk cfg s = let n = length s in
    "S" `elem` cykMatrix cfg s !(0,n-1)

{-
A very simple grammar, in CNF form.
-}
exampleCFG :: CFG
exampleCFG = [NTprod "S" "A" "B",
              NTprod "A" "A" "A",
              Tprod "A" "a",
              NTprod "B" "B" "B",
--             (NTprod "B" "A" "A"),
              Tprod "B" "b"]


main = print (cyk exampleCFG "aaabb")
