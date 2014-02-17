\documentclass{article}
\author{Aaron Gorenstein}
\date{February 15, 2014}
\title{A Haskell Implementation of the CYK Algorithm}

\usepackage{fullpage}
\usepackage[usenames,dvipsnames]{color}

% copied from http://www.haskell.org/haskellwiki/Literate_programming
\usepackage{listings}
\lstloadlanguages{Haskell}
\lstnewenvironment{code}
{\lstset{}%
   \csname lst@SetFirstLabel\endcsname}
{\csname lst@SaveFirstLabel\endcsname}
\lstset{
    language=Haskell,
    numbers=left,
      basicstyle=\small\ttfamily,
      flexiblecolumns=false,
      basewidth={0.5em,0.45em},
      literate={+}{{$+$}}1 {/}{{$/$}}1 {*}{{$*$}}1 {=}{{$=$}}1
      {>}{{$>$}}1 {<}{{$<$}}1 {\\}{{$\lambda$}}1
      {\\\\}{{\char`\\\char`\\}}1
      {->}{{$\rightarrow$}}2 {>=}{{$\geq$}}2 {<-}{{$\leftarrow$}}2
      {<=}{{$\leq$}}2 {=>}{{$\Rightarrow$}}2 
      {\ .}{{$\circ$}}2 {\ .\ }{{$\circ$}}2
      {>>}{{>>}}2 {>>=}{{>>=}}2
      {|}{{$\mid$}}1 ,
      commentstyle=\color{ProcessBlue},
      keywordstyle=\color{OliveGreen}
}

\begin{document}
\maketitle

This is a brief ``literate program'' written in Haskell implementing the CYK algorithm.
I assume the reader is already familiar with the CYK algorithm.
My implementation is very simplistic---this is unavoidable as I am a Haskell novice.
This has not deterred me, because my target audience is other novices!
In particular, I'm eager to share what I found to be the ``natural'' way of expressing the CYK algorithm in Haskell, especially in comparison to my C++ implementation.
In many ways it looks quite similar, but it seems to more faithfully reflect the recursive structure our dynamic algorithm counts on.

There are two main sections in this document.
The first details the ``foundation''.
There we define how to express a CFG that is in CNF, and some reverse-lookup functions.
That is, for a string of symbols $\alpha$ in the gammar $G$, is there a production in $G$ of the form $A\to \alpha$?
If so, return all such $A$.
The second section focuses entirely on the main CYK algorithm, and focuses on explaining how our Haskell code reflects the ``imperative'' definition as found in the C++ version.
A final epilogue simply shows a very simple instatiation of the algorithm on a grammar and input string.

Without further ado:
\section{Prologue: Building the Foundation}
Our code begins in a very staid manner.
\begin{code}
import Data.Array -- for Array
import Data.List  -- for nub
\end{code}
The function \texttt{nub} takes a list and removes redundant elements.
For instance, the list $[1,2,3,2,3]$ is transformed to $[1,2,3]$.

This code assumes that the grammar is in Chomsky Normal form, the definition
of which I will not address here---it's very standard.
Regardless, we're able to define our grammar productions as having exactly two
forms (tuples, really), as we know that each production involves exactly 2 or 3 symbols.

\begin{code}
data Production = NTprod String String String | Tprod String String
type CFG = [Production]
\end{code}

The datatype \texttt{Production} is either a \emph{nonterminal} production (\texttt{NTprod})
of the form $A\to BC$ (observe that the right-hand-side is made of \emph{non}terminals),
or a \emph{terminal} (\texttt{Tprod}) production, of the form $A\to a$, where $a$ is a \emph{terminal}.
We specify these options very concretely with Tuples of the associated string representations.

A context-free grammar is simply a collection of these productions,
hence the na\"{i}ve definition in list form in line 2 of the above code snippet.
Observe that many things about the grammar are not checked (that the start symbol is not
used recursively, for instance).

With our grammar object defined, the only remaining feature we want to define is the ``reverse-lookup'': given a string of symbols from the gammar $\alpha$,
return any nonterminal $A$ such that the production $A\to\alpha$ exists.
As we know our grammar is in CNF, there are exactly two cases: either $\alpha$ is two nonterminals, or $\alpha$ is a single terminal.
To facilitate implementation, first we want to be able to, given a \texttt{Production} of either sort, extract its left-hand-side.
\begin{code}
prodLHS (NTprod a _ _) = a
prodLHS (Tprod a _) = a
\end{code}
This is an obvious utility.

Now we can do the reverse lookup for the two cases.
They're obviously very similar.
Open question: is there a nicer way of implementing this, perhaps as a single function?
Regardless, here is the $A\to BC$ lookup (given $BC$, find all $A$ such that $A\to BC$):
\begin{code}
ntGens :: CFG -> (String, String) -> [String]
ntGens cfg (a, b) = map prodLHS (filter filterFunc cfg)
    where filterFunc (NTprod _ x y) = x == a && y == b
          filterFunc _ = False
\end{code}
Here is the $A\to a$ lookup (given $a$, find all $A$ such that $A\to a$).
\begin{code}
termGens :: CFG -> String -> [String]
termGens cfg b = map prodLHS (filter filterFunc cfg)
    where filterFunc (Tprod _ y) = y == b
          filterFunc _ = False
\end{code}

Those are the only data types and functions we need!
We are now ready to define our main algorithm.

\section{The Main Algorithm}
Let's jump right into the main algorithm, and we shall explain it line-by-line:
\begin{code}
cykMatrix :: CFG -> String -> Array (Int, Int) [String]
cykMatrix cfg s =
    let termGens' = termGens cfg
        ntGens' = ntGens cfg
        n = length s
        m = array ((0,0), (n-1,n-1))
                ([((i,i), termGens' [s!!i])     | i <- [0..n-1]] ++ 
                 [((r, r+l), generators r l) | l <- [1..n-1], r <- [0..n-l-1]])
                    where generators :: Int -> Int -> [String]
                          generators r l =
                            nub $ concat [ntGens' (a,b) | t <- [0..l-1],
                                                          a <- m!(r,r+t),
                                                          b <- m!(r+t+1,r+l)]
    in m
\end{code}
Lines 1-6 are just setting up the function---the real magic starts afterwards.
Recall from the CYK algorithm design that $m!(i,j)$ is the set of all nonterminals which generates the substring of $s$ starting at index $i$ and ending at $j$.
The base case, as shown on line 7, follows naturally: at $m!(i,i)$, the nonterminals are exactly those which generate the single terminal found at location $i$ in $s$.
(The curious $[s!!i]$ notation is simply because $s!!i$ is a character, and for type-safety we have to ``promote'' back to a String type, which we do with the $[]$ notation.)
On line 8, we say more-or-less the same thing, conceptually, but in the general case.

Thus, it is entirely the list-comprehension beginning on line 11 where the ``magic'' happens.
It states:
The nonterminals which can generate the substring from index $r$ to $r+\ell$ are exactly those $C$ involved in the production $C\to AB$, where $AB$ are represented by $(a,b)$ in the \texttt{ntGens'} call.
Moreover, for any $a,b$, it must be that $a$ generates some substring-prefix $s[r,r+t]$ and $b$ generates the corresponding suffix $s[r+t+1,r+\ell]$.
The values $t$ can exist between $0$ and $\ell-1$.
Quite a lot in packed into that single line!

I hope that was somewhat comprehensible.
Note that compared with the C++ version, that single list comprehension replaces 3 \texttt{for} loops!
Most importantly, I think the list comprehension better expresses the relationships between those three integers $r,t,l$.
Hooray!

\section{Epilogue: A Small Execution}
That was the major part of the work.
At this point we'll simply present a hard-coded example.
In the future this code may be extended to actually take things from input files and so forth, but for now this is just a quick test.

First, we use the matrix to actually compute our decision algorithm:
\begin{code}
cyk cfg s = let n = length s in
    "S" `elem` cykMatrix cfg s !(0,n-1)
\end{code}

Now let's define a very simple grammar:
\begin{code}
exampleCFG :: CFG
exampleCFG = [NTprod "S" "A" "B",
              NTprod "A" "A" "A",
              Tprod "A" "a",
              NTprod "B" "B" "B",
--             (NTprod "B" "A" "A"),
              Tprod "B" "b"]
\end{code}

And here is the easy way of using our CYK algorithm!
\begin{code}
main = print (cyk exampleCFG "aaabb")
\end{code}

Thank you for reading, I hope this has helped you understand something about Haskell or the CYK algorithm.
I also hope I have been able to share my excitement and enthusiasm with how different programming languages can sometimes express things in intellectually pleasing contrasting ways.

\end{document}
