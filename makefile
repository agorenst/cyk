all: cyk_haskell cyk_cpp pdf

cyk_haskell: cyk.lhs
	ghc cyk -o cyk_haskell

cyk_cpp: cyk.cpp
	g++ -std=c++0x -Wall cyk.cpp -o cyk_cpp

pdf:
	pdflatex cyk.lhs

clean:
	rm *~ *.o *.hi cyk_cpp cyk_haskell *.aux *.log
