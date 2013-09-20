-- Who --

Philipp Hausmann,      4003373, p.hausmann@students.uu.nl
Laurens van den Brink, 4022831, l.vandenbrink1@students.uu.nl


-- What's Where --
2.5 -> src/Afp/As1.hs
7.1 -> src/Afp/As1.hs
8.1 -> See "criterion.html", "heap_simple.ps", "heap_tree.ps", Haddock Documentation
9.1 -> see Haddock documentation


-- Building --

$ cabal configure --enable-benchmarks --enable-tests
$ cabal build

-- Running the criterion benchmak --
cabal bench --benchmark-options="-o bench.html"

-- Running heap benchmark --
$ cabal configure --enable-executable-profiling


-- References --

Cabal testin setup:
http://lambda.jstolarek.com/2012/10/code-testing-in-haskell/

Cabal benchmark setup:
http://lambda.jstolarek.com/2012/10/code-benchmarking-in-haskell/
