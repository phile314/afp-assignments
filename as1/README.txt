-- What's Where --
2.5 -> src/Afp/As1.hs
7.1 -> src/Afp/As1.hs
8.1 -> 
9.1 -> see Haddock documentation


-- Building --

$ cabal configure --enable-benchmarks --enable-tests
$ cabal build

-- Running the criterion benchmak --
cabal bench --benchmark-options="-o bench.html"



-- References --

Cabal testin setup:
http://lambda.jstolarek.com/2012/10/code-testing-in-haskell/

Cabal benchmark setup:
http://lambda.jstolarek.com/2012/10/code-benchmarking-in-haskell/
