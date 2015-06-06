all:
	cabal sandbox init
	cabal install

examples:
	cabal install -fexamples
