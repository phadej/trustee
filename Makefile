all :
	cabal new-build -w ghc-8.2.2
	cp `cabal-plan list-bin trustee` ~/.local/bin
