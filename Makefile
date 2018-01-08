all :  build

build : 
	cabal new-build -w ghc-8.2.2

install : build
	cp `cabal-plan list-bin trustee` ~/.local/bin
