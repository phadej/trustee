all :  build

build : 
	cabal new-build -w ghc-8.4.4

install : build
	cp `cabal-plan list-bin trustee` ~/.local/bin

install-tools :
	rm -rf ${HOME}/.local/bin/cabal-plan
	rm -rf ${HOME}/.local/bin/alex
	rm -rf ${HOME}/.local/bin/happy
	cabal new-install cabal-plan --symlink-bindir ${HOME}/.local/bin
	cabal new-install alex --symlink-bindir ${HOME}/.local/bin
	cabal new-install happy --symlink-bindir ${HOME}/.local/bin
