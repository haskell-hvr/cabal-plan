all :
	cabal new-build all

ghcid-exe :
	ghcid -c 'cabal new-repl cabal-plan:exe:cabal-plan'

diff-demo :
	cabal new-run cabal-plan -- diff --plan-json=fixtures/lens-4.17-ghc-7.8.json --plan-json=fixtures/lens-4.17-ghc-8.4.json

.PHONY: tags

tags :
	hasktags -c src src-exe
