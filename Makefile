all:
	ghc --make -O3 Main.hs -o Main
	./Main
	./make_pdf.rb
install:
	cabal update
	cabal install bytestring numeric-prelude typelevel-tensor process-qq