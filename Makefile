all: simplify-bkg

clean:
	rm -rf simplify-bkg
	rm -rf src/*.o src/*.hi

simplify-bkg:
	ghc -isrc -o simplify-bkg src/Main.hs
