all: simplify-bkg

clean:
	rm -rf simplify-bkg *.o *.hi

simplify-bkg:
	ghc -o simplify-bkg Main.hs
