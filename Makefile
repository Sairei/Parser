ENV=environnement
TEST=tests

all: $(ENV)
	
environnement:
	ghc EmvInteractif.hs
	
tests:
	ghc ExpressionTest.hs
	
clean:
	rm *.o *.hi EmvInteractif
