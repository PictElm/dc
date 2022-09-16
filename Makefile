
all: Main.hs; ghc -O Main -o dc
clean:; $(RM) Main.hi Main.o dc
