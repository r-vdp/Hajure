
all: compile
	@echo "All done"

compile:
	ghc -outputdir /tmp -O2 -Wall -Werror -fforce-recomp Hajure.hs -o runHajure

