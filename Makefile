
GHC = ghc -outputdir /tmp -rtsopts -O2 -Wall -Werror Hajure.hs -o runHajure

all: forceCompile
	@echo "All done"

forceCompile:
	${GHC} -fforce-recomp

compile:
	${GHC}

internal_run:
	./runHajure +RTS -sstderr -RTS test.cl

run: compile internal_run

frun: forceCompile internal_run

profile: compile
	./runHajure +RTS -sstderr -hT -RTS test.cl

svg: profile
	hp2pretty runHajure.hp
	eog runHajure.svg

