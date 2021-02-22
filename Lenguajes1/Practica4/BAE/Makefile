run : compile
	./BAEi ./demo/Factorial.bae

_mkBuildDir :
	mkdir -p build

compile : _mkBuildDir
	ghc src/Main.hs -isrc -outputdir build/ -o BAEi

clean :
	rm -f -r build
	rm -f BAEi
