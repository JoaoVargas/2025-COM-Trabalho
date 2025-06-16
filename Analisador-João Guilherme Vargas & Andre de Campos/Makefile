all: parser

parser: Lexer.hs Parser.hs
	ghc --make src/Main.hs -o compilador -v

Lexer.hs:
	alex src/Lexer.x

Parser.hs:
	happy src/Parser.y

clean:
	rm -f src/*.hi src/*.o src/*.info src/Lexer.hs src/Parser.hs compilador

test: parser
	./compilador < test/TestCases/test1.txt