all: parser

parser: Lexer.hs Parser.hs Main.hs
	ghc --make Main.hs -o compilador

Lexer.hs: Lexer.x
	alex Lexer.x

Parser.hs: Parser.y
	happy Parser.y

clean:
	rm -f *.hi *.o *.info Lexer.hs Parser.hs compilador

test: parser
	./compilador < test/TestCases/test1.txt