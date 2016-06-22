test:
	racket tests/test.rkt

doc: dssl.scrbl
	raco scribble --dest $@ $^

reinstall:
	raco pkg remove dssl
	raco pkg install
