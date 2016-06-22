test:
	racket tests/test.rkt

doc
	raco scribble dssl.scrbl

reinstall:
	raco pkg remove dssl
	raco pkg install
