test:
	racket tests/test.rkt

reinstall:
	raco pkg remove dssl
	raco pkg install
