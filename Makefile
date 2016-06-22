test:
	racket tests/test.rkt

doc: dssl.scrbl
	raco scribble --dest $@ $^

uninstall:
	raco pkg remove dssl

install:
	raco pkg install

reinstall:
	$(MAKE) uninstall
	$(MAKE) install
