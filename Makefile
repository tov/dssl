test:
	racket tests/test.rkt

doc: dssl.scrbl
	raco scribble --dest $@ $^
	echo "<meta http-equiv='refresh' content='0;url=dssl.html'>" > doc/index.html

uninstall:
	raco pkg remove dssl

install:
	raco pkg install

reinstall:
	$(MAKE) uninstall
	$(MAKE) install

upload-doc:
	$(MAKE) doc
	ghp-import -n doc
	git push -f git@github.com:tov/dssl.git gh-pages
