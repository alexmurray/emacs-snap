setup-env: setup-env.c
	gcc -DVARIANT=\"$(shell git branch --show-current)\" setup-env.c -o setup-env

install: setup-env
	install -m 755 setup-env $(DESTDIR)/
