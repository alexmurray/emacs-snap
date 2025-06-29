.PHONY: variant.h

variant.h: variant.h.in
	sed -e 's/@VARIANT@/"$(shell git branch --show-current || echo unknown)"/' < variant.h.in > variant.h

setup-env: setup-env.c variant.h
	gcc setup-env.c -o setup-env

install: setup-env
	install -m 755 setup-env $(DESTDIR)/
