SUBDIRS = $(shell ocamlfind query cryptokit > /dev/null 2> /dev/null || echo cryptokit)
SUBDIRS += ini_config dehtml hex logger \
	   xml xmlstring xmlstring_netstring \
	   scheduler strftime xmpp

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)                                                        

$(SUBDIRS):
	$(MAKE) -C $@ rawinstall

clean:
	for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir clean; \
	done

