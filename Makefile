SUBDIRS = libs sulci

.PHONY: subdirs $(SUBDIRS) clean

subdirs: $(SUBDIRS)

$(SUBDIRS):
	echo "export OCAMLPATH=$(shell pwd)/site-lib" > Makefile.global
	$(MAKE) -C $@

greybottle: libs greybottle
	$(MAKE) -C $@

clean:
	for dir in $(SUBDIRS) greybottle; do \
		$(MAKE) -C $$dir clean; \
	done

