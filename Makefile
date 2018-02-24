# GlyphWiki Font Generator -*- mode: Makefile -*-

FONTS	= A B C ExA1 ExA2 ExB ExC I

.PHONY: all install clean otf pdf proof

all: pdf proof

otf:
	for font in $(FONTS) ; do \
		$(MAKE) -f Makefile_HanaMin sub=$$font otf; \
	done

pdf:
	for font in $(FONTS) ; do \
		$(MAKE) -f Makefile_HanaMin sub=$$font pdf; \
	done

proof:
	for font in $(FONTS) ; do \
		$(MAKE) -f Makefile_HanaMin sub=$$font proof; \
	done

clean:
	for font in $(FONTS) ; do \
		$(MAKE) -f Makefile_HanaMin sub=$$font clean; \
	done

distclean:
	-rm -rf HanaMin*
	for font in $(FONTS) ; do \
		$(MAKE) -f Makefile_HanaMin sub=$$font distclean; \
	done
