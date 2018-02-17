# GlyphWiki Font Generator -*- mode: Makefile -*-

version = $(shell date "+%y.%m%d" | cut -c2-6)

.PHONY: all A B AX BX install clean

all: A B C
# all: A B AX BX
A: HanaMinA.otf
B: HanaMinB.otf
C: HanaMinC.otf
UX: HanaMinUX.otf
AX: HanaMinAX.otf
BX: HanaMinBX.otf
CX: HanaMinCX.otf

dump.tar.gz:
	wget http://glyphwiki.org/dump.tar.gz

dump_newest_only.txt: dump.tar.gz
	tar xvfz dump.tar.gz dump_newest_only.txt
	touch dump_all_versions.txt
	$(PERL) -i -pe 's/^( u319[-0-9a-z]*)(.*)$$/\1\2\n\1-vert\2/' dump_newest_only.txt

HanaMinA.otf:
	make -f Makefile_HanaMin sub=A otf

HanaMinB.otf:
	make -f Makefile_HanaMin sub=B otf

HanaMinC.otf:
	make -f Makefile_HanaMin sub=C otf

HanaMinUX.otf:
	make -f Makefile_HanaMin sub=UX otf

HanaMinAX.otf:
	make -f Makefile_HanaMin sub=AX otf

HanaMinBX.otf:
	make -f Makefile_HanaMin sub=BX otf

HanaMinCX.otf:
	make -f Makefile_HanaMin sub=CX otf

clean:
	-make -f Makefile_HanaMin sub=A clean
	-make -f Makefile_HanaMin sub=B clean
	-make -f Makefile_HanaMin sub=C clean
	-make -f Makefile_HanaMin sub=AX clean
	-make -f Makefile_HanaMin sub=BX clean

distclean:
	-make -f Makefile_HanaMin sub=A distclean
	-make -f Makefile_HanaMin sub=B distclean
	-make -f Makefile_HanaMin sub=C distclean
	-make -f Makefile_HanaMin sub=AX distclean
	-make -f Makefile_HanaMin sub=BX distclean
	-rm *.zip
