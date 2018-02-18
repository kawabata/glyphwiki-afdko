# GlyphWiki Font Generator -*- mode: Makefile -*-

.PHONY: all A B AX BX install clean otf pdf proof

all: otf pdf proof

otf:
	make -f Makefile_HanaMin sub=A otf
	make -f Makefile_HanaMin sub=B otf
	make -f Makefile_HanaMin sub=C otf
	make -f Makefile_HanaMin sub=UX otf
	make -f Makefile_HanaMin sub=AX otf
	make -f Makefile_HanaMin sub=BX otf
	make -f Makefile_HanaMin sub=CX otf
	make -f Makefile_HanaMin sub=I otf

proof:
	make -f Makefile_HanaMin sub=A proof
	make -f Makefile_HanaMin sub=B proof
	make -f Makefile_HanaMin sub=C proof
	make -f Makefile_HanaMin sub=UX proof
	make -f Makefile_HanaMin sub=AX proof
	make -f Makefile_HanaMin sub=BX proof
	make -f Makefile_HanaMin sub=CX proof
	make -f Makefile_HanaMin sub=I proof

pdf:
	make -f Makefile_HanaMin sub=A pdf
	make -f Makefile_HanaMin sub=B pdf
	make -f Makefile_HanaMin sub=C pdf
	make -f Makefile_HanaMin sub=UX pdf
	make -f Makefile_HanaMin sub=AX pdf
	make -f Makefile_HanaMin sub=BX pdf
	make -f Makefile_HanaMin sub=CX pdf
	make -f Makefile_HanaMin sub=I pdf

clean:
	-make -f Makefile_HanaMin sub=A clean
	-make -f Makefile_HanaMin sub=B clean
	-make -f Makefile_HanaMin sub=C clean
	-make -f Makefile_HanaMin sub=UX clean
	-make -f Makefile_HanaMin sub=AX clean
	-make -f Makefile_HanaMin sub=BX clean
	-make -f Makefile_HanaMin sub=CX clean
	-make -f Makefile_HanaMin sub=I clean

distclean:
	-make -f Makefile_HanaMin sub=A distclean
	-make -f Makefile_HanaMin sub=B distclean
	-make -f Makefile_HanaMin sub=C distclean
	-make -f Makefile_HanaMin sub=UX distclean
	-make -f Makefile_HanaMin sub=AX distclean
	-make -f Makefile_HanaMin sub=BX distclean
	-make -f Makefile_HanaMin sub=CX distclean
	-make -f Makefile_HanaMin sub=I distclean
	-rm *.zip
	-rm *.sfont
