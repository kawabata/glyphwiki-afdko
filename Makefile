# GlyphWiki Font Generator -*- mode: Makefile -*-

.PHONY: all A B AX BX install clean

all: otf pdf proof

otf: HanaMinA.otf HanaMinB.otf HanaMinC.otf HanaMinUX.otf HanaMinAX.otf HanaMinBX.otf HanaMinCX.otf HanaMinI.otf
pdf: HanaMinA.pdf HanaMinB.pdf HanaMinC.pdf HanaMinUX.pdf HanaMinAX.pdf HanaMinBX.pdf HanaMinCX.pdf HanaMinI.pdf
proof: HanaMinA.proof.pdf HanaMinB.proof.pdf HanaMinC.proof.pdf HanaMinUX.proof.pdf HanaMinAX.proof.pdf HanaMinBX.proof.pdf HanaMinCX.proof.pdf HanaMinI.proof.pdf

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

HanaMinI.otf:
	make -f Makefile_HanaMin sub=I otf

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
