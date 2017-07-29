# GlyphWiki Font Generator -*- mode: Makefile -*-

spec_A	= '^u00[2-7][0-9a-f]$$$$:^u[2-9f][0-9a-f]{3}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[ghjktuv]))?(-vert)?$$$$:^cdp-....$$$$'
spec_B	= '^u00[2-7][0-9a-f]$$$$:^u30[0-9a-f]{2}(-vert)?$$$$:^u2[0-9a-f]{4}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[ghjktuv]))?$$$$'

spec_AX	= '^u00[2-7][0-9a-f]$$$$:^u[2-9f][0-9a-f]{3}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[ghjktuv])?([01][0-9])?)?(-(var|itaiji)-[0-9]+)?(-vert)?$$$$:^kumimoji-u2ff[0-9ab](-u[0-9a-f]{4,5}){2,3}(-(j[av]|kp|us|[ghjktuv])?([01][0-9])?)?(-(var|itaiji)-[0-9]+)?(-vert)?$$$$:^cdp-....$$$$'
spec_BX	= '^u00[2-7][0-9a-f]$$$$:^u2[0-9a-f]{4}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[ghjktuv])?([01][0-9])?)?(-(var|itaiji)-[0-9]+)?(-vert)?$$$$'

version = $(shell date "+%y.%m%d" | cut -c2-6)

.PHONY: all A B AX BX install clean

all: A B
# all: A B AX BX
A: HanaMinA.otf
B: HanaMinB.otf
AX: HanaMinAX.otf
BX: HanaMinBX.otf

HanaMinA.otf:
	make -f Makefile_HanaMin sub=A spec=$(spec_A) otf

HanaMinB.otf:
	make -f Makefile_HanaMin sub=B spec=$(spec_B) otf

HanaMinAX.otf:
	make -f Makefile_HanaMin sub=AX spec=$(spec_AX) otf

HanaMinBX.otf:
	make -f Makefile_HanaMin sub=BX spec=$(spec_BX) otf

install: all
	zip HanaMinAFDKO-$(version).zip ~/cvs/HanaMinAFDKO/README.md HanaMinA.otf HanaMinB.otf
	zip HanaMinExtAFDKO-$(version).zip ~/cvs/HanaMinAFDKO/README.md HanaMinAX.otf HanaMinBX.otf

clean:
	-make -f Makefile_HanaMin sub=A clean
	-make -f Makefile_HanaMin sub=B clean
	-make -f Makefile_HanaMin sub=AX clean
	-make -f Makefile_HanaMin sub=BX clean

distclean:
	-make -f Makefile_HanaMin sub=A distclean
	-make -f Makefile_HanaMin sub=B distclean
	-make -f Makefile_HanaMin sub=AX distclean
	-make -f Makefile_HanaMin sub=BX distclean
	-rm *.zip
