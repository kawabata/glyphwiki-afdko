# GlyphWiki Font Generator -*- mode: Makefile -*-

spec_A	= '^u00[2-7][0-9a-f]$$$$:^u[2-9f][0-9a-f]{3}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[ghjktuv]))?(-vert)?$$$$:^X0213(-u[0-9a-f]{4,5})?$$$$:^cdp-....$$$$'

spec_B	= '^u00[2-7][0-9a-f]$$$$:^u30[0-9a-f]{2}(-vert)?$$$$:^u2[0-9a-f]{4}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[ghjktuv]))?$$$$'

spec_AX	= '^u00[2-7][0-9a-f]$$$$:^u[2-9f][0-9a-f]{3}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[g-kmtuv])([01][0-9])?)?(-(var|itaiji)-[0-9]+)?(-vert)?$$$$:^kumimoji-u2ff[0-9ab](-u[0-9a-f]{4,5}){2,3}(-(j[av]|kp|us|[g-kmtuv])([01][0-9])?)?(-(var|itaiji)-[0-9]+)?(-vert)?$$$$:^cdp-....$$$$'

spec_BX	= '^u00[2-7][0-9a-f]$$$$:^u2[0-9a-f]{4}(-u[0-9a-f]{4,5})?(-(j[av]|kp|us|[g-kmtuv])([01][0-9])?)?(-(var|itaiji)-[0-9]+)?(-vert)?$$$$'

.PHONY: all A B AX BX clean

all: HanaMinAFDKO.tar.xz
A: HanaMinA.otf
B: HanaMinB.otf
AX: HanaMinAX.otf
BX: HanaMinBX.otf

HanaMinA.otf:
	make -f Makefile_HanaMin sub=A spec=$(spec_A) proof

HanaMinB.otf:
	make -f Makefile_HanaMin sub=B spec=$(spec_B) proof

HanaMinAX.otf:
	make -f Makefile_HanaMin sub=AX spec=$(spec_AX) proof

HanaMinBX.otf:
	make -f Makefile_HanaMin sub=BX spec=$(spec_BX) proof

HanaMinAFDKO.tar.xz: HanaMinA.otf HanaMinB.otf HanaMinA.proof.pdf HanaMinB.proof.pdf
	tar cvfJ HanaMinAFDKO.tar.xz HanaMinA.otf HanaMinB.otf README.md HanaMinA.proof.pdf HanaMinB.proof.pdf HanaMinA.cmap HanaMinB.cmap

clean:
	-make -f Makefile_HanaMin clean
	-make -f Makefile_HanaMin sub=B clean
	-make -f Makefile_HanaMin sub=AX clean
	-make -f Makefile_HanaMin sub=BX clean
