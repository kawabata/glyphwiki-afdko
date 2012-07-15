# GlyphWiki Font Generator

PROG	= /Users/kawabata/GlyphWiki/program
RHINO	= /Users/kawabata/GlyphWiki/program/js.jar
version = 2.$(shell date "+%m%d")
sub	= A
spec	= ^u00[2-7][0-9a-f]$$,^u2[ef][0-9a-f]{2}$$,^u3[014-9a-f][0-9a-f]{2}(-u....)?(-vert)?$$,^u[4-9][0-9a-f]{3}(-ue0...)?$$,^uf[9af][0-9a-f]{2}(-ue0...)?$$,X0213(-ue01..)?$$,^cdp-....$$

# ---

BASE    = HanaMin$(sub)

all: otf

dump.tar.gz:
	wget http://glyphwiki.org/dump.tar.gz

# 漢文については'-vert'を追記する。
dump_all_versions.txt dump_newest_only.txt: dump.tar.gz
	tar xvfz dump.tar.gz dump_all_versions.txt dump_newest_only.txt
	touch dump_all_versions.txt
	perl -i -pe 's/^( u319[-0-9a-z]*)(.*)$$/\1\2\n\1-vert\2/' dump_newest_only.txt

# 空白文字を追記する。
$(BASE).map $(BASE).alias $(BASE).source: dump_all_versions.txt dump_newest_only.txt
	perl $(PROG)/dumpucs.pl "$(spec)" $(BASE)
	echo "0u0020	0:0:0:0\n0u3000	0:0:0:0" >> $(BASE).source

$(BASE).svg: $(BASE).source
	java -jar $(RHINO) $(PROG)/makesvg.js $(BASE) 
	perl $(PROG)/makeSVGFont.pl $(BASE) >$(BASE).log 2>$(BASE).err 

$(BASE).pfa: $(BASE).svg
	tx -t1 $(BASE).svg $(BASE).pfa >>$(BASE).log 2>>$(BASE).err
	checkOutlines -k -O -x -e $(BASE).pfa >/dev/null 2>>$(BASE).err

$(BASE).dump: $(BASE).pfa
	tx -dump $(BASE).pfa > $(BASE).dump

$(BASE).fmndb $(BASE).cidinfo:
	sed -e s/\$$sub/$(sub)/ $(PROG)/template.fmndb > $(BASE).fmndb
	sed -e s/\$$sub/$(sub)/ $(PROG)/template.cidinfo > $(BASE).cidinfo

$(BASE).tmp.cmap $(BASE).ivs $(BASE).cidmap $(BASE).features $(BASE).html: $(BASE).map $(BASE).alias $(BASE).dump
	emacs --script $(PROG)/gw-afdko.el $(BASE) >>$(BASE).log 2>>$(BASE).err
	sed -e s/\$$version/$(version)/ $(PROG)/template.tables >> $(BASE).features

$(BASE).cmap : $(BASE).tmp.cmap
	perl $(PROG)/cmap-tool.pl < $(BASE).tmp.cmap > $(BASE).cmap

$(BASE).raw: $(BASE).cidinfo $(BASE).cidmap $(BASE).pfa
	mergeFonts -cid $(BASE).cidinfo $(BASE).raw $(BASE).cidmap $(BASE).pfa >>$(BASE).log 2>>$(BASE).err

$(BASE).hinted.raw: $(BASE).raw
	perl $(PROG)/hintcidfont.pl $(PROG)/hintparam.txt < $(BASE).raw > $(BASE).hinted.raw
	autohint -r -q $(BASE).hinted.raw >>$(BASE).log 2>>$(BASE).err

# subroutinize "-S" は非常に時間がかかるので注意
$(BASE).otf: $(BASE).fmndb $(BASE).ivs $(BASE).cmap $(BASE).hinted.raw $(BASE).features
	makeotf -mf $(BASE).fmndb -cs 1 -ci $(BASE).ivs -ch		\
	 $(BASE).cmap -f $(BASE).hinted.raw -ff $(BASE).features -o $@

$(BASE).pdf: $(BASE).otf
	spot -Proof $(BASE).otf > $(BASE).ps
	/usr/bin/pstopdf $(BASE).ps $(BASE).pdf

otf: $(BASE).otf

proof: $(BASE).pdf

clean:
	rm $(BASE).map $(BASE).alias $(BASE).source $(BASE).svg		\
	  $(BASE).pfa $(BASE).dump $(BASE).fmndb $(BASE).cidmap		\
	  $(BASE).cidinfo $(BASE).cmap $(BASE).tmp.cmap $(BASE).raw	\
	  $(BASE).hinted.raw $(BASE).log $(BASE).err $(BASE).features	\
	  $(BASE).ivs $(BASE).ps current.fpr checkOutlines.log*		\
	 GlyphWiki-*
	rm -r ./work

distclean: 
	rm -r $(BASE).* dump* ./work Glyphwiki-* current.fpr checkOutlines.log*

