# GlyphWiki AFDKO Font Generator

- site :: http://github.com/kawabata/glyphwiki-afdko

## About

This is a tool to produce the fonts from [GlyphWiki](http://glyphwiki.org).

## Usage

You need AFDKO, Emacs (24.2 or later), Rhino, perl, some basic Unix
tools, 2Gbyte of disk space and internet connection.

Then, run the following commands.

    git clone http://github.com/kawabata/glyphwiki-afdko
    cd glyphwiki-afdko
    «check Makefile settings»
    make all

This will download "dump.tar.gz" from GlyphWiki web site, and then
create the font from it. It may take several hours (depends on your
machine power), and then "HanaMinA.otf" and "HanaMinA.pdf" will be
produced.

To produce HanaMinB, change $(sub) and $(spec) variables as follows.

    make sub=B spec='^u00[2-7a-f][0-9a-f]$$,^u2f00$$,^u30..(-vert)?$$,^u2[0-9a-f]{4}(-ue0...)?(-ufe0.)?$$' all

Note that "spec" is a regular expression to match for GlyphWiki name.
When make fails, there may be some empty files, which you should
remove to avoid unexpected error.

    find . -type f -empty

For details of this tool, please refer the paper in
http://www.atypi.org/past-conferences/hong-kong-2012/programme/11-oct-type-technologies-workshops/activity?a=154
.

Any bug report is welcome, but I couldn't gurantee to fix them.

## License.

Kage Engine is taken from Kage
(http://git.chise.org/git/chise/kage.git), distributed under GPL.

'cmap-tool.pl' is copyrighted by Adobe, and is distribued under Adobe
Opentype Font Developer's Kit ("Fdk") License Agreement, see
(http://www.adobe.com/jp/devnet/opentype/afdko/eula.html)

For the license of the fonts, please refer
[GlyphWiki License](http://en.glyphwiki.org/wiki/GlyphWiki:License).

