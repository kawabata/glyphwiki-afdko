# GlyphWiki AFDKO Font Generator

- site :: http://github.com/kawabata/glyphwiki-afdko

## About

This is a tool to produce the fonts from [GlyphWiki](http://glyphwiki.org).

## Usage

You need [AFDKO](http://www.adobe.com/jp/devnet/opentype/afdko.html),
Emacs (24.2 or later),
[Rhino](https://developer.mozilla.org/ja/docs/Rhino), perl, wget, some
basic Unix tools, 2Gbyte of disk space and internet connection.

Then, run the following commands.

    git clone http://github.com/kawabata/glyphwiki-afdko
    cd glyphwiki-afdko
    «check Makefile/Makefile_HanaMin settings»
    make

This will download "dump.tar.gz" from GlyphWiki web site and create
the fonts. It may take several hours (depends on your machine power),
and then "HanaMinA.otf" and "HanaMinB.otf" will be produced.

Note that "spec" is a regular expression to match for GlyphWiki name.
When make command fails, there may be some empty files, which you
should remove to avoid unexpected error.

    find . -type f -empty -delete

For details of this tool, please refer the paper in
http://www.atypi.org/past-conferences/hong-kong-2012/programme/11-oct-type-technologies-workshops/activity?a=154
.

Any bug report is welcome, but I couldn't gurantee to fix them.

## License

Kage Engine is taken from Kage
(http://git.chise.org/git/chise/kage.git), distributed under GPL.

'cmap-tool.pl' is copyrighted by Adobe, and is distribued under Adobe
Opentype Font Developer's Kit ("Fdk") License Agreement, see
(http://www.adobe.com/jp/devnet/opentype/afdko/eula.html)

For the license of the generated fonts, please refer
[GlyphWiki License](http://en.glyphwiki.org/wiki/GlyphWiki:License).
