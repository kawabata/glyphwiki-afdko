GlyphWiki AFDKO Font Generator
==============================

## OverView

This is a tool to produce the fonts from [GlyphWiki](http://glyphwiki.org).

## Description

This tool creates OpenType GSUB/GPOS featured font from
[Kage](http://fonts.jp/kage/) data of GlyphWiki.

Following procedures are taken to create fonts from GlyphWiki.

1. Download newest dump data from GlyphWiki web site.
2. Extract and expand Kage Data of needed glyphs.
3. Convert Kage data to SVG image files by [Kage Engine](http://git.chise.org/gitweb/?p=chise/kage.git;a=summary).
4. Clip the outlines of SVG images by using
   [Clipper](http://sourceforge.net/projects/polyclipping/), then
   convert it to SVG Font.
5. Convert SVG Font to PostScript Type1 name-keyed font by
   [AFDKO](http://www.adobe.com/jp/devnet/opentype/afdko.html) tx.
6. Dump name-keys of created font and create CID index.
7. Create CMAP (including IVS) and GSUB/GPOS features based on dump data.
8. Create CID-keyed font by using AFDKO mergeFonts.
9. Create OpenType font from CID-Keyed font by using AFDKO mkotf.

## Requirements

You need [AFDKO](http://www.adobe.com/jp/devnet/opentype/afdko.html),
[Emacs](www.gnu.org/software/emacs) (24.2 or later),
[Rhino](https://developer.mozilla.org/ja/docs/Rhino), perl, wget,
[Math::Clipper](http://search.cpan.org/~smueller/Math-Clipper/lib/Math/Clipper.pm),
basic Unix tools, 2Gbyte of disk space and the Internet connection.

If you are using MacOS 10.9.1 or later, installation of Math::Clipper
via CPAN may fail with default perl. In that case, try using perl5.18.

## Usage

In default, two fonts will be generated.

- HanaMinA.otf (Font Family Name: Hanazono Mincho A)
  This font mainly covers CJKV Ideographs of BMP (Basic Mulitilingual Plane).

- HanaMinB.otf (Font Family Name: Hanazono Mincho B)
  This font mainly covers CJKV Ideographs of SIP (Supplemental Ideographic Plane).

Specify these names in your CSS or other tools as you like.

## Installation

Run the following command to create the newest font. Please make sure
that you install all the required fonts, set up Perl library
environment variables and properly set the $(AFDKO) environment
variable to the installation directory of AFDKO perl scripts.

    git clone http://github.com/kawabata/glyphwiki-afdko
    cd glyphwiki-afdko
    «check Makefile/Makefile_HanaMin settings»
    make

This will download "dump.tar.gz" from GlyphWiki web site and create
the fonts. It may take several hours (depends on your machine power),
and then "HanaMinA.otf" and "HanaMinB.otf" will be produced.

You can check the content of produced fonts by AFDKO spot tools, e.g.

    spot -Proof HanaMinA.pdf > HanaMinA.ps
    ps2pdf HanaMinA.ps -o HanaMinA.pdf

Note that "spec" is a regular expression to match for GlyphWiki name.
Also, you can directly download the file from
[HanaMinAFDKO](https://github.com/cjkvi/HanaMinAFDKO/release) page.

## Troubleshooting

When make command fails, there may be some empty files, which you
should remove to avoid unexpected error.

    find . -type f -empty -delete

For details of this tool, please refer the paper in

## License

Kage Engine is taken from Kage
(http://git.chise.org/git/chise/kage.git), distributed under
[GPLv2](http://www.gnu.org/licenses/gpl-2.0.html).

'cmap-tool.pl' is copyrighted by Adobe, and is distribued under Adobe
Opentype Font Developer's Kit ("Fdk")
[License Agreement](http://www.adobe.com/jp/devnet/opentype/afdko/eula.html).

For the license of the generated fonts, please refer
[GlyphWiki License](http://en.glyphwiki.org/wiki/GlyphWiki:License).

## References

KAWABATA TAichi and KAMICHI Koichi, GlyphWiki - a Wiki-based glyph design and font production system. [link](http://www.atypi.org/past-conferences/hong-kong-2012/papers-and-presentations/glyphwiki-a-wiki-based-glyph-design-and-font-production-system.-taichi-kawabata-and-kamichi-koichi/view)
