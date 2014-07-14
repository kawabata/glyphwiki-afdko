# GlyphWiki AFDKO Font Generator

This software will produce AFDKO version of HanaMin font.

## Usage

You need AFDKO, Emacs (24.2 or later), Rhino, perl, some basic Unix
tools, 2Gbyte of disk space and internet connection.

Then, run the following commands.

    git clone http://github.com/kawabata/glyphwiki-afdko
    git clone http://git.chise.org/git/chise/kage.git
    cd glyphwiki-afdko
    ln -s ../kage/engine
    <check Makefile settings>
    make all

After several hours (depends on your machine power), "HanaMinA.otf"
and "HanaMinA.pdf" will be produced. 

To produce HanaMinB, change $(sub) and $(spec) variables as follows.

    make sub=B spec='^u00[2-7][0-9a-f]$$,^u2f00$,^u30..(-vert)?$$,^u2[0-9a-f]{4}(-ue0...)?$$' all

Note that "spec" is a regular expression to match for GlyphWiki name.
