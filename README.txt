[配布物]
・README.txt
・Makefile
・dumpucs.pl
・makesvg.js
・makeSVGFont.pl
・template.cidinfo
・template.fmndb
・template.tables
・engine/ （改造版。SVGフォントを1000unit/em で出力。）
・hintcidfont.pl （改造版。FDArray名を任意に。）

[必要なもの]
・JRE 1.6以降
・js.jar … Rhino JavaScript Interpreter
・perl, sed, emacs 等のUnixツール
・AFDKO … http://lundestudio.com/AFDKO-NEW/
・cmap-tool.pl … http://lundestudio.com/AFDKO-06252012/

[実行]
(1) 作業用の適当なディレクトリを作成。
(2) Makefile を作業ディレクトリにコピーして、先頭のDIR, RHINO, spec, sub を編集。
(3) make otf を実行。（数時間かかる）
(4) HanaMin$(sub).otf が生成される。

[注意]
Makefile の最後の makeotf で、
===========
makeotfexe [FATAL] <HanaMinXXX-Regular> GSUB feature 'trad' causes
overflow of offset to a subtable (0x114c4)
===========
というエラーがでる場合は、HanaMinXXX.features ファイルの、
===========
feature trad {
  script DFLT;
  language dflt;
  .... (※)
} trad;
===========
の部分を、
===========
lookup TRAD useExtension {
  ... (※)
} TRAD;
feature trad {
  script DFLT;
  language dflt;
  lookup TRAD;
} trad;
===========
に変更して make (makeotf) を再実行する。


[参考]
パラメータ設定例： （正規表現の`$'はMakefile中では`$$'にすること）
u0020-u007e,u2f00,u3000,u3042,u30a2 はどのフォントにもあった方が良い。
make sub=A spec='^u00[2-7][0-9a-f]$$,^u2[ef][0-9a-f]{2}$$,^u3[014-9a-f][0-9a-f]{2}(-u....)?(-vert)?$$,^u[4-9][0-9a-f]{3}(-ue0...)?$$,^uf[9af][0-9a-f]{2}(-ue0...)?$$,X0213(-ue01..)?$$,^cdp-....$$'
make sub=B spec='^u00[2-7][0-9a-f]$$,^u2f00$,^u30..(-vert)?$$,^u2[0-9a-f]{4}(-ue0...)?$$'
make sub=M spec='^u00[2-7][0-9a-f]$$,^u11[0-f]{2}$$,^u2[0-9a-f]{3}$$,^u3[0-3][0-9a-f]{2}(-vert)?$$,^u[a-d][0-9a-f]{3}$$,^u1[0-9a-f]{4}$$,^kumimoji(-u[0-9a-f]+)+$$'

make sub=Aext spec='^u00[2-7][0-9a-f],^u2[ef][0-9a-f]{2}(-.+)?$$,^u3[014-9a-f][0-9a-f]{2},^u[4-9][0-9a-f]{3},^uf[9af][0-9a-f]{2},X0213.*,^cdp'
make sub=Bext spec='^u00[2-7][0-9a-f],^u2f00$$,^u30..$$,^u2[0-9a-f]{4}'
make sub=Mext spec='^u00[2-7][0-9a-f],^u11[0-f]{2},^u2[0-9a-f]{3}(-.+)?$$,^u3[0-3][0-9a-f]{2},^u[a-d][0-9a-f]{3},^u1[0-9a-f]{4},^kumimoji'
