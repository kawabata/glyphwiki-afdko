;;; gw-afdko.el  -*- coding: utf-8 -*-

;; GlyphWiki to AFDKO generator

;; Usage ::
;; % emacs --script $DIR/gw-afdko.el gw000000

;;;; Code:

;; basic definitions
(defvar gw-base      nil) ;; "HanaMinA")
(defvar gw-file-base nil) ;;  (expand-file-name
                          ;;   (concat "~/work/HanaMin/" gw-base "/" gw-base)))
;; input files
(defvar gw-map-file    nil) ;; (concat gw-file-base ".map"))
(defvar gw-alias-file  nil) ;; (concat gw-file-base ".alias"))
(defvar gw-dump-file   nil) ;; (concat gw-file-base ".dump"))
;; output files
(defvar gw-cmap-file     nil) ;; (concat gw-file-base ".cmap"))
(defvar gw-html-file     nil) ;; (concat gw-file-base ".html"))
(defvar gw-ivs-file      nil) ;; (concat gw-file-base ".ivs"))
(defvar gw-cidmap-file   nil) ;; (concat gw-file-base ".cidmap"))
(defvar gw-features-file nil) ;; (concat gw-file-base ".features"))

;; tables
(defvar gw-glyphname-ucs-table nil)
(defvar gw-glyphname-feature-table nil)
(defvar gw-ucs-cid-table nil)
(defvar gw-feature-table nil)
(defvar gw-cid-vmtx-table nil)
(defvar gw-regexp-cid-table nil)

;; misc variables
(defvar gw-feature-order '(aalt ccmp hwid liga locl ss00 ss01 ss02 ss03
                           ss04 ss05 ss06 ss07 ss08 ss09 ss10 ss11 ss12
                           ss13 ss14 ss15 salt vert vrt2))
(defvar gw-lang-regexp
  (regexp-opt '("g" "t" "j" "k" "v" "h" "u" "us" "i" "ja" "jv" "js" "kp")))
(defvar gw-vmtx-advanceY-data ;; VertAdvanceY
  '(("^u319.+-vert$" . 500)))
(defvar gw-vkrn-data ;; vkrn
  '(("^u3190-vert" "^u319[1-9a-f]\\(-u3191\\)?-vert" -500)
    ("^u319[0-9a-f]\\(-u3191\\)?-vert" "^\\(u300[12]\\)-vert" -500)))

;; "js" "m" "kp" "i" は後でサポート
(defvar gw-lang
  '(("j" . "JAN")  ("ja" . "JAN") ("js" . "JAN") ("jv" . "JAN")
    ("g" . "ZHS")  ("t"  . "ZHT") ("h"  . "ZHH") ("k"  . "KOR")
    ("kp" . "KOR") ("v" . "VIE")  ("u"  . "ENG")))
(defvar gw-lang-script
  '(("dflt" . "DFLT")
    ("JAN" . "hani") ("ZHS" . "hani") ("ZHT" . "hani") ("ZHH" . "hani")
    ("KOR" . "hani") ("VIE" . "hani") ("ENG" . "hani")))
(defvar gw-lang-html
  '(("dflt" . "DFLT")
    ("JAN" . "ja")   ("ZHS" . "zh")   ("ZHT" . "zh_TW") ("ZHH" . "zh_HK")
    ("KOR" . "ko")   ("VIE" . "vn")   ("ENG" . "en")))

(defvar gw-cmap-preamble
"%!PS-Adobe-3.0 Resource-CMap
%%DocumentNeededResources: ProcSet (CIDInit)
%%IncludeResource: ProcSet (CIDInit)
%%BeginResource: CMap (GlyphWiki-UTF32-H)
%%Title: (GlyphWiki-UTF32-H Adobe Identity 0)
%%Version: 1.000
%%EndComments

/CIDInit /ProcSet findresource begin

12 dict begin

begincmap

/CIDSystemInfo 3 dict dup begin
  /Registry (Adobe) def
  /Ordering (Identity) def
  /Supplement 0 def
end def

/CMapName /GlyphWiki-UTF32-H def
/CMapVersion 1.000 def
/CMapType 1 def

/XUID [] def

/WMode 0 def

1 begincodespacerange
  <00000000> <0010FFFF>
endcodespacerange

1 beginnotdefrange
<00000000> <0000001f> 1
endnotdefrange

0 begincidchar
")

(defvar gw-cmap-postscript "
endcidchar

endcmap
CMapName currentdict /CMap defineresource pop
end
end

%%EndResource
%%EOF
")

(defvar gw-html-preamble
"<head>
  <title>CSS3 Font Test.</title>
  <meta http-equiv='Content-Type' content='text/html; charset=UTF-8'/>
  <style type='text/css'>
    @font-face {
      font-family: 'Hanazono Mincho';
      src: url('./%s.otf') format('opentype');
    }
    .HanaMin {
      font-family: 'Hanazono Mincho';
      font-size: 32;
    };
  </style>
</head>
<body>
  <h3>花園明朝・漢字表示テスト</h3>
  <table border='1'>
    <tr><td>GlyphWiki名</td><td>lang</td><td>font-feature-settings</td><td></td></tr>
")

(defvar gw-html-postscript
"</table>
</body>
")

;; utility function
(defun addhash (key value table &optional append)
  "Add VALUE with list associated with KEY in table TABLE."
  (let ((x (gethash key table)))
    (add-to-list 'x value append)
    (puthash key x table)))

;; main
(defun gw-setup ()
  (interactive)
  (gw-setup-variables)
  (dolist (file (list gw-map-file gw-alias-file gw-dump-file))
    (if (not (file-exists-p file))
        (error "file `%s' does not exist!" file)))
  (with-temp-buffer
    (insert-file-contents gw-map-file)
    (while (re-search-forward "^u\\([0-9a-f]+\\)\t\\(.+\\)" nil t)
      (let ((ucs-name (concat "u" (match-string 1)))
            (ucs (string-to-number (match-string 1) 16))
            (glyphname (match-string 2)))
        (puthash glyphname ucs gw-glyphname-ucs-table)
        (puthash ucs-name ucs gw-glyphname-ucs-table))))
  (with-temp-buffer
    (insert-file-contents gw-dump-file)
    (while (re-search-forward "glyph\\[\\([0-9]+\\)\\] {u\\([^,]+\\)," nil t)
      (puthash (string-to-number (match-string 2) 16)
               (string-to-number (match-string 1)) gw-ucs-cid-table)))
  (with-temp-buffer
    (insert-file-contents gw-alias-file)
    (while (re-search-forward "^\\(.+\\)\t\\(.+\\)" nil t)
      (let* ((alias (match-string 1)) (real (match-string 2))
             (ucs   (gw-glyphname-to-ucs real)))
        (puthash alias ucs gw-glyphname-ucs-table))))
  ;; output cidmap file
  (with-temp-file gw-cidmap-file
    (insert-file-contents gw-dump-file)
    (delete-non-matching-lines "^glyph\\[\\([0-9]+\\)\\] {\\([^,]+\\),")
    (goto-char (point-min)) (insert "mergeFonts\n")
    (while (re-search-forward "^glyph\\[\\([0-9]+\\)\\] {\\([^,]+\\),.*$" nil t)
      (replace-match (concat (match-string 1) " " (match-string 2))))
    )
  ;; output CMAP file ;; 後で、cmap-tools で整えなおすこと。
  ;; % cmap-tool.pl < XXX.cmap > XXX.tmp.cmap
  ;; % mv XXX.tmp.cmap XXX.cmap
  (with-temp-file gw-cmap-file
    (insert gw-cmap-preamble)
    (maphash
     (lambda (ucs cid)
       (when (< ucs #xf0000)
         (insert (format "<%08x> %d\n" ucs cid))))
     gw-ucs-cid-table)
    (insert gw-cmap-postscript))
  ;; output IVS file
  (with-temp-file gw-ivs-file
    (maphash
     (lambda (glyphname ucs)
       (when (or (string-match "^u\\([0-9a-f]+\\)-u\\(e01[0-9a-f][0-9a-f]\\)$" glyphname)
                 (string-match "^u\\([0-9a-f]+\\)-u\\(fe0[0-9a-f]\\)$" glyphname))
         (insert (format "%s %s; Adobe-Japan1; CID+%d\n"
                         (upcase (match-string 1 glyphname))
                         (upcase (match-string 2 glyphname))
                         (gethash ucs gw-ucs-cid-table)))))
     gw-glyphname-ucs-table))
  ;; process features & vertical parameters
  (maphash
   (lambda (glyphname ucs)
     (gw-process-feature glyphname)
     (gw-process-vertical glyphname))
   gw-glyphname-ucs-table)
  ;; output features & vertical parameters
  (with-temp-file gw-features-file
    (gw-output-feature)
    (gw-output-vertical))
  ;; output html file
  (with-temp-file gw-html-file
    (insert (format gw-html-preamble gw-base))
    (goto-char (point-max))
    (maphash
     (lambda (glyphname features)
       (if (not (string-match "^u[0-9a-f]+$" glyphname))
           (insert (gw-font-html-table-row glyphname) "\n")))
     gw-glyphname-feature-table)
    (insert gw-html-postscript))
  )

(defun gw-setup-variables ()
  (setq ;; input file
        gw-map-file      (concat gw-file-base ".map")
        gw-alias-file    (concat gw-file-base ".alias")
        gw-dump-file     (concat gw-file-base ".dump")
        ;; output file
        gw-cmap-file     (concat gw-file-base ".cmap")
        gw-html-file     (concat gw-file-base ".html")
        gw-ivs-file      (concat gw-file-base ".ivs")
        gw-cidmap-file   (concat gw-file-base ".cidmap")
        gw-features-file (concat gw-file-base ".features")
        ;; related tables
        gw-glyphname-ucs-table     (make-hash-table :test 'equal)
        gw-ucs-cid-table           (make-hash-table :test 'equal)
        gw-feature-table           (make-hash-table :test 'equal)
        gw-glyphname-feature-table (make-hash-table :test 'equal)
        gw-cid-vmtx-table          (make-hash-table :test 'equal)
        gw-regexp-cid-table        (make-hash-table :test 'equal)))

(defun gw-glyphname-to-ucs (glyphname)
  (or (gethash glyphname gw-glyphname-ucs-table)
      (if (string-match "^u\\([0-9a-f]+\\)$" glyphname)
          (string-to-number (match-string 1 glyphname) 16)
        nil)))

(defun gw-glyphname-to-cid (glyphnames)
  ;; グリフ名と、グリフ名のリストの両方に対応。
  (if (listp glyphnames)
      (let ((cids (mapcar 'gw-glyphname-to-cid glyphnames)))
        (if (memq nil cids) nil cids))
    (let* ((ucs (gw-glyphname-to-ucs glyphnames))
           (cid (gethash ucs gw-ucs-cid-table)))
      cid)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; features

(defun gw-process-feature (glyphname)
  (let* ((analysis    (gw-analyze-feature glyphname))
         (parents       (car analysis))
         (relation      (cdr analysis))
         (feature-set (and analysis
                           (gw-relation-to-feature-set relation)))
         (feature       (car feature-set))
         (lang          (elt feature-set 1))
         (alt           (elt feature-set 2))
         (parents-cid (gw-glyphname-to-cid parents))
         (cid         (gw-glyphname-to-cid glyphname)))
    (cond ((null feature) nil)
          ((equal feature "ivs") nil)
          ((null analysis) (message "improper glyphname %s" glyphname) nil)
          ((null feature-set) (message "improper relation %s" relation) nil)
          ((null parents-cid) (message "improper parents %s" parents) nil)
           ;;(message "improper parents-cid %s (gn=%s) mapcar=%s"
           ;;         parents glyphname (mapcar 'gw-glyphname-to-cid parents))
           ;;nil)
          ((null cid) (message "improper cid %s" glyphname) nil)
          (t
           (puthash glyphname (list parents feature lang alt)
                    gw-glyphname-feature-table)
           (gw-register-feature feature lang parents-cid alt cid)))))

(defun gw-relation-to-feature-set (relation)
  ;; relation から、(feature lang alt) を返す。
  (cond ((null relation) nil)
        ((equal relation "-vert") '("vert"))
        ((equal relation "-halfwidth") '("hwid"))
        ((string-match "^-var-\\([0-9]+\\)$" relation)
         (list "salt" nil (string-to-number (match-string 1 relation))))
        ((string-match "^-itaiji-\\([0-9]+\\)$" relation)
         (list "aalt" nil (string-to-number (match-string 1 relation))))
        ((string-match "^-\\([0-9][0-9]\\)$" relation)
         (list (concat "ss" (match-string 1 relation))))
        ((string-match "^-\\(j[av]\\|kp\\|us\\|[g-kmtuv]\\)\\([0-9][0-9]\\)$"
                       relation)
         (list (concat "ss" (match-string 2 relation))
               (cdr (assoc (match-string 1 relation) gw-lang))))
        ((string-match "^-\\(j[av]\\|kp\\|us\\|[g-kmtuv]\\)$" relation)
         (list "locl" (cdr (assoc (match-string 1 relation) gw-lang))))
        ((or (equal "ivs" relation)
             (equal "ccmp" relation)
             (equal "liga" relation))
         (list relation))
        (t (error "imporper relation! %s" relation))))

(defvar gw-glyphname-regexp
  (concat
   "^\\(?:kumimoji-\\)?"
   "\\(u[0-9a-f]+\\|cdp-[0-9a-f]+\\)" ;; base 1
   "\\(\\(?:-\\(?:u[0-9a-f]+\\|cdp-[0-9a-f]+\\)\\)+\\)?" ;; base 2
   "\\(-\\(j[av]\\|kp\\|us\\|[g-kmtuv]\\)?\\([01][0-9]\\)?\\)?" ;; component modifier
   "\\(-\\(?:var\\|itaiji\\)-[0-9]+\\)?" ;; variation
   "\\(-vert\\)?" ;; vert
   "\\(-halfwidth\\)?$")) ;; half width

(defun gw-analyze-feature (glyphname)
  ;; GlyphWiki 名を解析して、親グリフ名リストと親子関係 (parents relation)を
  ;; glyphwiki名で返す。不正な場合はnilを返す。
  (if (string-match gw-glyphname-regexp glyphname)
    (let ((base1 (match-string 1 glyphname))
          (base2 (match-string 2 glyphname))
          (lang-comp (match-string 3 glyphname))
          (variation (match-string 6 glyphname))
          (vert (match-string 7 glyphname))
          (half-width (match-string 8 glyphname))
          parents relation)
      (cond (vert (setq parents (concat base1 base2 lang-comp variation)
                        relation vert))
            (half-width (setq parents (concat base1 base2 lang-comp variation)
                              relation half-width))
            (variation (setq parents (concat base1 base2 lang-comp)
                             relation variation))
            (lang-comp (setq parents (concat base1 base2)
                             relation lang-comp))
            (base2     (setq parents (gw-split-base-glyphs (concat base1 base2))
                             relation
                             (cond ((string-match "^-ue01" base2) "ivs")
                                   ((string-match "^-ufe0" base2) "ivs")
                                   ((string-match "^u2ff" base1) "ccmp")
                                   (t "liga")))))
      (cons parents relation))
    (message "Not proper GlyphWiki name! %s" glyphname)
    nil))

(defun gw-split-base-glyphs (base-chars)
  (let (result)
    (with-temp-buffer
      (insert base-chars)
      (goto-char (point-min))
      (while (re-search-forward "\\(u[0-9a-f]+\\|cdp-[0-9a-f]+\\)" nil t)
        (setq result (cons (match-string 1) result))))
    (nreverse result)))

;; gw-feature-table (key: feature, val:  lang-table)
;; lang-table (key: opentype-lang, val: orig-table)
;; orig-table (key: char)
;;   - single substitution : char -> char
;;   - multiple substitution : not used.
;;   - alternate substitution : char -> ((num . char) (num . char) ...)
;;   - ligature substitution  : (char list) -> char

(defun gw-register-feature (feature lang original alt target)
  (let* ((lang       (or lang "dflt"))
         (lang-table (or (gethash feature gw-feature-table)
                         (let ((new-lang-table (make-hash-table :test 'equal)))
                           (puthash feature new-lang-table gw-feature-table)
                           new-lang-table)))
         (orig-table (or (gethash lang lang-table)
                         (let ((new-orig-table (make-hash-table :test 'equal)))
                           (puthash lang new-orig-table lang-table)
                           new-orig-table))))
    (if alt (addhash original (cons alt target) orig-table)
      (if (gethash original orig-table)
          (message "Warning! Duplicate feature! %s" (list feature lang original alt target)))
      (unless (equal original target)
        (puthash original target orig-table)))))

;; cf. ~/bin/FDK/Technical Documentation/topic_feature_file_syntax.html
(defun gw-output-feature ()
  "現在のバッファに gw-feature-table　の内容を出力する."
  (dolist (lang-script gw-lang-script)
    (insert "languagesystem " (cdr lang-script) " " (car lang-script) ";\n"))
  (let ((vert-lang-table (gethash "vert" gw-feature-table)))
    ;;;; vert がなければ空vertを作る。
    ;;;;(when (null vert-lang-table)
    ;;;;  (gw-register-feature "vert" nil 0 nil 0)
    ;;;;  (setq vert-lang-table (gethash "vert" gw-feature-table)))
    ;; vert があればvrt2にコピーする。
    (when vert-lang-table
      (puthash "vrt2" (copy-hash-table vert-lang-table) gw-feature-table)))
  ;; 各featureを順に出力する。
  (dolist (feature gw-feature-order)
    (let ((lang-table (gethash (symbol-name feature) gw-feature-table)))
      (when lang-table
        ;; aalt に限って、 useExtension を入れる。
        (if (equal feature 'aalt)
            (insert "feature aalt useExtension {\n")
          (insert (format "feature %s {\n" feature)))
        ;; aalt に限って、dflt 言語を出力しない。
        (let ((dflt-table (gethash "dflt" lang-table)))
          (if dflt-table
              (if (equal feature 'aalt)
                  (gw-output-feature-lang nil dflt-table)
              (gw-output-feature-lang "dflt" dflt-table))))
        (maphash
         (lambda (lang orig-table)
           (if (not (equal lang "dflt"))
               (gw-output-feature-lang lang orig-table)))
         lang-table)
        (insert "} " (symbol-name feature) ";\n")))))

(defun gw-output-feature-lang (lang orig-table)
  (when lang
    (insert "  script " (cdr (assoc lang gw-lang-script)) ";\n")
    (insert "  language " lang ";\n"))
  (maphash
   (lambda (orig target)
     (insert
      (format
       "    substitute %s %s;\n"
       (if (integerp orig) (format "\\%d" orig) ;; single/alternate sub.
         ;; ligature substitution
         (mapconcat (lambda (x) (format "\\%d" x)) orig " "))
       (if (listp target)
           ;; alternate substitution
           (concat
            "from ["
            (mapconcat
             (lambda (x) (format "\\%d" x))
             (gw-sort-to-array target)
             " ")
            "]")
         ;; single/ligature substitution
         (format "by \\%d" target)))))
   orig-table))

(defun gw-sort-to-array (list)
  ;; ((3 . 223) (1 . 114) (2 . 341)...) → [114 341 223 ...]
  (let ((len 1) vec)
    (dolist (item list)
      (if (< len (car item)) (setq len (car item))))
    (setq vec (make-vector len 1)) ;; 2018 ここを 0→1にしてみる
    (dolist (item list)
      (aset vec (1- (car item)) (cdr item)))
    vec))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertical matrix/kerning
(defun gw-process-vertical (glyphname)
  (dolist (item gw-vmtx-advanceY-data)
    (let ((regexp (car item)) (val (cdr item)))
      (if (string-match regexp glyphname)
          (puthash (gw-glyphname-to-cid glyphname) val
                   gw-cid-vmtx-table))))
  (dolist (regexp (apply 'nconc
                         (mapcar
                          (lambda (x) (list (elt x 0) (elt x 1)))
                          gw-vkrn-data)))
    (if (string-match regexp glyphname)
        (addhash regexp (gw-glyphname-to-cid glyphname)
                 gw-regexp-cid-table))))

(defun gw-output-vertical ()
  ;; table vmtx
  (goto-char (point-min)) ;; vrt2 の後に来ると `VertAdvanceY redefined' エラーが発生することがあるため。
  (when (/= 0 (hash-table-count gw-cid-vmtx-table))
    (insert "\ntable vmtx {\n")
    (maphash (lambda (cid vertAdvanceY)
               (insert (format "  VertAdvanceY \\%d %d;\n"
                               cid vertAdvanceY)))
             gw-cid-vmtx-table)
    (insert "} vmtx;\n"))
  ;; feature vkrn
  (goto-char (point-max))
  (let (positions)
    (dolist (item gw-vkrn-data)
      (let* ((regexp1      (elt item 0))
             (regexp1-item (gethash regexp1 gw-regexp-cid-table))
             (regexp2      (elt item 1))
             (regexp2-item (gethash regexp2 gw-regexp-cid-table))
             (vkrn         (elt item 2)))
        (dolist (item1 regexp1-item)
          (dolist (item2 regexp2-item)
            (setq positions (cons (list item1 item2 vkrn) positions))))))
    (when positions
      (insert "\nfeature vkrn {\n")
      (insert "  script DFLT;\n")
      (insert "  language dflt;\n")
      (dolist (position positions)
        (let* ((first   (elt position 0))
               (second  (elt position 1))
               (kerning (elt position 2)))
          (insert (format "    position \\%d \\%d %d;\n"
                              first second kerning))))
      (insert "} vkrn; \n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML output
(defun gw-font-html-table-row (glyphname)
  (let* ((result (gw-font-features glyphname))
         (chars    (elt result 0))
         (lang     (elt result 1))
         (features (elt result 2))
         (lang-html (if lang (cdr (assoc lang gw-lang-html))))
         (features-html
          (if features
              (mapconcat
               (lambda (x)
                 (let ((feature (car x))
                       (alt (cdr x)))
                   (concat "'" feature "'"
                           (if alt (format " %d" alt) ""))))
                 features ","))))
    ; glyphname lang features
    (concat "<tr><td>" glyphname
            "</td><td>" (or lang-html "")
            "</td><td>" (or features-html "")
            "</td><td class='HanaMin'"
            (if lang-html (concat " lang='" lang-html "'") "")
            (if features-html
                (concat " style=\"-webkit-font-feature-settings: "
                        features-html ";\"") "")
            ">" chars "</td></tr>")))

(defun gw-font-features (glyphname &optional lang features)
  ;; glyphname から祖先を探索して (親文字 lang ((feature . alt) (feature . alt)...)) を返す。
  ;; glyphname の親や祖先がグリフとしてない場合はnilを返す。
  (let* ((feature-set (gethash glyphname gw-glyphname-feature-table))
         (parent      (car feature-set))
         (feature     (elt feature-set 1))
         (lang2       (elt feature-set 2))
         (alt         (elt feature-set 3)))
    (if (and (stringp glyphname)
             (or (string-match "^u[0-9a-f]+$" glyphname)
                 (string-match "^cdp-[0-9a-f]+$" glyphname)))
          (list (string (gw-uname-to-ucs glyphname)) lang features)
      (if (null feature-set) nil
        (if (listp parent)
            (list (apply 'string (mapcar 'gw-uname-to-ucs parent))
                  (or lang lang2)
                  (cons (cons feature alt) features))
          (gw-font-features parent (or lang lang2)
                            (cons (cons feature alt) features)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun gw-uname-to-ucs (gw-name)
  (cond
   ((string-match "^u\\([0-9a-f]+\\)$" gw-name)
    (string-to-number (match-string 1 gw-name) 16))
   ((string-match
     "^cdp-\\([0-9a-f][0-9a-f]\\)\\([0-9a-f][0-9a-f]\\)$" gw-name)
    (let* ((h (string-to-number (match-string 1 gw-name) 16))
           (l (string-to-number (match-string 2 gw-name) 16))
           (ucs (+ #xeeb8 (* 157 (- h #x81)))))
      (setq ucs
            (if (< l #x80) (+ ucs l (- #x40)) (+ ucs l (- #x62))))
      ucs))
   (t (error "not proper char name!"))))

;; --------------------------
;; commad-line main function

(defun gw-afdko-main (argv)
  (when (or (null argv) (equal (car argv) "-h"))
    (error "Usage: emacs --script gw-afdko.el <basename>"))
  (let* ((fname (expand-file-name (car argv))))
    (setq gw-file-base   (file-name-sans-extension fname)
          gw-base        (file-name-nondirectory gw-file-base))
    (gw-setup)))

;; when invoked from command line
(when argv (gw-afdko-main argv))
