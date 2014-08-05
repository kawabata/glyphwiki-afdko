# Create SVG Font from KAGE SVG data.

use utf8;

if($#ARGV != 0){
  print "ERROR: input the target name\n";
  exit;
}
$TARGET = @ARGV[0];

$TARGET_DIR = ".";
$WORK_DIR = "$TARGET_DIR/work";

open FH, ">:utf8", "$TARGET_DIR/$TARGET.svg";
print FH qq|<font horiz-adv-x="1000">\n<font-face font-family="$TARGET-GlyphWiki" units-per-em="1000" ascent="880" descent="120"/>\n<missing-glyph />\n|;

opendir(DIR, $WORK_DIR);
foreach my $dir (sort { -d $a <=> -d $b } readdir(DIR)) {
  if($dir !~ m/\.svg/){
    next;
  }
  $dir =~ m/^0u([0-9a-f]{4,6})\.svg$/;
  if(-e "$WORK_DIR/$dir"){
    my $ucs=$1;
    open FH2, "<:utf8", "$WORK_DIR/$dir";
    while ($line=<FH2>) {
      if ($line =~ "<glyph") {
        $line =~ s/unicode="a"/unicode="&#x$ucs;" glyph-name="u$ucs"/;
        print FH $line;
      }
    }
    close FH2;
  }
}
print FH qq|\n</font>\n|;
close FH;
