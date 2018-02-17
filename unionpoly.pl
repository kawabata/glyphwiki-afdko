#!/usr/bin/perl

use Math::Clipper ':all';

$filename = $ARGV[0];
open FH, "<$filename";
my @buffer = <FH>;
close FH;

my @poly = ();
my $count = 0;
foreach(@buffer){
    if($_ =~ m/^<polygon points="([^"]+)"/){
	my @temp = split(/ /, $1);
	@temp = map{ my @temp2 = split(/,/, $_); map{ $_ *= 10; } @temp2; join(",", @temp2) } @temp;
	my $temp = "[ [".join('],[', @temp)."] ]";
	if(!orientation(eval($temp))){
	$temp = "[ [".join('],[', reverse(@temp))."] ]";
	}
	push(@poly, $temp);
	$count++;
    }
}

$result = simplify_polygons(eval("[".join(",", @poly)."]"), PFT_NONZERO);

if($count == 0){
    exit;
}

# unlink($filename);

open FH, ">$filename.font";

print FH<<"EOT";
<font horiz-adv-x="1000">
<font-face font-family="GlyphWiki" units-per-em="1000" ascent="880" descent="120"/>
EOT

print FH "<glyph unicode=\"a\" d=\"";

my $i = 0;
while(length($result->[$i][0][0]) > 0){
    print FH "M ".($result->[$i][0][0] / 2)." ".(- $result->[$i][0][1] / 2 + 880)." ";
    my $j = 1;
    while(length($result->[$i][$j][0]) > 0){
	print FH "L ".($result->[$i][$j][0] / 2)." ".(- $result->[$i][$j][1] / 2 + 880)." ";
	$j++;
    }
    print FH "Z ";
    $i++;
}

print FH "\" />\n";

print FH <<"EOT";
</font>
EOT

close FH;
