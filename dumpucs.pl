#!/usr/bin/perl

use utf8;
use POSIX;
use List::Util qw(min max);

# USAGE: dumpucs.pl regexps <output>
# e.g. dumpucs.pl '^u[2-9][0-9a-f]{3}:^uf[9a][0-9a-f]{3}:^cdp,:kumimoji' HanaMinA
# e.g. dumpucs.pl '^u2[0-9a-f]{4}' HanaMinB

my @regexps = split(/:/,$ARGV[0]);

my $fontname = $ARGV[1];

my $dump1 = "dump_newest_only.txt";
my $dump2 = "dump_all_versions.txt";
my %dump1;
my %dump2;

open FH, "<$dump1";
while(<FH>){
    $_ =~ m/^ ([^ ]+) *\|[^\|]+\| (.+)\n$/;
    $dump1{$1} = $2;
}
close FH;

# Load "dump_all_versions.txt" entries with "@XXX" (old revision)
# included.
open FH, "<$dump2";
while(<FH>){
    $_ =~ m/^ ([^\@]+\@[0-9]+) *\|[^\|]+\| (.+)\n$/;
    $dump2{$1} = $2;
}
close FH;

my %names;
my %data; # {Kage Stroke data -> name} table
my $alias;
my $map;
my $code = 0xf0000;

### Output files
## output file :: $fontname.{source,alias,map}

my $fh;
open my $fh, ">$fontname.source";

# uXXXX を、$fontname.source へ出力し、グリフ形状を@dataへ登録。
foreach(sort(keys(%dump1))){
    my $name = $_;
    if ($name =~ m/^u[0-9a-f]+$/) {
        if (check_regexps($name) == 1) {
            proc_ucs($name,$fh);
        }
    }
}

# cdp を、$fontname.sourceへ出力、@data, $map へ加える。
foreach(sort(keys(%dump1))){
    my $name = $_;
    if($name =~ m/^cdp-[0-9a-f]{4}$/){
        if (check_regexps($name) == 1) {
            proc_cdp($name,$fh);
        }
    }
}

# 最新データから、条件に適合するデータを抽出し、
# グリフが存在するなら alias へ、存在しないなら $map と@data へ登録。
foreach(sort(keys(%dump1))){
    my $name = $_;
    # この条件チェックは今となっては無用かも。
    if (!($name =~ m/^u[0-9a-f]+$/) &&
        !($name =~ m/^cdp-[0-9a-f]+$/) &&
        ($name =~ m/^(kumimoji-)?(u([0-9a-f]+)|(cdp)-[0-9a-f]+)((?:-(?:u[0-9a-f]+|cdp-[0-9a-f]+))*)(-(?:j[av]|kp|us|[g-kmtuv])?(?:[01][0-9])?)?(-(?:var|itaiji)-[0-9]+)?(-vert)?$/)) {
        #my $name_kumimoji = $1;
        #my $name_ucs_val = hex($3);
        #my $name_cdp = $4;
        my $name_base = $2.$5;
        #my $name_regcomp = $6;
        #my $name_variation = $7;
        my $name_vert = $8;
        if (check_regexps($name)) {
            # name_base を分解して、各文字が未登録なら登録する。
            foreach (split(/(?<!p)-/,$name_base)) {
                my $base_name = $_;
                if ($_ =~ /^cdp/) {
                    proc_cdp($base_name,$fh);
                } elsif ($_ =~ /^u/) {
                    proc_ucs($base_name,$fh);
                }
            }
            my $data = get_strokes_data($name);
            if($data ne ""){
                # `-vert' は必ず別グリフにする。
                if((!exists($data{$data})) || $name_vert){
                    my $scode = sprintf("u%x", $code);
                    print $fh "0$scode\t$data\n";
                    $map .= "$scode\t$name\n";
                    $code++;
                    $data{$data} = $name;
                } else {
                    if ($data{$data} ne $name) {
                        $alias .= "$name\t$data{$data}\n";
                    }
                }
            }
        }
    }
}

close $fh;

open my $fh, ">$fontname.alias";
print $fh $alias;
close $fh;

open my $fh, ">$fontname.map";
print $fh $map;
close $fh;

sub proc_ucs {
    my ($name,$fh) = @_;
    my $data = get_strokes_data($name);
    if(($data ne "") && (! $names{$name})) {
        print $fh "0$name\t$data\n";
        $data{$data} = $name;
        $names{$name} = 1;
    }
}

# Convert CDP characters to UCS and register to @data and $map.
sub proc_cdp {
    my ($name,$fh) = @_;
    my $data = get_strokes_data($name);
    if((hex(substr($name, 4, 4)) >= 0x854b) && (! $names{$name})) {
        $names{$name} = 1;
        my $h = hex(substr($name, 4, 2));
        my $l = hex(substr($name, 6, 2));
        my $code = 0xeeb8 + 157 * ($h - 0x81);
        if($l < 0x80){
            $code = $code + $l - 0x40;
        } else {
            $code = $code + $l - 0x62;
        }
        my $scode = sprintf("u%x", $code);
        if($data ne ""){
            print $fh "0$scode\t$data\n";
            $data{$data} = $name;
            $map .= "$scode\t$name\n";
        }
    }
}

# 第一引数が、第二引数の範囲リスト内にあるかをチェックする。
# 範囲内にある場合は 1 を、ない場合は 0 を返す。
sub check_regexps {
    my ($val) = @_;
    my $flag = 0;
    foreach my $regexp (@regexps) {
        if ($val =~ m/$regexp/) {$flag=1;}
    }
    return $flag;
}

# 部品データを得る。バージョンがついているかどうかを判別する
sub get_page{
    my ($wikiname) = @_;
    if($wikiname =~ m/\@/){ # バージョンつき
        $wikiname =~ s/\@/\\\@/g;
        return $dump2{$wikiname};
    } else { # バージョンなし。最新分を使う
        return $dump1{$wikiname};
    }
}

# `99' 部品を展開して、ストローク情報のみにする。
sub get_strokes_data{
    my ($name) = @_;
    my $buffer = &get_page($name);

    my @result = ();
    foreach(split(/\$/, $buffer)){
        if($_ =~ m/^99:/){
            my ($type, $sx, $sy, $x1, $y1, $x2, $y2, $name, $dummy, $sx2, $sy2) = split(/:/, $_);
            my $sub = &get_strokes_data($name);
            my ($minX,$minY,$maxX,$maxY) = get_box($name);
            if ($sx != 0 || $sy != 0) {
                if ($sx > 100) { # B mode
                    $sx -= 200;
                } else {         # A mode
                    $sx2 = 0;
                    $sy2 = 0;
                }
            }
            foreach(split(/\$/, $sub)){
                # 0を捨てる
                if($_ =~ m/^1:/){
                    my ($ntype, $sa, $sa2, $nx1, $ny1, $nx2, $ny2) = split(/:/, $_);
                    if ($sx != 0 || $sy != 0) {
                        $nx1 = stretch($sx,$sx2,$nx1,$minX,$maxX);
                        $ny1 = stretch($sy,$sy2,$ny1,$minY,$maxY);
                        $nx2 = stretch($sx,$sx2,$nx2,$minX,$maxX);
                        $ny2 = stretch($sy,$sy2,$ny2,$minY,$maxY);
                    }
                    push(@result,
                         join(':', ($ntype, $sa, $sa2,
                                    int($x1 + $nx1  * ($x2 - $x1) / 200),
                                    int($y1 + $ny1  * ($y2 - $y1) / 200),
                                    int($x1 + $nx2  * ($x2 - $x1) / 200),
                                    int($y1 + $ny2  * ($y2 - $y1) / 200))));
                } elsif($_ =~ m/^(2|3):/){
                    my ($ntype, $sa, $sa2, $nx1, $ny1, $nx2, $ny2, $nx3, $ny3) = split(/:/, $_);
                    if ($sx != 0 || $sy != 0) {
                        $nx1 = stretch($sx,$sx2,$nx1,$minX,$maxX);
                        $ny1 = stretch($sy,$sy2,$ny1,$minY,$maxY);
                        $nx2 = stretch($sx,$sx2,$nx2,$minX,$maxX);
                        $ny2 = stretch($sy,$sy2,$ny2,$minY,$maxY);
                        $nx3 = stretch($sx,$sx2,$nx3,$minX,$maxX);
                        $ny3 = stretch($sy,$sy2,$ny3,$minY,$maxY);
                    }
                    push(@result,
                         join(':', ($ntype, $sa, $sa2,
                                    int($x1 + $nx1  * ($x2 - $x1) / 200),
                                    int($y1 + $ny1  * ($y2 - $y1) / 200),
                                    int($x1 + $nx2  * ($x2 - $x1) / 200),
                                    int($y1 + $ny2  * ($y2 - $y1) / 200),
                                    int($x1 + $nx3  * ($x2 - $x1) / 200),
                                    int($y1 + $ny3  * ($y2 - $y1) / 200))));
                } elsif($_ =~ m/^(4|6|7):/){
                    my ($ntype, $sa, $sa2, $nx1, $ny1, $nx2, $ny2, $nx3, $ny3, $nx4, $ny4) = split(/:/, $_);
                    if ($sx != 0 || $sy != 0) {
                        $nx1 = stretch($sx,$sx2,$nx1,$minX,$maxX);
                        $ny1 = stretch($sy,$sy2,$ny1,$minY,$maxY);
                        $nx2 = stretch($sx,$sx2,$nx2,$minX,$maxX);
                        $ny2 = stretch($sy,$sy2,$ny2,$minY,$maxY);
                        $nx3 = stretch($sx,$sx2,$nx3,$minX,$maxX);
                        $ny3 = stretch($sy,$sy2,$ny3,$minY,$maxY);
                        $nx4 = stretch($sx,$sx2,$nx4,$minX,$maxX);
                        $ny4 = stretch($sy,$sy2,$ny4,$minY,$maxY);
                    }
                    push(@result,
                         join(':', ($ntype, $sa, $sa2,
                                    int($x1 + $nx1  * ($x2 - $x1) / 200),
                                    int($y1 + $ny1  * ($y2 - $y1) / 200),
                                    int($x1 + $nx2  * ($x2 - $x1) / 200),
                                    int($y1 + $ny2  * ($y2 - $y1) / 200),
                                    int($x1 + $nx3  * ($x2 - $x1) / 200),
                                    int($y1 + $ny3  * ($y2 - $y1) / 200),
                                    int($x1 + $nx4  * ($x2 - $x1) / 200),
                                    int($y1 + $ny4  * ($y2 - $y1) / 200))));
                }
            }
        } elsif($_ !~ m/^0:/) {
            push(@result, $_);
        }
    }
    return join('$', @result);
}

sub stretch {
    my ($dp,$sp,$p,$min,$max) = @_;
    my ($p1,$p2,$p3,$p4);
    if ($p < $sp + 100) {
        $p1 = $min;
        $p3 = $min;
        $p2 = $sp + 100;
        $p4 = $dp + 100;
    } else {
        $p1 = $sp + 100;
        $p3 = $dp + 100;
        $p2 = $max;
        $p4 = $max;
    }
    if ($p2 == $p1) {
        return $p; #stop streching
    }
    return POSIX::floor((($p - $p1) / ($p2 - $p1)) * ($p4 - $p3) + $p3);
}

sub get_box {
    my ($name) = @_;
    my $data = get_strokes_data($name);
    my $minX = 200;
    my $minY = 200;
    my $maxX = 0;
    my $maxY = 0;
    foreach(split(/\$/, $data)){
        if($_ =~ m/^1:/){
            my ($ntype, $sa, $sa2, $nx1, $ny1, $nx2, $ny2) = split(/:/, $_);
            $minX = min ($minX,$nx1,$nx2);
            $maxX = max ($maxX,$nx1,$nx2);
            $minY = min ($minY,$ny1,$ny2);
            $maxY = max ($maxY,$ny1,$ny2);
        } elsif($_ =~ m/^(2|3):/){
            my ($ntype, $sa, $sa2, $nx1, $ny1, $nx2, $ny2, $nx3, $ny3) = split(/:/, $_);
            $minX = min ($minX,$nx1,$nx2,$nx3);
            $maxX = max ($maxX,$nx1,$nx2,$nx3);
            $minY = min ($minY,$ny1,$ny2,$ny3);
            $maxY = max ($maxY,$ny1,$ny2,$ny3);
        } elsif($_ =~ m/^(4|6|7):/){
            my ($ntype, $sa, $sa2, $nx1, $ny1, $nx2, $ny2, $nx3, $ny3, $nx4, $ny4) = split(/:/, $_);
            $minX = min ($minX,$nx1,$nx2,$nx3,$nx4);
            $maxX = max ($maxX,$nx1,$nx2,$nx3,$nx4);
            $minY = min ($minY,$ny1,$ny2,$ny3,$ny4);
            $maxY = max ($maxY,$ny1,$ny2,$ny3,$ny4);
        }
    }
    return ($minX,$minY,$maxX,$maxY);
}
