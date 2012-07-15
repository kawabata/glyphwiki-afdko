#!/usr/bin/perl

use utf8;

# USAGE: dumpucs.pl regexps <output>
# e.g. dumpucs.pl ^u[2-9][0-9a-f]{3},^uf[9a][0-9a-f]{3},^cdp,^kumimoji HanaMinA
# e.g. dumpucs.pl ^u2[0-9a-f]{4} HanaMinB

my @regexps = split(/,/,$ARGV[0]);

my $fontname = $ARGV[1];

# dumpデータの読み込み
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

open FH, "<$dump2";
while(<FH>){
  $_ =~ m/^ ([^\@]+\@[0-9]+) *\|[^\|]+\| (.+)\n$/;
  $dump2{$1} = $2;
}
close FH;

my $dbh = "dummy";

my %names;
my %data;
my $alias;
my $map;
my $code = 0xf0000;

my @x0213sip = (
0x2000B, 0x20089, 0x200A2, 0x200A4, 0x201A2, 0x20213, 0x2032B,
0x20371, 0x20381, 0x203F9, 0x2044A, 0x20509, 0x205D6, 0x20628,
0x2074F, 0x20807, 0x2083A, 0x208B9, 0x2097C, 0x2099D, 0x20AD3,
0x20B1D, 0x20B9F, 0x20D45, 0x20DE1, 0x20E64, 0x20E6D, 0x20E95,
0x20F5F, 0x21201, 0x2123D, 0x21255, 0x21274, 0x2127B, 0x212D7,
0x212E4, 0x212FD, 0x2131B, 0x21336, 0x21344, 0x213C4, 0x2146D,
0x2146E, 0x215D7, 0x21647, 0x216B4, 0x21706, 0x21742, 0x218BD,
0x219C3, 0x21C56, 0x21D2D, 0x21D45, 0x21D62, 0x21D78, 0x21D92,
0x21D9C, 0x21DA1, 0x21DB7, 0x21DE0, 0x21E33, 0x21E34, 0x21F1E,
0x21F76, 0x21FFA, 0x2217B, 0x22218, 0x2231E, 0x223AD, 0x226F3,
0x2285B, 0x228AB, 0x2298F, 0x22AB8, 0x22B46, 0x22B4F, 0x22B50,
0x22BA6, 0x22C1D, 0x22C24, 0x22DE1, 0x231B6, 0x231C3, 0x231C4,
0x231F5, 0x23372, 0x233D0, 0x233D2, 0x233D3, 0x233D5, 0x233DA,
0x233DF, 0x233E4, 0x2344A, 0x2344B, 0x23451, 0x23465, 0x234E4,
0x2355A, 0x23594, 0x235C4, 0x23638, 0x23639, 0x2363A, 0x23647,
0x2370C, 0x2371C, 0x2373F, 0x23763, 0x23764, 0x237E7, 0x237FF,
0x23824, 0x2383D, 0x23A98, 0x23C7F, 0x23CFE, 0x23D00, 0x23D0E,
0x23D40, 0x23DD3, 0x23DF9, 0x23DFA, 0x23F7E, 0x24096, 0x24103,
0x241C6, 0x241FE, 0x243BC, 0x24629, 0x246A5, 0x247F1, 0x24896,
0x24A4D, 0x24B56, 0x24B6F, 0x24C16, 0x24D14, 0x24E0E, 0x24E37,
0x24E6A, 0x24E8B, 0x2504A, 0x25055, 0x25122, 0x251A9, 0x251CD,
0x251E5, 0x2521E, 0x2524C, 0x2542E, 0x2548E, 0x254D9, 0x2550E,
0x255A7, 0x25771, 0x257A9, 0x257B4, 0x259C4, 0x259D4, 0x25AE3,
0x25AE4, 0x25AF1, 0x25BB2, 0x25C4B, 0x25C64, 0x25DA1, 0x25E2E,
0x25E56, 0x25E62, 0x25E65, 0x25EC2, 0x25ED8, 0x25EE8, 0x25F23,
0x25F5C, 0x25FD4, 0x25FE0, 0x25FFB, 0x2600C, 0x26017, 0x26060,
0x260ED, 0x26270, 0x26286, 0x2634C, 0x26402, 0x2667E, 0x266B0,
0x2671D, 0x268DD, 0x268EA, 0x26951, 0x2696F, 0x269DD, 0x26A1E,
0x26A58, 0x26A8C, 0x26AB7, 0x26AFF, 0x26C29, 0x26C73, 0x26CDD,
0x26E40, 0x26E65, 0x26F94, 0x26FF6, 0x26FF7, 0x26FF8, 0x270F4,
0x2710D, 0x27139, 0x273DA, 0x273DB, 0x273FE, 0x27410, 0x27449,
0x27614, 0x27615, 0x27631, 0x27684, 0x27693, 0x2770E, 0x27723,
0x27752, 0x27985, 0x27A84, 0x27BB3, 0x27BBE, 0x27BC7, 0x27CB8,
0x27DA0, 0x27E10, 0x27FB7, 0x2808A, 0x280BB, 0x28277, 0x28282,
0x282F3, 0x283CD, 0x2840C, 0x28455, 0x2856B, 0x285C8, 0x285C9,
0x286D7, 0x286FA, 0x28946, 0x28949, 0x2896B, 0x28987, 0x28988,
0x289BA, 0x289BB, 0x28A1E, 0x28A29, 0x28A43, 0x28A71, 0x28A99,
0x28ACD, 0x28ADD, 0x28AE4, 0x28BC1, 0x28BEF, 0x28D10, 0x28D71,
0x28DFB, 0x28E1F, 0x28E36, 0x28E89, 0x28EEB, 0x28F32, 0x28FF8,
0x292A0, 0x292B1, 0x29490, 0x295CF, 0x2967F, 0x296F0, 0x29719,
0x29750, 0x298C6, 0x29A72, 0x29DDB, 0x29E15, 0x29E3D, 0x29E49,
0x29E8A, 0x29EC4, 0x29EDB, 0x29EE9, 0x29FCE, 0x2A01A, 0x2A02F,
0x2A082, 0x2A0F9, 0x2A190, 0x2A38C, 0x2A437, 0x2A5F1, 0x2A602,
0x2A61A, 0x2A6B2);

my %x0213sip;
foreach(@x0213sip) {$x0213sip{$_}=1}

### kage, alias, map データの出力

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
if ($cdp) {
  foreach(sort(keys(%dump1))){
    my $name = $_;
    if($name =~ m/^cdp-[0-9a-f]{4}$/){
      proc_cdp($name,$fh);
    }
  }
}

# 最新データから、条件に適合するデータを抽出し、
# グリフが存在するなら alias へ、存在しないなら $map と@data へ登録。
foreach(sort(keys(%dump1))){
  my $name = $_;
  if (!($name =~ m/^u[0-9a-f]+$/) &&
      !($name =~ m/^cdp-[0-9a-f]+$/) && 
      ($name =~ m/^(kumimoji-)?(u([0-9a-f]+)|(cdp)-[0-9a-f]+)((?:-(?:u[0-9a-f]+|cdp-[0-9a-f]+))*)(-(?:j[av]|kp|us|[g-kmtuv])?(?:[01][0-9])?)?(-(?:var|itaiji)-[0-9]+)?(-vert)?$/)) {
    my $name_kumimoji = $1;
    my $name_ucs_val = hex($3);
    my $name_cdp = $4;
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
      my $data = get_strokes_data($dbh, $name);
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
  my $data = get_strokes_data($dbh, $name);
  if(($data ne "") && (! $names{$name})) {
    print $fh "0$name\t$data\n";
    $data{$data} = $name;
    $names{$name} = 1;
  }
}

# CDP文字をUCS符号に変換して、@dataと$mapに登録する。
sub proc_cdp {
  my ($name,$fh) = @_;
  my $data = get_strokes_data($dbh, $name);
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
    # X0213は特別扱い
    if (($regexp =~ m/^X0213/) &&
        ($val =~ m/^u(2[0-9a-f]{4})(?(?{$x0213sip{hex($1)}})|(?!))/)) {
        my $regexp_new = "^u2...." . substr($regexp,5);
        if ($val =~ m/$regexp_new/) {$flag=1;}}
    elsif ($val =~ m/$regexp/) {$flag=1;}
  }
  return $flag;
}

# 部品データを得る。バージョンがついているかどうかを判別する
sub get_page{
  my ($dbh, $wikiname) = @_;
  if($wikiname =~ m/\@/){ # バージョンつき
    return $dump2{$wikiname};
  } else { # バージョンなし。最新分を使う
    return $dump1{$wikiname};
  }
}

# `99' 部品を展開して、ストローク情報のみにする。
sub get_strokes_data{
  my ($dbh, $name) = @_;
  my $buffer = &get_page($dbh, $name);
  
  my @result = ();
  foreach(split(/\$/, $buffer)){
    if($_ =~ m/^99:/){
      my ($type, $a1, $a2, $x1, $y1, $x2, $y2, $name) = split(/:/, $_);
      my $sub = &get_strokes_data($dbh, $name);
      foreach(split(/\$/, $sub)){
        # 0を捨てる
        if($_ =~ m/^(1|8|9):/){
          my ($stype, $sa1, $sa2, $sx1, $sy1, $sx2, $sy2) = split(/:/, $_);
          push(@result, join(':', ($stype, $sa1, $sa2,
                         int($x1 + $sx1  * ($x2 - $x1) / 200),
                         int($y1 + $sy1  * ($y2 - $y1) / 200),
                         int($x1 + $sx2  * ($x2 - $x1) / 200),
                         int($y1 + $sy2  * ($y2 - $y1) / 200))));
        } elsif($_ =~ m/^(2|12|3):/){
          my ($stype, $sa1, $sa2, $sx1, $sy1, $sx2, $sy2, $sx3, $sy3) = split(/:/, $_);
          push(@result, join(':', ($stype, $sa1, $sa2,
                         int($x1 + $sx1  * ($x2 - $x1) / 200),
                         int($y1 + $sy1  * ($y2 - $y1) / 200),
                         int($x1 + $sx2  * ($x2 - $x1) / 200),
                         int($y1 + $sy2  * ($y2 - $y1) / 200),
                         int($x1 + $sx3  * ($x2 - $x1) / 200),
                         int($y1 + $sy3  * ($y2 - $y1) / 200))));
        } elsif($_ =~ m/^(4|6|7):/){
          my ($stype, $sa1, $sa2, $sx1, $sy1, $sx2, $sy2, $sx3, $sy3, $sx4, $sy4) = split(/:/, $_);
          push(@result, join(':', ($stype, $sa1, $sa2,
                         int($x1 + $sx1  * ($x2 - $x1) / 200),
                         int($y1 + $sy1  * ($y2 - $y1) / 200),
                         int($x1 + $sx2  * ($x2 - $x1) / 200),
                         int($y1 + $sy2  * ($y2 - $y1) / 200),
                         int($x1 + $sx3  * ($x2 - $x1) / 200),
                         int($y1 + $sy3  * ($y2 - $y1) / 200),
                         int($x1 + $sx4  * ($x2 - $x1) / 200),
                         int($y1 + $sy4  * ($y2 - $y1) / 200))));
        }
      }
    } elsif($_ !~ m/^0:/) {
      push(@result, $_);
    }
  }
  return join('$', @result);
}
