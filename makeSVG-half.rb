#!/bin/env ruby -pine
if $_ =~ /<glyph unicode="&#x([0-9a-f]+);/ then
  code = $1.to_i(16)
  STDERR.puts code
  if (code.between?(     0,0x2007) ||
      code.between?(0x3130,0x319f) ||
      code.between?(0xff61,0xffdc) ||
      code.between?(0xffe8,0xffee)) then
    STDERR.puts "repl"
    $_.sub!(/" d="/,"\" horiz-adv-x=\"500\" d=\"")
  end
end
