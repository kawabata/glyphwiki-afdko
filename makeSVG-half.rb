#!/usr/bin/env ruby
#encoding utf-8

require 'tempfile'

$hash = Hash.new(nil)

def load_hwg_data ()
  File.open("HalfwidthGlyphs.txt", :encoding => "UTF-8").each do |line|
    line.scan(/\[\[(.+?)\]\]/) {|gwname|
      $hash[gwname[0]]=true}
  end
end

def load_map_data (mapfile)
  File.open(mapfile).each do |line|
    line.scan(/^(u.+?)	(u.+)$/) {|code, gwname|
      if $hash[gwname]==true then $hash[code]=true end
    }
  end
end

def set_halfwidth(svgfile)
  Tempfile.open(".#{File.basename(svgfile)}", Dir.pwd) do |tempfile|
    File.open(svgfile).each do |line|
      if line =~ /glyph-name="(u[0-9a-f]+)"/ then
        if $hash[$1] == true then
          line.sub!(/" d="/,"\" horiz-adv-x=\"500\" d=\"")
        end
      end
      tempfile.puts line
    end
    tempfile.fdatasync
    tempfile.close
    stat = File.stat(svgfile)
    FileUtils.chown stat.uid, stat.gid, tempfile.path
    FileUtils.chmod stat.mode, tempfile.path
    FileUtils.mv tempfile.path, svgfile
  end
end

load_hwg_data()
load_map_data(ARGV[1])
set_halfwidth(ARGV[0])
