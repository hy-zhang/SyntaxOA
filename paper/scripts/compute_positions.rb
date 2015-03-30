#require 'tempfile'
require 'fileutils'

# Example 1 - Read File and close
def scan(filename, beginpos, endpos, fileloc)
  File.open(filename, "r") do |file|
    counter = 1
    # TODO: use IO#each_line
    while line = file.gets
      if line =~ /BEGIN_(\w+)/
        raise "Symbol '#{$1}' already defined" if fileloc[$1]
        fileloc[$1] = filename
        beginpos[$1] = counter + 1
        puts "BEGIN #{$1}: #{beginpos[$1]}"
      end
      if line =~ /END_(\w+)/
        endpos[$1] = counter - 1
        puts "END #{$1}: #{endpos[$1]}"
      end
      counter += 1
    end
  end
end

def process(file, out, beginpos, endpos, fileloc)
  while line = file.gets
    if line =~ /APPLY:(\w+)=(\w+)/
      var = $1
      label = $2
      line = line.sub(/#{var}=([0-9-]*)/, "#{var}=#{beginpos[label]}-#{endpos[label]}")
      line = line.sub(/\{[^{]*\}/, "{#{fileloc[label]}}")
    end
    out.write(line)
  end
  file.close
  out.close
end

fileloc = {}
beginpos = {}
endpos = {}

Dir["../src/*.sf"].each do |file|
  scan(file, beginpos, endpos, fileloc)
end
#Dir['../srcscala/src/*/*.scala'].each do |file|
#  scan(file, beginpos, endpos, fileloc)
#end
#Dir['../src/*.java'].each do |file|
#  scan(file, beginpos, endpos, fileloc)
#end
#Dir['../src/*/*.java'].each do |file|
#  scan(file, beginpos, endpos, fileloc)
#end
#Dir['../code/*/*/*/*/*.java'].each do |file|
#  scan(file, beginpos, endpos, fileloc)
#end


(Dir["*.tex"] + Dir["sections/*.tex"]).each do |file|
  #temp = Tempfile.new('compute_positions')
  tempname = "footempfile.txt"
  temp = File.new(tempname, "w")
  process(File.new(file, "r"), temp, beginpos, endpos, fileloc)
  FileUtils.cp(file, "#{file}-old")
  FileUtils.cp(tempname, file)
end
