#! /usr/bin/ruby

buf = open("test-data").read

def find_it(buf)
  buf =~ /indecipherable|undecipherable/
end

len = find_it(buf)

start = Time.now
1000.times { find_it(buf)==len or raise }
puts "#{len} #{Time.now - start}"
