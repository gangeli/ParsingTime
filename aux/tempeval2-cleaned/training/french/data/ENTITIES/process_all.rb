#!/usr/bin/ruby

last_dct = nil
File.new("cand.dat").each_line do |line|
  items = line.split(/\t/)
  type = items[0]
  value = items[1]
  text = items[2].strip
  if text == "" then
    last_dct = value
  else
    raise "no dct '#{text}'" if not last_dct
    puts "#{type}\t#{value}\t#{last_dct}\t#{text}"
  end
end
