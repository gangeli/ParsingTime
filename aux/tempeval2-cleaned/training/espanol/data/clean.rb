lastDoc = nil;
lastWord = nil;
lastTimex = nil;
File.new('timex-attributes.tab').each_line { |line| 
  fields = line.split(/\t/); 
  if fields[6] == "val" then
    puts "#{lastDoc} #{lastTimex} #{fields[4][1..-1].to_i} #{fields[2]}"
    if lastDoc and lastTimex and fields[0] == lastDoc and \
       fields[4][1..-1].to_i == lastTimex and \
       fields[2].to_i != lastWord + 1 then
      puts "#{lastDoc} #{lastTimex} #{fields[4][1..-1].to_i} #{fields[2]}"
    end
    if(lastDoc == fields[0])
      lastTimex = fields[4][1..-1].to_i; 
      lastWord = fields[2].to_i
    else
      lastDoc = fields[0]
      lastTimex = nil
      lastWord = nil
    end
  end
}
