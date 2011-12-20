#!/bin/sh

mkdir cmd
mkdir lib
mkdir bin
mkdir doc
echo ''

if [ -r tree-tagger-3.2.tar.gz ]
then
    gzip -cd tree-tagger-3.2.tar.gz | tar -xf -
    echo 'SunOS version of TreeTagger installed.'
fi

if [ -r tree-tagger-linux-3.2.tar.gz ]
then
    gzip -cd tree-tagger-linux-3.2.tar.gz | tar -xf -
    echo 'Linux version of TreeTagger installed.'
fi

if [ -r tree-tagger-MacOSX-3.2.tar.gz ]
then
    gzip -cd tree-tagger-MacOSX-3.2.tar.gz | tar -xf -
    echo 'Mac OS-X version of TreeTagger for PowerPC installed.'
fi

if [ -r tree-tagger-MacOSX-3.2-intel.tar.gz ]
then
    gzip -cd tree-tagger-MacOSX-3.2-intel.tar.gz | tar -xf -
    echo 'Mac OS-X version of TreeTagger for Intel CPUs installed.'
fi

if [ -r tagger-scripts.tar.gz ] 
then
    gzip -cd tagger-scripts.tar.gz | tar -xf -
    chmod +x cmd/*
    echo 'Tagging scripts installed.'
fi

if [ -r german-par-3.2.bin.gz ]
then
    gzip -cd german-par-3.2.bin.gz > lib/german.par
    echo 'German parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r german-chunker-par-3.1.bin.gz ] 
then
    gzip -cd german-chunker-par-3.1.bin.gz > lib/german-chunker.par
    echo 'German chunker parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r english-par-3.2.bin.gz ]
then
    gzip -cd english-par-3.2.bin.gz > lib/english.par
    echo 'English parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r english-chunker-par-3.2.bin.gz ] 
then
    gzip -cd english-chunker-par-3.2.bin.gz > lib/english-chunker.par
    echo 'English chunker parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r french-par-3.2.bin.gz ]
then
    gzip -cd french-par-3.2.bin.gz > lib/french.par
    echo 'French parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r italian-par2-3.1.bin.gz ]
then
    gzip -cd italian-par2-3.1.bin.gz > lib/italian.par
    echo 'Italian parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r italian-par-3.1.bin.gz ]
then
    gzip -cd italian-par-3.1.bin.gz > lib/italian.par
    echo 'Italian parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r bulgarian-par-3.1.bin.gz ]
then
    gzip -cd bulgarian-par-3.1.bin.gz > lib/bulgarian.par
    echo 'Bulgarian parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r spanish-par-3.1.bin.gz ] 
then
    gzip -cd spanish-par-3.1.bin.gz > lib/spanish.par
    echo 'Spanish parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r dutch-par-3.1.bin.gz ] 
then
    gzip -cd dutch-par-3.1.bin.gz > lib/dutch.par
    echo 'Dutch parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r dutch2-par-3.1.bin.gz ] 
then
    gzip -cd dutch2-par-3.1.bin.gz > lib/dutch.par
    echo 'Dutch parameter file (SunOS, MacOS-X) installed.'
fi

if [ -r german-par-linux-3.2.bin.gz ]
then
    gzip -cd german-par-linux-3.2.bin.gz > lib/german.par
    echo 'German parameter file (Linux) installed.'
fi

if [ -r estonian-par-linux-3.2.bin.gz ] 
then
    gzip -cd estonian-par-linux-3.2.bin.gz > lib/estonian.par
    echo 'Estonian parameter file (Linux) installed.'
fi

if [ -r german-par-linux-3.2-utf8.bin.gz ]
then
    gzip -cd german-par-linux-3.2-utf8.bin.gz > lib/german-utf8.par
    echo 'German parameter file (Linux, UTF8) installed.'
fi

if [ -r german-chunker-par-linux-3.1.bin.gz ] 
then
    gzip -cd german-chunker-par-linux-3.1.bin.gz > lib/german-chunker.par
    echo 'German chunker parameter file (Linux) installed.'
fi

if [ -r german-chunker-par-linux-3.2-utf8.bin.gz ] 
then
    gzip -cd german-chunker-par-linux-3.2-utf8.bin.gz > lib/german-chunker-utf8.par
    echo 'German chunker parameter file (Linux) installed.'
fi

if [ -r english-par-linux-3.2.bin.gz ]
then
    gzip -cd english-par-linux-3.2.bin.gz > lib/english.par
    echo 'English parameter file (Linux) installed.'
fi

if [ -r english-chunker-par-linux-3.2.bin.gz ] 
then
    gzip -cd english-chunker-par-linux-3.2.bin.gz > lib/english-chunker.par
    echo 'English chunker parameter file (Linux) installed.'
fi

if [ -r french-par-linux-3.2.bin.gz ]
then
    gzip -cd french-par-linux-3.2.bin.gz > lib/french.par
    echo 'French parameter file (Linux, Latin1) installed.'
fi

if [ -r french-par-linux-3.2-utf8.bin.gz ]
then
    gzip -cd french-par-linux-3.2-utf8.bin.gz > lib/french-utf8.par
    echo 'French parameter file (Linux, UTF8) installed.'
fi

if [ -r french-chunker-par-linux-3.1.bin.gz ] 
then
    gzip -cd french-chunker-par-linux-3.1.bin.gz > lib/french-chunker.par
    echo 'French chunker parameter file (Linux) installed.'
fi

if [ -r french-chunker-par-linux-3.2-utf8.bin.gz ] 
then
    gzip -cd french-chunker-par-linux-3.2-utf8.bin.gz > lib/french-chunker-utf8.par
    echo 'French chunker parameter file (Linux, UTF8) installed.'
fi

if [ -r italian-par2-linux-3.1.bin.gz ]
then
    gzip -cd italian-par2-linux-3.1.bin.gz > lib/italian.par
    echo 'Italian parameter file (Linux) installed.'
fi

if [ -r italian-par-linux-3.1.bin.gz ]
then
    gzip -cd italian-par-linux-3.1.bin.gz > lib/italian.par
    echo 'Italian parameter file (Linux) installed.'
fi

if [ -r italian-par-linux-3.2-utf8.bin.gz ]
then
    gzip -cd italian-par-linux-3.2-utf8.bin.gz > lib/italian-utf8.par
    echo 'Italian parameter file (Linux, UTF8) installed.'
fi

if [ -r bulgarian-par-linux-3.1.bin.gz ]
then
    gzip -cd bulgarian-par-linux-3.1.bin.gz > lib/bulgarian.par
    echo 'Bulgarian parameter file (Linux) installed.'
fi

if [ -r russian-par-linux-3.1.bin.gz ]
then
    gzip -cd russian-par-linux-3.1.bin.gz > lib/russian.par
    echo 'Russian parameter file (Linux) installed.'
fi

if [ -r spanish-par-linux-3.2.bin.gz ]
then
    gzip -cd spanish-par-linux-3.2.bin.gz > lib/spanish.par
    echo 'Spanish parameter file (Linux) installed.'
fi

if [ -r spanish-par-linux-3.2-utf8.bin.gz ]
then
    gzip -cd spanish-par-linux-3.2-utf8.bin.gz > lib/spanish-utf8.par
    echo 'Spanish parameter file (Linux, UTF8) installed.'
fi

if [ -r dutch-par-linux-3.1.bin.gz ]
then
    gzip -cd dutch-par-linux-3.1.bin.gz > lib/dutch.par
    echo 'Dutch parameter file (Linux) installed.'
fi

if [ -r dutch2-par-linux-3.1.bin.gz ]
then
    gzip -cd dutch2-par-linux-3.1.bin.gz > lib/dutch.par
    echo 'Dutch parameter file (Linux) installed.'
fi

if [ -r greek-par-linux-3.2.bin.gz ]
then
    gzip -cd greek-par-linux-3.2.bin.gz > lib/greek.par
    echo 'Greek parameter file (Linux) installed.'
fi

if [ -r greek-par-linux-3.2-utf8.bin.gz ]
then
    gzip -cd greek-par-linux-3.2-utf8.bin.gz > lib/greek-utf8.par
    echo 'Greek parameter file (Linux, UTF8) installed.'
fi

if [ -r swahili-par-linux-3.2.bin.gz ]
then
    gzip -cd swahili-par-linux-3.2.bin.gz > lib/swahili.par
    echo 'Swahili parameter file (Linux) installed.'
fi

if [ -r latin-par-linux-3.2.bin.gz ]
then
    gzip -cd latin-par-linux-3.2.bin.gz > lib/latin.par
    echo 'Latin parameter file (Linux, Latin1) installed.'
fi

for file in cmd/*
do
    awk '$0=="BIN=./bin"{print "BIN='`pwd`'/bin";next}\
         $0=="CMD=./cmd"{print "CMD='`pwd`'/cmd";next}\
         $0=="LIB=./lib"{print "LIB='`pwd`'/lib";next}\
         {print}' $file > $file.tmp;
    mv $file.tmp $file;
done
echo 'Path variables modified in tagging scripts.'

chmod 0755 cmd/*

echo ''
echo 'You might want to add '`pwd`'/cmd and '`pwd`'/bin to the PATH variable so that you do not need to specify the full path to run the tagging scripts.'
echo ''
