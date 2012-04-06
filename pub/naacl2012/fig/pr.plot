#!/usr/bin/gnuplot

set style line 3 linecolor rgb "blue" 

#(Basic)
set terminal pdf
set output "pr.pdf"
set xtics nomirror
set ytics nomirror
set border 3
set xrange [0:1]
set yrange [0:1]
set xlabel "Extent recall"
set ylabel "Value accuracy"
#(HEIDEL1)
#plot "< echo ‘0.82 0.85’" pointtype 1
set arrow from 0.80,0.70 to 0.82,0.85
set label 'HeidelTime1' at 0.63,0.70
#(HEIDEL2)
#plot "< echo ‘0.91 0.77’" pointtype 1
set arrow from 0.80,0.60 to 0.91,0.77
set label 'HeidelTime2' at 0.63,0.60
#(SUTIME)
set arrow from 0.80,0.50 to 0.96,0.82
set label 'SUTime' at 0.69,0.50
#(US)
plot "pr.dat" with lines title "OurSystem", \
	"< echo '0.96 0.82'" linestyle 3 notitle, \
	"< echo '0.91 0.77'" linestyle 3 notitle, \
	"< echo '0.82 0.85'" linestyle 3 notitle
