set term pdf 
set output "gauss.pdf"
set border 1
set xlabel ""
set xrange [ -3 : 3 ]
set ylabel ""
set yrange [ 0 : 0.6 ]

u = -0.3
s = 1.0
f(x)=exp(-(x-u)*(x-u)/s/s/2.0)/sqrt(pi)/s

p=f(u)

set arrow from u,p to u,0.0 nohead linestyle 1
set arrow from -1.5,f(-1.5) to -1.5,0 nohead linestyle 2
set arrow from -0.5,f(-0.5) to -0.5,0 nohead linestyle 2

set arrow from 1,p-0.2   to u,p-0.2 linestyle 1
set label "Reference time" at 1,p-0.2
set arrow from 1,f(-1)-0.15   to -1,f(-1)-0.15 linestyle 2
set label "P(Nov 20)" at 1,f(-1)-0.15


set xtics nomirror ("Nov 13" -2, "Nov 20" -1, "t" u, "" 0, "Dec 4" 1, "Dec 11" 2)
set ytics nomirror ("" 0)
plot  f(x) notitle with lines linestyle 3
