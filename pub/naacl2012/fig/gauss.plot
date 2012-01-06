#(prettiness)
set term latex
set output "gauss.tex"
set border 1
set xlabel ""
set xrange [ -3 : 3 ]
set ylabel ""
set yrange [ 0 : 0.6 ]

#(params)
u = -0.3
s = 1.0
f(x)=exp(-(x-u)*(x-u)/s/s/2.0)/sqrt(pi)/s

p=f(u)

#(top label)
#set arrow from -1.5,p+0.02   to -0.5,p+0.02 linestyle 1 nohead
#set label "$P(\\texttt{Nov 20}) = \\int_{-1.5}^{-0.5} f(x)$" at -1.5,p+0.05

#(reference time)
set arrow from u,p to u,0.0 nohead linestyle 1
set arrow from 1,p-0.2   to u,p-0.2 linestyle 1
set label "Reference time" at 1,p-0.2

#(integral)
set arrow from -1.5,f(-1.5) to -1.5,0 nohead linestyle 2
set arrow from -0.5,f(-0.5) to -0.5,0 nohead linestyle 2
set arrow from -1.0,0    to -0.5,0.05  nohead linestyle 2
set arrow from -1.5,0    to -0.5,0.1  nohead linestyle 2
set arrow from -1.5,0.05 to -0.5,0.15 nohead linestyle 2
set arrow from -1.5,0.1  to -0.5,0.2  nohead linestyle 2
set arrow from -1.5,0.15 to -0.5,0.25 nohead linestyle 2
set arrow from -1.5,0.2  to -0.5,0.3  nohead linestyle 2
set arrow from -1.5,0.25 to -0.5,0.35 nohead linestyle 2
set arrow from -1.4,0.3  to -0.5,0.4  nohead linestyle 2
set arrow from -1.21,0.37 to -0.5,0.45 nohead linestyle 2
set arrow from -0.98,0.45 to -0.5,0.5 nohead linestyle 2
set arrow from 1,f(-1)-0.15   to -1,f(-1)-0.15 linestyle 2
set label "$P(\\texttt{11/20}) = \\int_{-1.5}^{-0.5} f(x)$" at 1,f(-1)-0.15

#(label)
set xtics nomirror ("\\begin{tabular}{c} \\\\ -2 \\\\ \\texttt{11/13} \\end{tabular}" -2, "\\begin{tabular}{c} \\\\ -1 \\\\ \\texttt{11/20} \\end{tabular}" -1,  "\\begin{tabular}{c} \\\\ -0.3 \\\\ $\\frac{t}{\\Delta_s}$ \\end{tabular}" u, "" 0, "\\begin{tabular}{c} \\\\ 1 \\\\ \\texttt{12/4} \\end{tabular}" 1, "\\begin{tabular}{c} \\\\ 2 \\\\ \\texttt{12/11} \\end{tabular}" 2)
#(no top tick)
set ytics nomirror ("" 0)
#(plot)
plot  f(x) notitle with lines linestyle 3
