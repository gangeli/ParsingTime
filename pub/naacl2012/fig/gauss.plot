set term latex
set output "gauss.tex"
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
set label "$P(\\texttt{Nov 20}) = \\int_{-1.5}^{1.5} f(x)$" at 1,f(-1)-0.15


set xtics nomirror ("\\begin{tabular}{c} \\\\ -2 \\\\ \\texttt{Nov 13} \\end{tabular}" -2, "\\begin{tabular}{c} \\\\ -1 \\\\ \\texttt{Nov 20} \\end{tabular}" -1,  "$\\frac{t}{\\Delta_s}$" u, "" 0, "\\begin{tabular}{c} \\\\ 1 \\\\ \\texttt{Dec 4} \\end{tabular}" 1, "\\begin{tabular}{c} \\\\ 2 \\\\ \\texttt{Dec 11} \\end{tabular}" 2)
set ytics nomirror ("" 0)
plot  f(x) notitle with lines linestyle 3
