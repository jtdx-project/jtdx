# run: gnuplot fig_compare.gnuplot
# then: pdflatex fig_compare.tex

set term epslatex standalone size 6in,2*6/3in
set output "fig_success.tex"
set xlabel "$E_b/N_0$ (dB)"
set ylabel "Decode Probability"
set style func linespoints
set key off
set tics in
set mxtics 2
set mytics 10 
set grid
#set logscale y
plot [-1:6] [1e-6:1] \
   "160-80-sf4.results" using 1:($2/1000000) with linespoints lt 2 lw 2 pt 2, \
   "ldpc-144-72-sf2.results" using 1:($2/1000000) with linespoints lt 3 lw 2 pt 5, \
   "jtmskcode.results" using 1:($4/1000000) with linespoints lt 1 lw 2 pt 7, \
   "success.lab" with labels
exit
#   "160-80-sf4.results" using 1:($4/1000000) with linespoints lt 3 lw 2 pt 4, \
