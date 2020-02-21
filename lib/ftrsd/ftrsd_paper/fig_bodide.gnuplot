# gnuplot script for comparison to theoretical word error rate for
# bounded distance decoding 

# run: gnuplot fig_bodide.gnuplot
# then: pdflatex fig_bodide.tex

set term epslatex standalone size 6in,2*6/3in
set output "fig_bodide.tex"
set xlabel "$E_b/N_0$ (dB)"
set ylabel "Word Error Rate"
set style func linespoints
set key off
set tics in
set mxtics 2
set mytics 10 
set grid
set logscale y
plot [3:9] [1e-4:1] \
   "bmdata.dat" using ($1+29.1):(1-$2) with linespoints lt 2 lw 2 pt 2, \
   "ftdata-100000.dat" using ($1+29.1):(1-$3) with linespoints lt 1 lw 2 pt 7, \
   "bmtheory25.dat" using ($1-0.6):3 with linespoints lt 1 pt 5, \
   "kvasd-15.dat" using ($1+29.1):(1-$3) with linespoints lt 4 pt 6, \
   "bodide.lab" with labels

