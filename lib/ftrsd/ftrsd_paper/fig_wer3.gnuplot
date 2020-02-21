# gnuplot script for "Percent copy" figure
# run: gnuplot fig_wer3.gnuplot
# then: pdflatex fig_wer3.tex
#
set term epslatex standalone size 6in,6*2/3in
set output "fig_wer3.tex"
set xlabel "SNR in 2500 Hz Bandwidth (dB)"
set ylabel "Percent Copy"
set style func linespoints
set key off
set tics in
set mxtics 2
set mytics 10 
set grid
set label "r6315" at -25.25,30
set label "r6330" at -26.0,30
set label "$T=10^5$" at -22.8,15
set label "$d=0.0$" at -22.8,10
plot [-27:-22] [0:110] \
     "ftdata-100000.dat" using 1:(100*$3) with linespoints lt 1 pt 7 title 'FT-100K', \
     "ftdata-100000.dat" using 1:(100*$2) with linespoints lt 1 pt 7 title 'FT-100K'
