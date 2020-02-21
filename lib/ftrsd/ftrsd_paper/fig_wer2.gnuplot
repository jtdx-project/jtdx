# gnuplot script for "Percent copy" figure
# run: gnuplot fig_wer2.gnuplot
# then: pdflatex fig_wer2.tex
#
set term epslatex standalone size 6in,6*2/3in
set output "fig_wer2.tex"
set xlabel "SNR in 2500 Hz Bandwidth (dB)"
set ylabel "Percent Copy"
set style func linespoints
set key off
set tics in
set mxtics 2
set mytics 5
set grid
plot [-27:-22] [0:110] \
     "ftdata-100000.dat" using 1:(100*$3) with linespoints lt 1 lw 2 pt 7, \
     "ftdata-10000.dat" using 1:(100*$3) with linespoints lt 1 lw 2 pt 7, \
     "ftdata-1000.dat" using 1:(100*$3) with linespoints lt 1 lw 2 pt 7, \
     "ftdata-100.dat" using 1:(100*$3) with linespoints lt 1 lw 2 pt 7, \
     "ftdata-10.dat" using 1:(100*$2) with linespoints lt 1 lw 2 pt 7, \
     "kvasd-15.dat" using 1:(100*$2) with linespoints lt 4 pt 6, \
     "bmdata.dat" using 1:(100*$2) with linespoints lt 2 lw 2 pt 2, \
     "wer2.lab" with labels
