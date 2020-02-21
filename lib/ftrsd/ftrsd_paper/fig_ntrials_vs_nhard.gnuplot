# gnuplot script for "ntrials_vs_nhard" figure
# run: gnuplot fig_ntrials_vs_nhard.gnuplot
# then: pdflatex fig_ntrials_vs_nhard.tex
#
set term epslatex standalone size 6in,2*6/3in
set output "fig_ntrials_vs_nhard.tex"
set xlabel "Errors in received word, $X$"
set ylabel "Number of trials"
set tics in
set mxtics 5
set mytics 10
#set grid
set logscale y
plot "stats-100000-24db-3.dat" using 1:4 pt 12 notitle
