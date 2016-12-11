# From http://www.gnuplot.info/demo/pm3d.8.gnu

unset logscale
set view map scale 1
set samples 50, 50
set isosamples 50, 50
unset surface
set xrange [ -15.0000 : 15.0000 ] noreverse nowriteback
set yrange [ -15.0000 : 15.0000 ] noreverse nowriteback
set zrange [ -0.250000 : 1.00000 ] noreverse nowriteback
set pm3d implicit at b
splot 'pm3d.8.expected'
