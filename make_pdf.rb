#!/usr/bin/env ruby
def gnuplot(str)
  open('tmp','w'){|fp|fp.puts str}
  `gnuplot tmp`
end


['center', 'frontier'].each{|fn|
  gnuplot <<GNUPLOT
set term postscript enhanced color 1
set out '#{fn}.eps'
unset tics
set border 0
set size ratio -1
plot \
  '#{fn}-galaxies.txt' w p pt 7 ps 0.3  lc rgb "#fff0f0" t '',\
  '#{fn}-stars.txt'    w p pt 7 ps 0.15 lc rgb "#d0ffd0" t '',\
  '#{fn}-grid.txt'     w p pt 7 ps 0.05 lc rgb "#0f0fff" t ''
GNUPLOT
}

['northpole', 'southpole'].each{|fn|
  gnuplot <<GNUPLOT
set term postscript enhanced color portrait 1
set out '#{fn}.eps'
unset tics
set border 0
set size ratio -1
plot \
  '#{fn}-galaxies.txt' w p pt 7 ps 0.3  lc rgb "#fff0f0" t '',\
  '#{fn}-stars.txt'    w p pt 7 ps 0.15  lc rgb "#d0ffd0" t '',\
  '#{fn}-grid.txt'     w p pt 7 ps 0.05 lc rgb "#0f0fff" t ''
GNUPLOT
}
