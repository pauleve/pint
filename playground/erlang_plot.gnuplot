./r_plot_erlang > erlang-sa.pts
splot [0:2000] [0:4000] 'erlang-sa.pts' using 3:4:2 w pm3d
splot [0:200] [0:400] 'erlang-sa.pts' using 3:4:2 w pm3d, exp(6/((y/x)**0.75))
plot [0:] [0:10] 'erlang-sa.pts' u (column(4)/column(3)):2
plot [0:200] [0:100] 'erlang-r.pts' u (column(4)/column(3)):2, exp(6/(x**0.75))
./r_plot_erlang rate > erlang-r.pts
splot [0:50] [0:50] [0:1] 'erlang-r.pts' using 3:4:1 w pm3d
splot [0:50] [0:50] [0:1] 'erlang-r.pts' using 3:4:1 w pm3d, exp(-x) + (1/y)
