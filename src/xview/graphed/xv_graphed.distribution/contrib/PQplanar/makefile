# (C) Universitaet Passau, Michael Himsolt 1986-1991
#########################################################################
#									#
#			    GRAPHED MAKEFILE				#
#									#
#########################################################################


#
# Directories
#
GRAPHEDDIR = ../..
#
# Sourcefiles
#
CSOURCES = main.c uwe.c adj.c dfs.c dumpgr.c floatuti.c listen.c listen1.c neck.c planar.c \
           plzeich.c pqtest.c pqtree.c pqtreehi.c sgraphhi.c stnumber.c  \
	   gauss.c simplex.c simplexh.c mtx_auf.c mtx_kore.c mtx_test.c mtx_stat.c \
	   simptrei.c zeichsim.c  interfac.c PQ_triang.c edgelist.c necklist.c plan_sf.c \
           kan_sort.c simp_mps.c file.c local_op.c local_sf.c
HSOURCES = main.h adj.h dfs.h listen.h listen1.h path.h planar.h plzeich.h  \
           pqtree.h pqtreehi.h sgraphhi.h simplex.h simplexh.h simptrei.h gauss.h dfs.h \
	   interfac.h triang.h edgelist.h necklist.h plan_sf.h kan_sort.h simp_mps.h \
           local_sf.h local_op.h zeichsim.h stnumber.h modula.h neck.h \
           mtx_auf.h mtx_test.h
MYFILES = $(HSOURCES) $(CSOURCES)

MODULE = PQplanar

include ../makefile.template
include $(GRAPHEDDIR)/makefile.template


# DO NOT DELETE THIS LINE -- make depend depends on it.

main.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
main.o: /usr/include/malloc.h ../../sgraph/sgraph.h ../../sgraph/slist.h
main.o: ../../sgraph/graphed.h ../../sgraph/graphed_structures.h
main.o: /usr/openwin/include/xview/rect.h
main.o: /usr/openwin/include/xview/xv_c_types.h
main.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
main.o: ../../sgraph/sgragra_interface.h listen.h listen1.h sgraphhi.h
main.o: modula.h main.h adj.h interfac.h stnumber.h plan_sf.h local_sf.h
main.o: ../../sgraph/algorithms.h
uwe.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
uwe.o: /usr/include/malloc.h ../../sgraph/sgraph.h listen1.h listen.h
uwe.o: modula.h sgraphhi.h dfs.h plzeich.h stnumber.h
adj.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
adj.o: /usr/include/malloc.h ../../sgraph/sgraph.h modula.h listen1.h adj.h
adj.o: sgraphhi.h interfac.h
dfs.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
dfs.o: /usr/include/malloc.h ../../sgraph/sgraph.h ../../sgraph/slist.h
dfs.o: ../../sgraph/graphed.h ../../sgraph/graphed_structures.h
dfs.o: /usr/openwin/include/xview/rect.h
dfs.o: /usr/openwin/include/xview/xv_c_types.h
dfs.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
dfs.o: ../../sgraph/sgragra_interface.h modula.h listen.h listen1.h
dfs.o: sgraphhi.h adj.h dfs.h plzeich.h interfac.h edgelist.h stnumber.h
dumpgr.o: path.h ../../sgraph/std.h /usr/include/assert.h
dumpgr.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
dumpgr.o: modula.h pqtree.h
floatuti.o: path.h ../../sgraph/std.h /usr/include/assert.h
floatuti.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
floatuti.o: modula.h /usr/include/math.h /usr/include/floatingpoint.h
floatuti.o: /usr/include/sys/ieeefp.h
listen.o: path.h ../../sgraph/std.h /usr/include/assert.h
listen.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
listen.o: modula.h listen.h
listen1.o: path.h ../../sgraph/std.h /usr/include/assert.h
listen1.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
listen1.o: listen1.h modula.h
neck.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
neck.o: /usr/include/malloc.h ../../sgraph/sgraph.h modula.h listen.h
neck.o: listen1.h adj.h interfac.h edgelist.h plzeich.h sgraphhi.h simplex.h
neck.o: neck.h necklist.h plan_sf.h /usr/include/math.h
neck.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
planar.o: path.h ../../sgraph/std.h /usr/include/assert.h
planar.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
planar.o: ../../sgraph/slist.h ../../sgraph/graphed.h
planar.o: ../../sgraph/graphed_structures.h /usr/openwin/include/xview/rect.h
planar.o: /usr/openwin/include/xview/xv_c_types.h
planar.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
planar.o: ../../sgraph/sgragra_interface.h modula.h listen.h planar.h
planar.o: pqtree.h pqtreehi.h sgraphhi.h listen1.h adj.h interfac.h
planar.o: stnumber.h
plzeich.o: path.h ../../sgraph/std.h /usr/include/assert.h
plzeich.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
plzeich.o: modula.h listen.h listen1.h adj.h interfac.h edgelist.h plzeich.h
plzeich.o: gauss.h sgraphhi.h simplex.h simplexh.h simptrei.h neck.h
plzeich.o: necklist.h triang.h plan_sf.h kan_sort.h stnumber.h
plzeich.o: /usr/include/math.h /usr/include/floatingpoint.h
plzeich.o: /usr/include/sys/ieeefp.h
pqtest.o: path.h ../../sgraph/std.h /usr/include/assert.h
pqtest.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
pqtest.o: modula.h pqtree.h
pqtree.o: path.h ../../sgraph/std.h /usr/include/assert.h
pqtree.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
pqtree.o: listen.h modula.h planar.h pqtree.h pqtreehi.h listen1.h adj.h
pqtree.o: interfac.h
pqtreehi.o: path.h ../../sgraph/std.h /usr/include/assert.h
pqtreehi.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
pqtreehi.o: listen.h modula.h planar.h pqtree.h pqtreehi.h listen1.h adj.h
pqtreehi.o: dfs.h interfac.h edgelist.h
sgraphhi.o: path.h ../../sgraph/std.h /usr/include/assert.h
sgraphhi.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
sgraphhi.o: ../../sgraph/slist.h ../../sgraph/graphed.h
sgraphhi.o: ../../sgraph/graphed_structures.h
sgraphhi.o: /usr/openwin/include/xview/rect.h
sgraphhi.o: /usr/openwin/include/xview/xv_c_types.h
sgraphhi.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
sgraphhi.o: ../../sgraph/sgragra_interface.h modula.h listen1.h edgelist.h
sgraphhi.o: adj.h interfac.h sgraphhi.h
stnumber.o: path.h ../../sgraph/std.h /usr/include/assert.h
stnumber.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
stnumber.o: modula.h listen.h plan_sf.h stnumber.h sgraphhi.h
gauss.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
gauss.o: /usr/include/malloc.h ../../sgraph/sgraph.h modula.h listen.h
gauss.o: listen1.h dfs.h plzeich.h sgraphhi.h simptrei.h /usr/include/math.h
gauss.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
simplex.o: path.h ../../sgraph/std.h /usr/include/assert.h
simplex.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
simplex.o: modula.h simplex.h simptrei.h
simplexh.o: path.h ../../sgraph/std.h /usr/include/assert.h
simplexh.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
simplexh.o: modula.h listen.h listen1.h edgelist.h plzeich.h sgraphhi.h
simplexh.o: simplexh.h neck.h necklist.h adj.h interfac.h mtx_auf.h
simplexh.o: mtx_test.h plan_sf.h simptrei.h /usr/include/math.h
simplexh.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
mtx_auf.o: path.h ../../sgraph/std.h /usr/include/assert.h
mtx_auf.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
mtx_auf.o: modula.h listen.h listen1.h dfs.h adj.h interfac.h plzeich.h
mtx_auf.o: sgraphhi.h simplexh.h simptrei.h edgelist.h neck.h necklist.h
mtx_auf.o: /usr/include/math.h /usr/include/floatingpoint.h
mtx_auf.o: /usr/include/sys/ieeefp.h
mtx_kore.o: path.h ../../sgraph/std.h /usr/include/assert.h
mtx_kore.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
mtx_kore.o: modula.h listen1.h dfs.h plzeich.h sgraphhi.h simplexh.h
mtx_kore.o: simptrei.h /usr/include/math.h /usr/include/floatingpoint.h
mtx_kore.o: /usr/include/sys/ieeefp.h
mtx_test.o: path.h ../../sgraph/std.h /usr/include/assert.h
mtx_test.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
mtx_test.o: modula.h listen.h listen1.h dfs.h plzeich.h sgraphhi.h simplexh.h
mtx_test.o: simptrei.h /usr/include/math.h /usr/include/floatingpoint.h
mtx_test.o: /usr/include/sys/ieeefp.h
mtx_stat.o: path.h ../../sgraph/std.h /usr/include/assert.h
mtx_stat.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
mtx_stat.o: modula.h simplex.h
simptrei.o: path.h ../../sgraph/std.h /usr/include/assert.h
simptrei.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
simptrei.o: modula.h listen.h listen1.h dfs.h plzeich.h sgraphhi.h simplex.h
simptrei.o: simplexh.h simptrei.h plan_sf.h /usr/include/math.h
simptrei.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
zeichsim.o: path.h ../../sgraph/std.h /usr/include/assert.h
zeichsim.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
zeichsim.o: modula.h listen.h listen1.h adj.h interfac.h dfs.h plzeich.h
zeichsim.o: gauss.h sgraphhi.h mtx_auf.h mtx_test.h simptrei.h
zeichsim.o: /usr/include/math.h /usr/include/floatingpoint.h
zeichsim.o: /usr/include/sys/ieeefp.h
interfac.o: path.h ../../sgraph/std.h /usr/include/assert.h
interfac.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
interfac.o: ../../sgraph/slist.h ../../sgraph/graphed.h
interfac.o: ../../sgraph/graphed_structures.h
interfac.o: /usr/openwin/include/xview/rect.h
interfac.o: /usr/openwin/include/xview/xv_c_types.h
interfac.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
interfac.o: ../../sgraph/sgragra_interface.h sgraphhi.h modula.h listen.h
interfac.o: listen1.h adj.h edgelist.h neck.h necklist.h plan_sf.h interfac.h
interfac.o: kan_sort.h plzeich.h local_op.h /usr/include/math.h
interfac.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
PQ_triang.o: path.h ../../sgraph/std.h /usr/include/assert.h
PQ_triang.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
PQ_triang.o: modula.h listen.h listen1.h edgelist.h plzeich.h sgraphhi.h
PQ_triang.o: simplex.h simplexh.h neck.h adj.h interfac.h necklist.h
PQ_triang.o: plan_sf.h triang.h /usr/include/math.h
PQ_triang.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
edgelist.o: path.h ../../sgraph/std.h /usr/include/assert.h
edgelist.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
edgelist.o: modula.h listen.h listen1.h dfs.h plzeich.h sgraphhi.h simplex.h
edgelist.o: simplexh.h edgelist.h adj.h neck.h necklist.h interfac.h
edgelist.o: /usr/include/math.h /usr/include/floatingpoint.h
edgelist.o: /usr/include/sys/ieeefp.h
necklist.o: path.h ../../sgraph/std.h /usr/include/assert.h
necklist.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
necklist.o: modula.h listen.h listen1.h edgelist.h plzeich.h gauss.h
necklist.o: sgraphhi.h simplex.h neck.h necklist.h plan_sf.h adj.h interfac.h
necklist.o: /usr/include/math.h /usr/include/floatingpoint.h
necklist.o: /usr/include/sys/ieeefp.h
plan_sf.o: /usr/openwin/include/xview/xview.h
plan_sf.o: /usr/openwin/include/xview/xview_xvin.h /usr/include/signal.h
plan_sf.o: /usr/include/sys/signal.h /usr/include/vm/faultcode.h
plan_sf.o: /usr/include/sys/types.h /usr/include/sys/stdtypes.h
plan_sf.o: /usr/include/sys/sysmacros.h /usr/include/pixrect/pixrect.h
plan_sf.o: /usr/include/pixrect/pr_planegroups.h
plan_sf.o: /usr/include/pixrect/pr_util.h /usr/include/pixrect/memvar.h
plan_sf.o: /usr/include/pixrect/pixfont.h /usr/include/pixrect/traprop.h
plan_sf.o: /usr/include/pixrect/pr_line.h
plan_sf.o: /usr/openwin/include/xview/xv_c_types.h
plan_sf.o: /usr/openwin/include/xview/generic.h
plan_sf.o: /usr/openwin/include/xview/pkg_public.h
plan_sf.o: /usr/openwin/include/xview/pkg.h /usr/openwin/include/xview/attr.h
plan_sf.o: /usr/openwin/include/xview/base.h /usr/include/string.h
plan_sf.o: /usr/include/malloc.h /usr/openwin/include/xview/notify.h
plan_sf.o: /usr/include/stdio.h /usr/include/sys/wait.h
plan_sf.o: /usr/include/sys/time.h /usr/include/sys/time.h
plan_sf.o: /usr/include/sys/resource.h /usr/include/varargs.h
plan_sf.o: /usr/openwin/include/xview/xv_error.h
plan_sf.o: /usr/openwin/include/xview/sun.h
plan_sf.o: /usr/openwin/include/xview/server.h
plan_sf.o: /usr/openwin/include/xview/screen.h
plan_sf.o: /usr/openwin/include/xview/pixwin.h
plan_sf.o: /usr/openwin/include/xview/rect.h
plan_sf.o: /usr/openwin/include/xview/rectlist.h
plan_sf.o: /usr/openwin/include/xview/win_input.h
plan_sf.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
plan_sf.o: /usr/openwin/include/xview/win_event.h
plan_sf.o: /usr/openwin/include/xview/xv_version.h
plan_sf.o: /usr/openwin/include/xview/icon.h
plan_sf.o: /usr/openwin/include/xview/window.h
plan_sf.o: /usr/openwin/include/xview/drawable.h
plan_sf.o: /usr/openwin/include/xview/attrol.h
plan_sf.o: /usr/openwin/include/xview/svrimage.h
plan_sf.o: /usr/openwin/include/xview/frame.h
plan_sf.o: /usr/openwin/include/xview/openmenu.h
plan_sf.o: /usr/openwin/include/xview/panel.h
plan_sf.o: /usr/openwin/include/xview/canvas.h
plan_sf.o: /usr/openwin/include/xview/openwin.h ../../sgraph/std.h
plan_sf.o: /usr/include/assert.h ../../sgraph/sgraph.h modula.h main.h
plan_sf.o: plan_sf.h
kan_sort.o: path.h ../../sgraph/std.h /usr/include/assert.h
kan_sort.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
kan_sort.o: ../../sgraph/slist.h ../../sgraph/graphed.h
kan_sort.o: ../../sgraph/graphed_structures.h
kan_sort.o: /usr/openwin/include/xview/rect.h
kan_sort.o: /usr/openwin/include/xview/xv_c_types.h
kan_sort.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
kan_sort.o: ../../sgraph/sgragra_interface.h sgraphhi.h modula.h listen.h
kan_sort.o: listen1.h adj.h dfs.h edgelist.h neck.h necklist.h stnumber.h
kan_sort.o: plzeich.h plan_sf.h interfac.h kan_sort.h /usr/include/math.h
kan_sort.o: /usr/include/floatingpoint.h /usr/include/sys/ieeefp.h
simp_mps.o: path.h ../../sgraph/std.h /usr/include/assert.h
simp_mps.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
simp_mps.o: ../../sgraph/slist.h ../../sgraph/graphed.h
simp_mps.o: ../../sgraph/graphed_structures.h
simp_mps.o: /usr/openwin/include/xview/rect.h
simp_mps.o: /usr/openwin/include/xview/xv_c_types.h
simp_mps.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
simp_mps.o: ../../sgraph/sgragra_interface.h modula.h simptrei.h
simp_mps.o: /usr/include/math.h /usr/include/floatingpoint.h
simp_mps.o: /usr/include/sys/ieeefp.h /usr/openwin/include/xview/xview.h
simp_mps.o: /usr/openwin/include/xview/xview_xvin.h /usr/include/signal.h
simp_mps.o: /usr/include/sys/signal.h /usr/include/vm/faultcode.h
simp_mps.o: /usr/include/sys/types.h /usr/include/sys/stdtypes.h
simp_mps.o: /usr/include/sys/sysmacros.h /usr/include/pixrect/pixrect.h
simp_mps.o: /usr/include/pixrect/pr_planegroups.h
simp_mps.o: /usr/include/pixrect/pr_util.h /usr/include/pixrect/memvar.h
simp_mps.o: /usr/include/pixrect/pixfont.h /usr/include/pixrect/traprop.h
simp_mps.o: /usr/include/pixrect/pr_line.h
simp_mps.o: /usr/openwin/include/xview/generic.h
simp_mps.o: /usr/openwin/include/xview/pkg_public.h
simp_mps.o: /usr/openwin/include/xview/pkg.h
simp_mps.o: /usr/openwin/include/xview/attr.h
simp_mps.o: /usr/openwin/include/xview/base.h /usr/include/string.h
simp_mps.o: /usr/openwin/include/xview/notify.h /usr/include/sys/wait.h
simp_mps.o: /usr/include/sys/time.h /usr/include/sys/time.h
simp_mps.o: /usr/include/sys/resource.h /usr/include/varargs.h
simp_mps.o: /usr/openwin/include/xview/xv_error.h
simp_mps.o: /usr/openwin/include/xview/sun.h
simp_mps.o: /usr/openwin/include/xview/server.h
simp_mps.o: /usr/openwin/include/xview/screen.h
simp_mps.o: /usr/openwin/include/xview/pixwin.h
simp_mps.o: /usr/openwin/include/xview/rectlist.h
simp_mps.o: /usr/openwin/include/xview/win_input.h
simp_mps.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
simp_mps.o: /usr/openwin/include/xview/win_event.h
simp_mps.o: /usr/openwin/include/xview/xv_version.h
simp_mps.o: /usr/openwin/include/xview/icon.h
simp_mps.o: /usr/openwin/include/xview/window.h
simp_mps.o: /usr/openwin/include/xview/drawable.h
simp_mps.o: /usr/openwin/include/xview/attrol.h
simp_mps.o: /usr/openwin/include/xview/svrimage.h
simp_mps.o: /usr/openwin/include/xview/frame.h
simp_mps.o: /usr/openwin/include/xview/openmenu.h
file.o: path.h ../../sgraph/std.h /usr/include/assert.h /usr/include/stdio.h
file.o: /usr/include/malloc.h ../../sgraph/sgraph.h ../../sgraph/slist.h
file.o: ../../sgraph/graphed.h ../../sgraph/graphed_structures.h
file.o: /usr/openwin/include/xview/rect.h
file.o: /usr/openwin/include/xview/xv_c_types.h
file.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
file.o: ../../sgraph/sgragra_interface.h modula.h
local_op.o: path.h ../../sgraph/std.h /usr/include/assert.h
local_op.o: /usr/include/stdio.h /usr/include/malloc.h ../../sgraph/sgraph.h
local_op.o: ../../sgraph/slist.h ../../sgraph/graphed.h
local_op.o: ../../sgraph/graphed_structures.h
local_op.o: /usr/openwin/include/xview/rect.h
local_op.o: /usr/openwin/include/xview/xv_c_types.h
local_op.o: ../../sgraph/dispatch_commands.h ../../sgraph/sgraph_interface.h
local_op.o: ../../sgraph/sgragra_interface.h sgraphhi.h modula.h listen.h
local_op.o: listen1.h adj.h edgelist.h interfac.h kan_sort.h local_sf.h
local_op.o: /usr/include/math.h /usr/include/floatingpoint.h
local_op.o: /usr/include/sys/ieeefp.h
local_sf.o: /usr/openwin/include/xview/xview.h
local_sf.o: /usr/openwin/include/xview/xview_xvin.h /usr/include/signal.h
local_sf.o: /usr/include/sys/signal.h /usr/include/vm/faultcode.h
local_sf.o: /usr/include/sys/types.h /usr/include/sys/stdtypes.h
local_sf.o: /usr/include/sys/sysmacros.h /usr/include/pixrect/pixrect.h
local_sf.o: /usr/include/pixrect/pr_planegroups.h
local_sf.o: /usr/include/pixrect/pr_util.h /usr/include/pixrect/memvar.h
local_sf.o: /usr/include/pixrect/pixfont.h /usr/include/pixrect/traprop.h
local_sf.o: /usr/include/pixrect/pr_line.h
local_sf.o: /usr/openwin/include/xview/xv_c_types.h
local_sf.o: /usr/openwin/include/xview/generic.h
local_sf.o: /usr/openwin/include/xview/pkg_public.h
local_sf.o: /usr/openwin/include/xview/pkg.h
local_sf.o: /usr/openwin/include/xview/attr.h
local_sf.o: /usr/openwin/include/xview/base.h /usr/include/string.h
local_sf.o: /usr/include/malloc.h /usr/openwin/include/xview/notify.h
local_sf.o: /usr/include/stdio.h /usr/include/sys/wait.h
local_sf.o: /usr/include/sys/time.h /usr/include/sys/time.h
local_sf.o: /usr/include/sys/resource.h /usr/include/varargs.h
local_sf.o: /usr/openwin/include/xview/xv_error.h
local_sf.o: /usr/openwin/include/xview/sun.h
local_sf.o: /usr/openwin/include/xview/server.h
local_sf.o: /usr/openwin/include/xview/screen.h
local_sf.o: /usr/openwin/include/xview/pixwin.h
local_sf.o: /usr/openwin/include/xview/rect.h
local_sf.o: /usr/openwin/include/xview/rectlist.h
local_sf.o: /usr/openwin/include/xview/win_input.h
local_sf.o: /usr/openwin/include/X11/Xlib.h /usr/openwin/include/X11/X.h
local_sf.o: /usr/openwin/include/xview/win_event.h
local_sf.o: /usr/openwin/include/xview/xv_version.h
local_sf.o: /usr/openwin/include/xview/icon.h
local_sf.o: /usr/openwin/include/xview/window.h
local_sf.o: /usr/openwin/include/xview/drawable.h
local_sf.o: /usr/openwin/include/xview/attrol.h
local_sf.o: /usr/openwin/include/xview/svrimage.h
local_sf.o: /usr/openwin/include/xview/frame.h
local_sf.o: /usr/openwin/include/xview/openmenu.h
local_sf.o: /usr/openwin/include/xview/panel.h
local_sf.o: /usr/openwin/include/xview/canvas.h
local_sf.o: /usr/openwin/include/xview/openwin.h ../../sgraph/std.h
local_sf.o: /usr/include/assert.h ../../sgraph/sgraph.h modula.h main.h
local_sf.o: local_sf.h
