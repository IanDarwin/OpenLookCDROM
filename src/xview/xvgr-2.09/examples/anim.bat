redraw
title "Cardiod for varying values of \9a"
subtitle "\9a\2 = 9.0"
xaxis label "X = 2 * \9a\2 * cos(t) * (1 + cos(t))"
yaxis label "Y = 2 * \9a\2 * sin(t) * (1 + cos(t))"
doublebuffer true
source pipe
read "tc 9.0 2.0"
autoscale
kill s0
read "tc 8.0 3.0"
subtitle "\9a\2 = 8.0"
redraw
kill s0
read "tc 7.0 4.0"
subtitle "\9a\2 = 7.0"
redraw
kill s0
read "tc 6.0 5.0"
subtitle "\9a\2 = 6.0"
redraw
kill s0
read "tc 5.0 6.0"
subtitle "\9a\2 = 5.0"
redraw
kill s0
read "tc 4.0 7.0"
subtitle "\9a\2 = 4.0"
redraw
kill s0
read "tc 3.0 8.0"
subtitle "\9a\2 = 3.0"
redraw
kill s0
read "tc 2.0 9.0"
subtitle "\9a\2 = 2.0"
redraw
kill s0
read "tc 1.0 10.0"
subtitle "\9a\2 = 1.0"
redraw
