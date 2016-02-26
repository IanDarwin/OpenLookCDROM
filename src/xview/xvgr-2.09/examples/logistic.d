# ACE/gr parameter file
#
#
@page 95
@page inout 5
@link page off
@with string
@    string on
@    string loctype view
@    string .142857142857, .339256865913
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "Non-linear curve fitting:"
@with string
@    string on
@    string loctype view
@    string .142857142857, .306946688207
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "The function above is a sample from a logistic curve defined as"
@with string
@    string on
@    string loctype view
@    string .142857142857, .273021001616
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "y=1/(1+exp(-(x-a0)/a1))  for x in [-10,20] with a0=5 and a1=3."
@with string
@    string on
@    string loctype view
@    string .142857142857, .24071082391
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "To fit the parameters a0 and a1, open Edit/Transformations/Non-linear"
@with string
@    string on
@    string loctype view
@    string .142857142857, .208400646204
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "curve fitting. Enter items as requested, use initial guesses of a0=1 and"
@with string
@    string on
@    string loctype view
@    string .142857142857, .176090468498
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "a1=1 - make sure the item '# of parameters' is 2. For the function,"
@with string
@    string on
@    string loctype view
@    string .142857142857, .143780290792
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 1.000000
@string def "use the definition above i.e., y=1/(1+exp(-(x-a0)/a1)). Press Apply."
@with g0
@g0 on
@g0 label off
@g0 hidden false
@g0 type xy
@g0 autoscale type AUTO
@g0 fixedpoint off
@g0 fixedpoint type 0
@g0 fixedpoint xy 0.000000, 0.000000
@g0 fixedpoint format general general
@g0 fixedpoint prec 6, 6
@    default linestyle 1
@    default linewidth 1
@    default color 1
@    default char size 1.000000
@    default font 2
@    default font source 0
@    default symbol size 1.000000
@    world xmin -10
@    world xmax 20
@    world ymin 0
@    world ymax 1
@    view xmin 0.147870
@    view xmax 0.852130
@    view ymin 0.468498
@    view ymax 0.848142
@    title ""
@    title font 4
@    title size 1.500000
@    title color 1
@    title linewidth 1
@    subtitle ""
@    subtitle font 4
@    subtitle size 1.000000
@    subtitle color 1
@    subtitle linewidth 1
@    s0 type xy
@    s0 symbol 0
@    s0 symbol size 1.000000
@    s0 symbol fill 0
@    s0 symbol center false
@    s0 symbol char 0
@    s0 skip 0
@    s0 linestyle 1
@    s0 linewidth 1
@    s0 color 1
@    s0 fill 0
@    s0 fill with color
@    s0 fill color 0
@    s0 fill pattern 0
@    s0 errorbar type BOTH
@    s0 errorbar length 1.000000
@    s0 errorbar linewidth 1
@    s0 errorbar linestyle 1
@    s0 errorbar riser on
@    s0 errorbar riser linewidth 1
@    s0 errorbar riser linestyle 1
@    s0 xyz 0.000000, 0.000000
@    s0 comment "logistic.d"
@    xaxis  tick on
@    xaxis  tick major 10
@    xaxis  tick minor 5
@    xaxis  tick offsetx 0.000000
@    xaxis  tick offsety 0.000000
@    xaxis  tick alt off
@    xaxis  tick min 0
@    xaxis  tick max 1
@    xaxis  label ""
@    xaxis  label layout para
@    xaxis  label place auto
@    xaxis  label char size 1.000000
@    xaxis  label font 4
@    xaxis  label color 1
@    xaxis  label linewidth 1
@    xaxis  ticklabel on
@    xaxis  ticklabel type auto
@    xaxis  ticklabel prec 1
@    xaxis  ticklabel format decimal
@    xaxis  ticklabel layout horizontal
@    xaxis  ticklabel skip 0
@    xaxis  ticklabel stagger 0
@    xaxis  ticklabel op bottom
@    xaxis  ticklabel sign normal
@    xaxis  ticklabel start type auto
@    xaxis  ticklabel start 0.000000
@    xaxis  ticklabel stop type auto
@    xaxis  ticklabel stop 0.000000
@    xaxis  ticklabel char size 1.000000
@    xaxis  ticklabel font 4
@    xaxis  ticklabel color 1
@    xaxis  ticklabel linewidth 1
@    xaxis  tick major on
@    xaxis  tick minor on
@    xaxis  tick default 6
@    xaxis  tick in
@    xaxis  tick major color 1
@    xaxis  tick major linewidth 1
@    xaxis  tick major linestyle 1
@    xaxis  tick minor color 1
@    xaxis  tick minor linewidth 1
@    xaxis  tick minor linestyle 1
@    xaxis  tick log off
@    xaxis  tick size 1.000000
@    xaxis  tick minor size 0.500000
@    xaxis  bar off
@    xaxis  bar color 1
@    xaxis  bar linestyle 1
@    xaxis  bar linewidth 1
@    xaxis  tick major grid off
@    xaxis  tick minor grid off
@    xaxis  tick op both
@    xaxis  tick type auto
@    xaxis  tick spec 0
@    yaxis  tick on
@    yaxis  tick major .2
@    yaxis  tick minor .1
@    yaxis  tick offsetx 0.000000
@    yaxis  tick offsety 0.000000
@    yaxis  tick alt off
@    yaxis  tick min 0
@    yaxis  tick max 1
@    yaxis  label ""
@    yaxis  label layout para
@    yaxis  label place auto
@    yaxis  label char size 1.000000
@    yaxis  label font 4
@    yaxis  label color 1
@    yaxis  label linewidth 1
@    yaxis  ticklabel on
@    yaxis  ticklabel type auto
@    yaxis  ticklabel prec 1
@    yaxis  ticklabel format decimal
@    yaxis  ticklabel layout horizontal
@    yaxis  ticklabel skip 0
@    yaxis  ticklabel stagger 0
@    yaxis  ticklabel op left
@    yaxis  ticklabel sign normal
@    yaxis  ticklabel start type auto
@    yaxis  ticklabel start 0.000000
@    yaxis  ticklabel stop type auto
@    yaxis  ticklabel stop 0.000000
@    yaxis  ticklabel char size 1.000000
@    yaxis  ticklabel font 4
@    yaxis  ticklabel color 1
@    yaxis  ticklabel linewidth 1
@    yaxis  tick major on
@    yaxis  tick minor on
@    yaxis  tick default 6
@    yaxis  tick in
@    yaxis  tick major color 1
@    yaxis  tick major linewidth 1
@    yaxis  tick major linestyle 1
@    yaxis  tick minor color 1
@    yaxis  tick minor linewidth 1
@    yaxis  tick minor linestyle 1
@    yaxis  tick log off
@    yaxis  tick size 1.000000
@    yaxis  tick minor size 0.500000
@    yaxis  bar off
@    yaxis  bar color 1
@    yaxis  bar linestyle 1
@    yaxis  bar linewidth 1
@    yaxis  tick major grid off
@    yaxis  tick minor grid off
@    yaxis  tick op both
@    yaxis  tick type auto
@    yaxis  tick spec 0
@    altxaxis  tick on
@    altxaxis  tick major .5
@    altxaxis  tick minor .25
@    altxaxis  tick offsetx 0.000000
@    altxaxis  tick offsety 0.000000
@    altxaxis  tick alt off
@    altxaxis  tick min 0
@    altxaxis  tick max 1
@    altxaxis  label ""
@    altxaxis  label layout para
@    altxaxis  label place auto
@    altxaxis  label char size 1.000000
@    altxaxis  label font 4
@    altxaxis  label color 1
@    altxaxis  label linewidth 1
@    altxaxis  ticklabel off
@    altxaxis  ticklabel type auto
@    altxaxis  ticklabel prec 1
@    altxaxis  ticklabel format decimal
@    altxaxis  ticklabel layout horizontal
@    altxaxis  ticklabel skip 0
@    altxaxis  ticklabel stagger 0
@    altxaxis  ticklabel op bottom
@    altxaxis  ticklabel sign normal
@    altxaxis  ticklabel start type auto
@    altxaxis  ticklabel start 0.000000
@    altxaxis  ticklabel stop type auto
@    altxaxis  ticklabel stop 0.000000
@    altxaxis  ticklabel char size 1.000000
@    altxaxis  ticklabel font 4
@    altxaxis  ticklabel color 1
@    altxaxis  ticklabel linewidth 1
@    altxaxis  tick major off
@    altxaxis  tick minor on
@    altxaxis  tick default 6
@    altxaxis  tick in
@    altxaxis  tick major color 1
@    altxaxis  tick major linewidth 1
@    altxaxis  tick major linestyle 1
@    altxaxis  tick minor color 1
@    altxaxis  tick minor linewidth 1
@    altxaxis  tick minor linestyle 1
@    altxaxis  tick log off
@    altxaxis  tick size 1.000000
@    altxaxis  tick minor size 0.500000
@    altxaxis  bar off
@    altxaxis  bar color 1
@    altxaxis  bar linestyle 1
@    altxaxis  bar linewidth 1
@    altxaxis  tick major grid off
@    altxaxis  tick minor grid off
@    altxaxis  tick op both
@    altxaxis  tick type auto
@    altxaxis  tick spec 0
@    altyaxis  tick on
@    altyaxis  tick major .5
@    altyaxis  tick minor .25
@    altyaxis  tick offsetx 0.000000
@    altyaxis  tick offsety 0.000000
@    altyaxis  tick alt off
@    altyaxis  tick min 0
@    altyaxis  tick max 1
@    altyaxis  label ""
@    altyaxis  label layout para
@    altyaxis  label place auto
@    altyaxis  label char size 1.000000
@    altyaxis  label font 4
@    altyaxis  label color 1
@    altyaxis  label linewidth 1
@    altyaxis  ticklabel off
@    altyaxis  ticklabel type auto
@    altyaxis  ticklabel prec 1
@    altyaxis  ticklabel format decimal
@    altyaxis  ticklabel layout horizontal
@    altyaxis  ticklabel skip 0
@    altyaxis  ticklabel stagger 0
@    altyaxis  ticklabel op left
@    altyaxis  ticklabel sign normal
@    altyaxis  ticklabel start type auto
@    altyaxis  ticklabel start 0.000000
@    altyaxis  ticklabel stop type auto
@    altyaxis  ticklabel stop 0.000000
@    altyaxis  ticklabel char size 1.000000
@    altyaxis  ticklabel font 4
@    altyaxis  ticklabel color 1
@    altyaxis  ticklabel linewidth 1
@    altyaxis  tick major off
@    altyaxis  tick minor on
@    altyaxis  tick default 6
@    altyaxis  tick in
@    altyaxis  tick major color 1
@    altyaxis  tick major linewidth 1
@    altyaxis  tick major linestyle 1
@    altyaxis  tick minor color 1
@    altyaxis  tick minor linewidth 1
@    altyaxis  tick minor linestyle 1
@    altyaxis  tick log off
@    altyaxis  tick size 1.000000
@    altyaxis  tick minor size 0.500000
@    altyaxis  bar off
@    altyaxis  bar color 1
@    altyaxis  bar linestyle 1
@    altyaxis  bar linewidth 1
@    altyaxis  tick major grid off
@    altyaxis  tick minor grid off
@    altyaxis  tick op both
@    altyaxis  tick type auto
@    altyaxis  tick spec 0
@    zeroxaxis  tick on
@    zeroxaxis  tick major 10
@    zeroxaxis  tick minor 5
@    zeroxaxis  tick offsetx 0.000000
@    zeroxaxis  tick offsety 0.000000
@    zeroxaxis  tick alt off
@    zeroxaxis  tick min 0
@    zeroxaxis  tick max 1
@    zeroxaxis  label ""
@    zeroxaxis  label layout para
@    zeroxaxis  label place auto
@    zeroxaxis  label char size 1.000000
@    zeroxaxis  label font 4
@    zeroxaxis  label color 1
@    zeroxaxis  label linewidth 1
@    zeroxaxis  ticklabel off
@    zeroxaxis  ticklabel type auto
@    zeroxaxis  ticklabel prec 1
@    zeroxaxis  ticklabel format decimal
@    zeroxaxis  ticklabel layout horizontal
@    zeroxaxis  ticklabel skip 0
@    zeroxaxis  ticklabel stagger 0
@    zeroxaxis  ticklabel op bottom
@    zeroxaxis  ticklabel sign normal
@    zeroxaxis  ticklabel start type auto
@    zeroxaxis  ticklabel start 0.000000
@    zeroxaxis  ticklabel stop type auto
@    zeroxaxis  ticklabel stop 0.000000
@    zeroxaxis  ticklabel char size 1.000000
@    zeroxaxis  ticklabel font 4
@    zeroxaxis  ticklabel color 1
@    zeroxaxis  ticklabel linewidth 1
@    zeroxaxis  tick major off
@    zeroxaxis  tick minor on
@    zeroxaxis  tick default 6
@    zeroxaxis  tick in
@    zeroxaxis  tick major color 1
@    zeroxaxis  tick major linewidth 1
@    zeroxaxis  tick major linestyle 1
@    zeroxaxis  tick minor color 1
@    zeroxaxis  tick minor linewidth 1
@    zeroxaxis  tick minor linestyle 1
@    zeroxaxis  tick log off
@    zeroxaxis  tick size 1.000000
@    zeroxaxis  tick minor size 0.500000
@    zeroxaxis  bar off
@    zeroxaxis  bar color 1
@    zeroxaxis  bar linestyle 1
@    zeroxaxis  bar linewidth 1
@    zeroxaxis  tick major grid off
@    zeroxaxis  tick minor grid off
@    zeroxaxis  tick op both
@    zeroxaxis  tick type auto
@    zeroxaxis  tick spec 0
@    zeroyaxis  tick on
@    zeroyaxis  tick major .2
@    zeroyaxis  tick minor .1
@    zeroyaxis  tick offsetx 0.000000
@    zeroyaxis  tick offsety 0.000000
@    zeroyaxis  tick alt off
@    zeroyaxis  tick min 0
@    zeroyaxis  tick max 1
@    zeroyaxis  label ""
@    zeroyaxis  label layout para
@    zeroyaxis  label place auto
@    zeroyaxis  label char size 1.000000
@    zeroyaxis  label font 4
@    zeroyaxis  label color 1
@    zeroyaxis  label linewidth 1
@    zeroyaxis  ticklabel off
@    zeroyaxis  ticklabel type auto
@    zeroyaxis  ticklabel prec 1
@    zeroyaxis  ticklabel format decimal
@    zeroyaxis  ticklabel layout horizontal
@    zeroyaxis  ticklabel skip 0
@    zeroyaxis  ticklabel stagger 0
@    zeroyaxis  ticklabel op left
@    zeroyaxis  ticklabel sign normal
@    zeroyaxis  ticklabel start type auto
@    zeroyaxis  ticklabel start 0.000000
@    zeroyaxis  ticklabel stop type auto
@    zeroyaxis  ticklabel stop 0.000000
@    zeroyaxis  ticklabel char size 1.000000
@    zeroyaxis  ticklabel font 4
@    zeroyaxis  ticklabel color 1
@    zeroyaxis  ticklabel linewidth 1
@    zeroyaxis  tick major off
@    zeroyaxis  tick minor on
@    zeroyaxis  tick default 6
@    zeroyaxis  tick in
@    zeroyaxis  tick major color 1
@    zeroyaxis  tick major linewidth 1
@    zeroyaxis  tick major linestyle 1
@    zeroyaxis  tick minor color 1
@    zeroyaxis  tick minor linewidth 1
@    zeroyaxis  tick minor linestyle 1
@    zeroyaxis  tick log off
@    zeroyaxis  tick size 1.000000
@    zeroyaxis  tick minor size 0.500000
@    zeroyaxis  bar off
@    zeroyaxis  bar color 1
@    zeroyaxis  bar linestyle 1
@    zeroyaxis  bar linewidth 1
@    zeroyaxis  tick major grid off
@    zeroyaxis  tick minor grid off
@    zeroyaxis  tick op both
@    zeroyaxis  tick type auto
@    zeroyaxis  tick spec 0
@    legend off
@    legend loctype view
@    legend layout 0
@    legend vgap 2
@    legend hgap 1
@    legend length 4
@    legend box off
@    legend box fill off
@    legend box fill with color
@    legend box fill color 0
@    legend box fill pattern 1
@    legend box color 1
@    legend box linewidth 1
@    legend box linestyle 1
@    legend x1 .8
@    legend y1 .8
@    legend font 4
@    legend char size 1.000000
@    legend linestyle 1
@    legend linewidth 1
@    legend color 1
@    frame on
@    frame type 0
@    frame linestyle 1
@    frame linewidth 1
@    frame color 1
@    frame fill off
@    frame background color 0
@WITH G0
@G0 ON
@TYPE xy
-10.000000 0.006693
-9.696970 0.007399
-9.393939 0.008179
-9.090909 0.009040
-8.787879 0.009992
-8.484848 0.011042
-8.181818 0.012201
-7.878788 0.013481
-7.575758 0.014892
-7.272727 0.016449
-6.969697 0.018165
-6.666667 0.020058
-6.363636 0.022142
-6.060606 0.024438
-5.757576 0.026966
-5.454545 0.029746
-5.151515 0.032804
-4.848485 0.036165
-4.545455 0.039856
-4.242424 0.043906
-3.939394 0.048347
-3.636364 0.053212
-3.333333 0.058537
-3.030303 0.064358
-2.727273 0.070715
-2.424242 0.077648
-2.121212 0.085197
-1.818182 0.093407
-1.515152 0.102319
-1.212121 0.111977
-0.909091 0.122421
-0.606061 0.133694
-0.303030 0.145831
0.000000 0.158869
0.303030 0.172836
0.606061 0.187758
0.909091 0.203650
1.212121 0.220522
1.515152 0.238374
1.818182 0.257194
2.121212 0.276959
2.424242 0.297635
2.727273 0.319173
3.030303 0.341512
3.333333 0.364576
3.636364 0.388281
3.939394 0.412525
4.242424 0.437202
4.545455 0.462194
4.848485 0.487376
5.151515 0.512624
5.454545 0.537806
5.757576 0.562798
6.060606 0.587475
6.363636 0.611719
6.666667 0.635424
6.969697 0.658488
7.272727 0.680827
7.575758 0.702365
7.878788 0.723041
8.181818 0.742806
8.484848 0.761626
8.787879 0.779478
9.090909 0.796350
9.393939 0.812242
9.696970 0.827164
10.000000 0.841131
10.303030 0.854169
10.606061 0.866306
10.909091 0.877579
11.212121 0.888023
11.515152 0.897681
11.818182 0.906593
12.121212 0.914803
12.424242 0.922352
12.727273 0.929285
13.030303 0.935642
13.333333 0.941463
13.636364 0.946788
13.939394 0.951653
14.242424 0.956094
14.545455 0.960144
14.848485 0.963835
15.151515 0.967196
15.454545 0.970254
15.757576 0.973034
16.060606 0.975562
16.363636 0.977858
16.666667 0.979942
16.969697 0.981835
17.272727 0.983551
17.575758 0.985108
17.878788 0.986519
18.181818 0.987799
18.484848 0.988958
18.787879 0.990008
19.090909 0.990960
19.393939 0.991821
19.696970 0.992601
20.000000 0.993307
&
