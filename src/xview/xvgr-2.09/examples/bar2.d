# ACE/gr parameter file
#
@with string
@    string on
@    string loctype view
@    string 0.223750, 0.257143
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 0.790000
@string def "Specified tick marks and tick labels are done with the features provided"
@with string
@    string on
@    string loctype view
@    string 0.223750, 0.234286
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 0.790000
@string def "in 'View/Ticks/Special ticks-tick labels'. There are a couple of variations"
@with string
@    string on
@    string loctype view
@    string 0.223750, 0.211429
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 0.790000
@string def "allowed - tick marks may be specified while the tick labels are drawn"
@with string
@    string on
@    string loctype view
@    string 0.223750, 0.188571
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 0.790000
@string def "using the location of the tick mark or the tick labels may be specified"
@with string
@    string on
@    string loctype view
@    string 0.223750, 0.165714
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 0.790000
@string def "while using the start/step method of tick mark placement."
@with string
@    string on
@    string loctype view
@    string 0.223750, 0.120000
@    string linewidth 1
@    string color 1
@    string rot 0
@    string font 4
@    string just 0
@    string char size 0.790000
@string def "This example uses specified tick marks and specified tick labels."
@with g0
@g0 on
@g0 label off
@g0 hidden false
@g0 type bar
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
@    default font 4
@    default font source 0
@    default symbol size 1.000000
@    world xmin 0
@    world xmax 6
@    world ymin -10
@    world ymax 20
@    view xmin 0.220000
@    view xmax 0.978750
@    view ymin 0.514286
@    view ymax 0.874286
@    title "Test of tick marks and tick labels"
@    title font 3
@    title size 1.500000
@    title color 1
@    title linewidth 1
@    subtitle "Using specified tick marks and tick labels"
@    subtitle font 1
@    subtitle size 1.000000
@    subtitle color 1
@    subtitle linewidth 1
@    s0 type xy
@    s0 symbol 0
@    s0 symbol size 1.000000
@    s0 symbol fill 1
@    s0 symbol center false
@    s0 skip 0
@    s0 linestyle 1
@    s0 linewidth 1
@    s0 color 1
@    s0 fill 1
@    s0 fill with pattern
@    s0 fill color 1
@    s0 fill pattern 2
@    s0 errorbar type BOTH
@    s0 errorbar length 1.000000
@    s0 errorbar linewidth 1
@    s0 errorbar linestyle 1
@    s0 errorbar riser on
@    s0 errorbar riser linewidth 1
@    s0 errorbar riser linestyle 1
@    s0 comment "bar.d"
@    s1 type xy
@    s1 symbol 0
@    s1 symbol size 1.000000
@    s1 symbol fill 0
@    s1 symbol center false
@    s1 skip 0
@    s1 linestyle 1
@    s1 linewidth 1
@    s1 color 1
@    s1 fill 1
@    s1 fill with pattern
@    s1 fill color 2
@    s1 fill pattern 4
@    s1 errorbar type BOTH
@    s1 errorbar length 1.000000
@    s1 errorbar linewidth 1
@    s1 errorbar linestyle 1
@    s1 errorbar riser on
@    s1 errorbar riser linewidth 1
@    s1 errorbar riser linestyle 1
@    s1 comment "bar.d"
@    s2 type xy
@    s2 symbol 0
@    s2 symbol size 1.000000
@    s2 symbol fill 0
@    s2 symbol center false
@    s2 skip 0
@    s2 linestyle 1
@    s2 linewidth 1
@    s2 color 1
@    s2 fill 1
@    s2 fill with pattern
@    s2 fill color 1
@    s2 fill pattern 10
@    s2 errorbar type BOTH
@    s2 errorbar length 1.000000
@    s2 errorbar linewidth 1
@    s2 errorbar linestyle 1
@    s2 errorbar riser on
@    s2 errorbar riser linewidth 1
@    s2 errorbar riser linestyle 1
@    s2 comment "bar.d"
@    s3 type xy
@    s3 symbol 0
@    s3 symbol size 1.000000
@    s3 symbol fill 0
@    s3 symbol center false
@    s3 skip 0
@    s3 linestyle 1
@    s3 linewidth 1
@    s3 color 1
@    s3 fill 1
@    s3 fill with pattern
@    s3 fill color 4
@    s3 fill pattern 11
@    s3 errorbar type BOTH
@    s3 errorbar length 1.000000
@    s3 errorbar linewidth 1
@    s3 errorbar linestyle 1
@    s3 errorbar riser on
@    s3 errorbar riser linewidth 1
@    s3 errorbar riser linestyle 1
@    s3 comment "bar.d"
@    xaxis  tick on
@    xaxis  tick major 1.000000
@    xaxis  tick minor 0.500000
@    xaxis  tick offsetx 0.000000
@    xaxis  tick offsety 0.000000
@    xaxis  tick alt off
@    xaxis  tick min 0.000000
@    xaxis  tick max 1.000000
@    xaxis  label "This is the X-axis label"
@    xaxis  label layout para
@    xaxis  label char size 1.000000
@    xaxis  label font 4
@    xaxis  label color 1
@    xaxis  label linewidth 1
@    xaxis  ticklabel on
@    xaxis  ticklabel type spec
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
@    xaxis  ticklabel char size 0.850000
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
@    xaxis  tick type spec
@    xaxis  tick spec 5
@    xaxis  tick 0, 1.000000
@    xaxis  ticklabel 0, "Bar one"
@    xaxis  tick 1, 2.000000
@    xaxis  ticklabel 1, "Bar two"
@    xaxis  tick 2, 3.000000
@    xaxis  ticklabel 2, "Bar three"
@    xaxis  tick 3, 4.000000
@    xaxis  ticklabel 3, "Bar four"
@    xaxis  tick 4, 5.000000
@    xaxis  ticklabel 4, "Bar none"
@    yaxis  tick on
@    yaxis  tick major 10.000000
@    yaxis  tick minor 5.000000
@    yaxis  tick offsetx 0.000000
@    yaxis  tick offsety 0.000000
@    yaxis  tick alt off
@    yaxis  tick min 0.000000
@    yaxis  tick max 1.000000
@    yaxis  label "This is the Y-axis label"
@    yaxis  label layout para
@    yaxis  label char size 1.000000
@    yaxis  label font 4
@    yaxis  label color 1
@    yaxis  label linewidth 1
@    yaxis  ticklabel on
@    yaxis  ticklabel type spec
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
@    yaxis  ticklabel char size 1.030000
@    yaxis  ticklabel font 1
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
@    yaxis  tick type spec
@    yaxis  tick spec 5
@    yaxis  tick 0, 0.000000
@    yaxis  ticklabel 0, "Bar one"
@    yaxis  tick 1, 5.000000
@    yaxis  ticklabel 1, "Bar two"
@    yaxis  tick 2, 10.000000
@    yaxis  ticklabel 2, "Bar three"
@    yaxis  tick 3, 12.000000
@    yaxis  ticklabel 3, "Bar four"
@    yaxis  tick 4, 20.000000
@    yaxis  ticklabel 4, "Bar none"
@    altxaxis off
@    altyaxis off
@    zeroxaxis  tick on
@    zeroxaxis  tick major 2.000000
@    zeroxaxis  tick minor 1.000000
@    zeroxaxis  tick offsetx 0.000000
@    zeroxaxis  tick offsety 0.000000
@    zeroxaxis  tick alt off
@    zeroxaxis  tick min 0.000000
@    zeroxaxis  tick max 1.000000
@    zeroxaxis  label ""
@    zeroxaxis  label layout para
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
@    zeroxaxis  ticklabel op left
@    zeroxaxis  ticklabel sign normal
@    zeroxaxis  ticklabel start type auto
@    zeroxaxis  ticklabel start 0.000000
@    zeroxaxis  ticklabel stop type auto
@    zeroxaxis  ticklabel stop 0.000000
@    zeroxaxis  ticklabel char size 1.000000
@    zeroxaxis  ticklabel font 1
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
@    zeroyaxis  tick major 10.000000
@    zeroyaxis  tick minor 5.000000
@    zeroyaxis  tick offsetx 0.000000
@    zeroyaxis  tick offsety 0.000000
@    zeroyaxis  tick alt off
@    zeroyaxis  tick min 0.000000
@    zeroyaxis  tick max 1.000000
@    zeroyaxis  label ""
@    zeroyaxis  label layout para
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
@    zeroyaxis  ticklabel font 1
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
@    legend on
@    legend loctype view
@    legend layout 0
@    legend vgap 3
@    legend hgap 1
@    legend length 2
@    legend box off
@    legend x1 0.042500
@    legend y1 0.492857
@    legend font 1
@    legend char size 1.000000
@    legend linestyle 1
@    legend linewidth 1
@    legend color 1
@    legend string 0 "A set legend"
@    legend string 1 "Another set legend"
@    legend string 2 "Yet another"
@    legend string 3 "And another"
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
1.000000 2.000000
2.000000 5.000000
3.000000 9.000000
&
@TYPE xy
1.000000 9.000000
2.000000 5.000000
3.000000 4.000000
&
@TYPE xy
1.000000 7.000000
2.000000 7.000000
3.000000 2.000000
&
@TYPE xy
1.000000 6.000000
2.000000 9.000000
3.000000 20.000000
4.000000 -6.000000
5.000000 5.000000
&
