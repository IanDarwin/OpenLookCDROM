/* $XConsortium: fig_enums.h,v 5.1 91/02/16 10:07:33 rws Exp $ */

/*
 */
/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	SimGraphics Engineering Corportation
|
| File          :	fig_enums.h
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Static integer variable initialized to a broad
|			range of values.  Usefule for passing parameters
|			to FORTRAN77 subprograms
| Status        :	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/
/* Phigs f77 constants */
/* INTEGERS */
/* Positive */
static int PHIGS_f77_0=0, PHIGS_f77_1=1, PHIGS_f77_2=2, PHIGS_f77_3=3;
static int PHIGS_f77_4=4, PHIGS_f77_5=5, PHIGS_f77_6=6, PHIGS_f77_7=7;
static int PHIGS_f77_8=8, PHIGS_f77_9=9;
static int PHIGS_f77_10=10, PHIGS_f77_11=11, PHIGS_f77_12=12, PHIGS_f77_13=13;
static int PHIGS_f77_14=14, PHIGS_f77_15=15, PHIGS_f77_16=16, PHIGS_f77_17=17;
static int PHIGS_f77_18=18, PHIGS_f77_19=19;
static int PHIGS_f77_20=20, PHIGS_f77_21=21, PHIGS_f77_22=22, PHIGS_f77_23=23;
static int PHIGS_f77_24=24, PHIGS_f77_25=25, PHIGS_f77_26=26, PHIGS_f77_27=27;
static int PHIGS_f77_28=28, PHIGS_f77_29=29;
static int PHIGS_f77_30=30, PHIGS_f77_31=31, PHIGS_f77_32=32, PHIGS_f77_33=33;
static int PHIGS_f77_34=34, PHIGS_f77_35=35, PHIGS_f77_36=36, PHIGS_f77_37=37;
static int PHIGS_f77_38=38, PHIGS_f77_39=39;
static int PHIGS_f77_40=40, PHIGS_f77_41=41, PHIGS_f77_42=42, PHIGS_f77_43=43;
static int PHIGS_f77_44=44, PHIGS_f77_45=45, PHIGS_f77_46=46, PHIGS_f77_47=47;
static int PHIGS_f77_48=48, PHIGS_f77_49=49;
static int PHIGS_f77_50=50, PHIGS_f77_51=51, PHIGS_f77_52=52, PHIGS_f77_53=53;
static int PHIGS_f77_54=54, PHIGS_f77_55=55, PHIGS_f77_56=56, PHIGS_f77_57=57;
static int PHIGS_f77_58=58, PHIGS_f77_59=59;

/* Negative */
static int PHIGS_f77_m0=  0, PHIGS_f77_m1= -1;
static int PHIGS_f77_m2= -2, PHIGS_f77_m3= -3;
static int PHIGS_f77_m4= -4, PHIGS_f77_m5= -5;
static int PHIGS_f77_m6= -6, PHIGS_f77_m7= -7;
static int PHIGS_f77_m8= -8, PHIGS_f77_m9= -9;
static int PHIGS_f77_m10= -10, PHIGS_f77_m11= -11;
static int PHIGS_f77_m12= -12, PHIGS_f77_m13= -13;
static int PHIGS_f77_m14= -14, PHIGS_f77_m15= -15;
static int PHIGS_f77_m16= -16, PHIGS_f77_m17= -17;
static int PHIGS_f77_m18= -18, PHIGS_f77_m19= -19;
static int PHIGS_f77_m20= -20, PHIGS_f77_m21= -21;
static int PHIGS_f77_m22= -22, PHIGS_f77_m23= -23;
static int PHIGS_f77_m24= -24, PHIGS_f77_m25= -25;
static int PHIGS_f77_m26= -26, PHIGS_f77_m27= -27;
static int PHIGS_f77_m28= -28, PHIGS_f77_m29= -29;

/* Quick Reference defines */
#define Fi0 &PHIGS_f77_0
#define Fi1 &PHIGS_f77_1
#define Fi2 &PHIGS_f77_2
#define Fi3 &PHIGS_f77_3
#define Fi4 &PHIGS_f77_4
#define Fi5 &PHIGS_f77_5
#define Fi6 &PHIGS_f77_6
#define Fi7 &PHIGS_f77_7
#define Fi8 &PHIGS_f77_8
#define Fi9 &PHIGS_f77_9
#define Fi10 &PHIGS_f77_10
#define Fi11 &PHIGS_f77_11
#define Fi12 &PHIGS_f77_12
#define Fi13 &PHIGS_f77_13
#define Fi14 &PHIGS_f77_14
#define Fi15 &PHIGS_f77_15
#define Fi16 &PHIGS_f77_16
#define Fi17 &PHIGS_f77_17
#define Fi18 &PHIGS_f77_18
#define Fi19 &PHIGS_f77_19
#define Fi20 &PHIGS_f77_20
#define Fi21 &PHIGS_f77_21
#define Fi22 &PHIGS_f77_22
#define Fi23 &PHIGS_f77_23
#define Fi24 &PHIGS_f77_24
#define Fi25 &PHIGS_f77_25
#define Fi26 &PHIGS_f77_26
#define Fi27 &PHIGS_f77_27
#define Fi28 &PHIGS_f77_28
#define Fi29 &PHIGS_f77_29
#define Fi30 &PHIGS_f77_30
#define Fi31 &PHIGS_f77_31
#define Fi32 &PHIGS_f77_32
#define Fi33 &PHIGS_f77_33
#define Fi34 &PHIGS_f77_34
#define Fi35 &PHIGS_f77_35
#define Fi36 &PHIGS_f77_36
#define Fi37 &PHIGS_f77_37
#define Fi38 &PHIGS_f77_38
#define Fi39 &PHIGS_f77_39
#define Fi40 &PHIGS_f77_40
#define Fi41 &PHIGS_f77_41
#define Fi42 &PHIGS_f77_42
#define Fi43 &PHIGS_f77_43
#define Fi44 &PHIGS_f77_44
#define Fi45 &PHIGS_f77_45
#define Fi46 &PHIGS_f77_46
#define Fi47 &PHIGS_f77_47
#define Fi48 &PHIGS_f77_48
#define Fi49 &PHIGS_f77_49
#define Fi50 &PHIGS_f77_50
#define Fi51 &PHIGS_f77_51
#define Fi52 &PHIGS_f77_52
#define Fi53 &PHIGS_f77_53
#define Fi54 &PHIGS_f77_54
#define Fi55 &PHIGS_f77_55
#define Fi56 &PHIGS_f77_56
#define Fi57 &PHIGS_f77_57
#define Fi58 &PHIGS_f77_58
#define Fi59 &PHIGS_f77_59

#define Fim0 &PHIGS_f77_m0
#define Fim1 &PHIGS_f77_m1
#define Fim2 &PHIGS_f77_m2
#define Fim3 &PHIGS_f77_m3
#define Fim4 &PHIGS_f77_m4
#define Fim5 &PHIGS_f77_m5
#define Fim6 &PHIGS_f77_m6
#define Fim7 &PHIGS_f77_m7
#define Fim8 &PHIGS_f77_m8
#define Fim9 &PHIGS_f77_m9
#define Fim10 &PHIGS_f77_m10
#define Fim11 &PHIGS_f77_m11
#define Fim12 &PHIGS_f77_m12
#define Fim13 &PHIGS_f77_m13
#define Fim14 &PHIGS_f77_m14
#define Fim15 &PHIGS_f77_m15
#define Fim16 &PHIGS_f77_m16
#define Fim17 &PHIGS_f77_m17
#define Fim18 &PHIGS_f77_m18
#define Fim19 &PHIGS_f77_m19

/* REALS */
/* Some Numbers */
static float PHIGS_f77_f_0=0., PHIGS_f77_f_1=1.;
static float PHIGS_f77_f_2=2., PHIGS_f77_f_3=3.;
static float PHIGS_f77_f_4=4., PHIGS_f77_f_5=5.;
static float PHIGS_f77_f_6=6., PHIGS_f77_f_7=7.;
static float PHIGS_f77_f_8=8., PHIGS_f77_f_9=9.;
static float PHIGS_f77_f_m0=  0., PHIGS_f77_f_m1= -1.;
static float PHIGS_f77_f_m2= -2., PHIGS_f77_f_m3= -3.;
static float PHIGS_f77_f_m4= -4., PHIGS_f77_f_m5= -5.;
static float PHIGS_f77_f_m6= -6., PHIGS_f77_f_m7= -7.;
static float PHIGS_f77_f_m8= -8., PHIGS_f77_f_m9= -9.;
static float PHIGS_f77_f_p0=.0, PHIGS_f77_f_p1=.1;
static float PHIGS_f77_f_p2=.2, PHIGS_f77_f_p3=.3;
static float PHIGS_f77_f_p4=.4, PHIGS_f77_f_p5=.5;
static float PHIGS_f77_f_p6=.6, PHIGS_f77_f_p7=.7;
static float PHIGS_f77_f_p8=.8, PHIGS_f77_f_p9=.9;
#define Fr0 &PHIGS_f77_f_0
#define Fr1 &PHIGS_f77_f_1
#define Fr2 &PHIGS_f77_f_2
#define Fr3 &PHIGS_f77_f_3
#define Fr4 &PHIGS_f77_f_4
#define Fr5 &PHIGS_f77_f_5
#define Fr6 &PHIGS_f77_f_6
#define Fr7 &PHIGS_f77_f_7
#define Fr8 &PHIGS_f77_f_8
#define Fr9 &PHIGS_f77_f_9

#define Frm0 &PHIGS_f77_f_m0
#define Frm1 &PHIGS_f77_f_m1
#define Frm2 &PHIGS_f77_f_m2
#define Frm3 &PHIGS_f77_f_m3
#define Frm4 &PHIGS_f77_f_m4
#define Frm5 &PHIGS_f77_f_m5
#define Frm6 &PHIGS_f77_f_m6
#define Frm7 &PHIGS_f77_f_m7
#define Frm8 &PHIGS_f77_f_m8
#define Frm9 &PHIGS_f77_f_m9

#define Frp0 &PHIGS_f77_f_p0
#define Frp1 &PHIGS_f77_f_p1
#define Frp2 &PHIGS_f77_f_p2
#define Frp3 &PHIGS_f77_f_p3
#define Frp4 &PHIGS_f77_f_p4
#define Frp5 &PHIGS_f77_f_p5
#define Frp6 &PHIGS_f77_f_p6
#define Frp7 &PHIGS_f77_f_p7
#define Frp8 &PHIGS_f77_f_p8
#define Frp9 &PHIGS_f77_f_p9

/* Common multiples of PI */
static float PHIGS_f77_f_pxpi=3.1415927;
static float PHIGS_f77_f_px2pi=6.2831853;
static float PHIGS_f77_f_pxpio2=1.5707963;

/* Convert degrees to radians */
static float PHIGS_f77_f_pxdtr=0.017453292;

