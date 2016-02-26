
/*  color.h
 *
 *  Colormap definitions used by calctool.
 *
 *  Copyright (c) Rich Burridge - May 1988.
 *               Sun Microsystems, Australia - All rights reserved.
 *
 *  Version 2.2.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#define  CALC_COLOR      "calcolor"
#define  CALC_COLORSIZE  16

#define  WHITE    0
#define  RED      1
#define  ORANGE   2
#define  YELLOW   3
#define  LGREEN   4
#define  BLUE     5
#define  MAUVE    6
#define  GREEN    7
#define  PINK     8
#define  LBLUE    9
#define  GREY     10
#define  LGREY    11
#define  LPURPLE  12
#define  BLACK    13
#define  SPARE14  14
#define  SPARE15  15

#define calc_colorsetup(r,g,b) \
        (r)[WHITE] = 255 ;       (g)[WHITE] = 255 ;       (b)[WHITE] = 255 ; \
        (r)[RED] = 255 ;         (g)[RED] = 50 ;          (b)[RED] = 0 ; \
        (r)[ORANGE] = 255 ;      (g)[ORANGE] = 128 ;      (b)[ORANGE] = 0 ; \
        (r)[YELLOW] = 255 ;      (g)[YELLOW] = 255 ;      (b)[YELLOW] = 0 ; \
        (r)[LGREEN] = 128 ;      (g)[LGREEN] = 255 ;      (b)[LGREEN] = 128 ; \
        (r)[BLUE] = 50 ;         (g)[BLUE] = 128 ;        (b)[BLUE] = 255 ; \
        (r)[MAUVE] = 128 ;       (g)[MAUVE] = 128 ;       (b)[MAUVE] = 255 ; \
        (r)[GREEN] = 0 ;         (g)[GREEN] = 158 ;       (b)[GREEN] = 158 ; \
        (r)[PINK] = 255 ;        (g)[PINK] = 192 ;        (b)[PINK] = 192 ; \
        (r)[LBLUE] = 50 ;        (g)[LBLUE] = 178 ;       (b)[LBLUE] = 255 ; \
        (r)[GREY] = 128 ;        (g)[GREY] = 128 ;        (b)[GREY] = 128 ; \
        (r)[LGREY] = 200 ;       (g)[LGREY] = 200 ;       (b)[LGREY] = 200 ; \
        (r)[LPURPLE] = 225 ;     (g)[LPURPLE] = 225 ;     (b)[LPURPLE] = 255 ; \
        (r)[BLACK] = 0 ;         (g)[BLACK] = 0 ;         (b)[BLACK] = 0 ; \
        (r)[SPARE14] = 0 ;       (g)[SPARE14] = 0 ;       (b)[SPARE14] = 0 ; \
        (r)[SPARE15] = 0 ;       (g)[SPARE15] = 0 ;       (b)[SPARE15] = 0 ;
