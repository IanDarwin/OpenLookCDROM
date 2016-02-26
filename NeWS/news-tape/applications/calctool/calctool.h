
/*  calctool.h
 *
 *  Contains all the global definitions used by calctool.
 *
 *  Copyright (c) Rich Burridge - May 1988.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Version 2.2.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

#include <stdio.h>
#include <strings.h>
#include <ctype.h>
#include <math.h>

char *sprintf() ;

#define  CLOSE          (void) close      /* To make lint happy. */
#define  FCLOSE         (void) fclose
#define  FFLUSH         (void) fflush
#define  FGETS          (void) fgets
#define  FPRINTF        (void) fprintf
#define  IOCTL          (void) ioctl
#define  READ           (void) read
#define  SPRINTF        (void) sprintf
#define  SSCANF         (void) sscanf
#define  STRCAT         (void) strcat
#define  STRCPY         (void) strcpy
#define  STRNCAT        (void) strncat
#define  STRNCPY        (void) strncpy
#define  WRITE          (void) write

#define  SMALLFONT      "/usr/lib/fonts/fixedwidthfonts/screen.r.7"
#define  NORMALFONT     "/usr/lib/fonts/fixedwidthfonts/screen.b.14"
#define  BIGFONT        "/usr/lib/fonts/fixedwidthfonts/gallant.r.19"

#define  HELPCURSOR     0      /* Cursor types. */
#define  MAINCURSOR     1

enum base_type { BIN, OCT, DEC, HEX } ;   /* Base definitions. */

enum font_type { SFONT, NFONT, BFONT } ;  /* Text font definitions. */

enum can_type { KEYCANVAS, REGCANVAS, PANELCANVAS } ;  /* Canvas types. */

enum but_state { NORMAL, INVERTED } ;     /* Calculator button states. */

enum item_type { BASEITEM, DISPLAYITEM, OPITEM, TTYPEITEM } ;  /* Text items. */

enum trig_type { DEG, GRAD, RAD } ;       /* Trigonometric types. */

enum op_type { OP_SET, OP_CLEAR, OP_NOP } ;  /* Operation item settings. */

#define  BBORDER        10     /* No of pixels in border. */
#define  BCOLS          6      /* No of columns of buttons. */
#define  BGAP           5      /* No of pixels between buttons. */
#define  BHEIGHT        52     /* Number of pixels for height. */
#define  BROWS          6      /* No of rows of buttons. */
#define  BWIDTH         44     /* No of pixels for width. */

#define  CTRL(n)        n - 96           /* Generate control character value. */
#define  DISPLAY        30               /* Calculators numerical display. */

#define  EQUAL          !strcmp          /* For character comparisons. */
#define  EXTRA          4                /* Extra useful character definitions. */

#ifndef HELPGIVEN
#define  HELPNAME       "calctool.help"
#endif

#define  MAX_DIGITS     32               /* Maximum displayable number of digits. */
#define  MAXLINE        80               /* Length of character strings. */
#define  MAXREGS        10               /* Maximum number of memory registers. */
#define  MAXVALID       6                /* Number of valid keys after an error. */
#define  MIN(x,y)       ((x) < (y) ? (x) : (y))

#ifndef  NEWSGIVEN
#define  NEWSNAME       "news.ps"
#endif

#define  NOBUTTONS      BROWS * BCOLS
#define  THEIGHT        (BROWS*BHEIGHT) + ((BROWS-1) * BGAP) + (2*BBORDER)
#define  TITEMS         NOBUTTONS*2 + EXTRA    /* Total definitions. */
#define  TWIDTH         (BCOLS*BWIDTH) + ((BCOLS-1) * BGAP) + (2*BBORDER)

typedef  unsigned long  BOOLEAN ;

struct button {
         char *str ;             /* Button display string. */
         char value ;            /* Unique button keyboard equivalent. */
         enum op_type opdisp ;   /* Display operation code during operation. */
         char color ;            /* Color of button portion. */
         int  (*func)() ;        /* Function to obey on button press. */
} ;
