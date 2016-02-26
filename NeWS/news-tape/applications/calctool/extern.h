
/*  extern.h
 *
 *  Contains the external variable definitions used by calctool.
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

extern char *make_number() ;
extern char base_str[4][4] ;
extern char cur_base_str[] ;       /* Current base value. */
extern char cur_op ;               /* Current arithmetic operation. */
extern char cur_op_str[] ;         /* Current op value. */
extern char cur_ttype_str[] ;      /* Current ttype value. */
extern char current ;              /* Current button or character pressed. */
extern char old_cal_value ;        /* Previous calculation operator. */
extern char display[MAXLINE] ;     /* Current calculator display. */
extern char pstr[] ;               /* Current button text string. */
extern char *selection ;           /* Current [Get] selection. */
extern char ttype_str[3][5] ;

extern double convert_display() ;
extern double disp_val ;           /* Value of the current display. */
extern double last_input ;         /* Previous number input by user. */
extern double mem_vals[] ;         /* Memory register values. */
extern double result ;             /* Current calculator total value. */
extern double tresults[] ;         /* Current trigonometric results. */

extern enum base_type base ;       /* Current base: BIN, OCT, DEC or HEX. */
extern enum trig_type ttype ;      /* Trigonometric type (deg, grad or rad). */

extern int accuracy ;       /* Number of digits precision (Max 9). */
extern int chxoff[] ;       /* X offset for various length button strings. */
extern int color ;          /* Color used for current raster operation. */
extern int column ;         /* Column number of current key/mouse press. */
extern int disp_length[] ;  /* Display length in characters for each base. */
extern int down ;           /* Indicates if mouse button is down. */
extern int error ;          /* Indicates some kind of display error. */
extern int iscolor ;        /* Set if this is a color screen. */
extern int ishelp ;         /* Set if there is a help file. */
extern int issel ;          /* Set if valid [Get] selection. */
extern int new_input ;      /* New number input since last op. */
extern int pending ;        /* Set for command with on multiple presses. */
extern int pending_op ;     /* Arithmetic operation for pending command. */
extern int pointed ;        /* Whether a decimal point has been given. */
extern int portion ;        /* Button portion on current key/mouse press. */
extern int row ;            /* Row number of current key/mouse press. */
extern int rstate ;         /* Is memory register frame displayed? */
extern int signgam ;
extern int spaces ;         /* Number of spaces in current button string. */
extern int toclear ;        /* Indicates if display should be cleared. */
extern int tstate ;         /* Current button set being displayed. */
extern int x ;              /* X offset for text for button. */
extern int y ;              /* Y offset for text for button. */

extern FILE *hfd ;          /* File descriptor for help information. */

extern struct button buttons[] ;  /* Calculator button values. */
