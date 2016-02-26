
/*  @(#)scantool_extern.h 1.4 90/04/04
 *
 *  External variables used by the scantool routines.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged.
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me, then an attempt will be made to fix them.
 */

extern char *br_strs[] ;       /* Brightness menu item strings. */
extern char *con_strs[] ;      /* Contrast menu item strings. */
extern char *gr_strs[] ;       /* Grain menu item strings. */
extern char helpname[] ;       /* Current help file to display. */
extern char *help_strs[] ;     /* Help menu item strings. */
extern char last_message[] ;   /* Last make_displayed message. */
extern char old_picname[] ;    /* Previous picture name. */
extern char output[] ;         /* Panel_set string to be output. */
extern char picname[] ;        /* Name of file for raster image. */
extern char progname[] ;       /* This programs name. */
extern char *res_strs[] ;      /* Resolution menu item strings. */
extern char *set_strs[] ;      /* Set menu item strings. */

extern int brightness ;   /* Brightness value. */
extern int butx ;         /* Top left X position of popup button. */
extern int buty ;         /* Top left Y position of popup button. */
extern int contrast ;     /* Contrast value. */
extern int cur_ch ;       /* Last keyboard character pressed. */
extern int curx ;         /* Current X position of the mouse. */
extern int cury ;         /* Current Y position of the mouse. */
extern int font_width ;   /* Width of a single character. */
extern int framevals[] ;  /* Initial frame in 1/8th inch intervals. */
extern int grain ;        /* Grain value. */
extern int nohelp ;       /* Indicates if a help file was found. */
extern int ops[] ;        /* Pseudo rasterop functions. */
extern int pid ;          /* Process id of the child scan process. */
extern int resolution ;   /* Resolution value. */
extern int scanning ;     /* Set if we are in the middle of a scan. */
extern int showing ;      /* Set if we are in the middle of a show. */
extern int switches[] ;
extern int type ;         /* Current pseudo event type. */
extern int width ;        /* Width in pixels of scantool window. */

extern FILE *hfd ;        /* File descriptor for help file. */

extern enum draw_type drawstate ;  /* Current redraw action. */

