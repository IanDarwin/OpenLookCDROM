
/*  @(#)extern.h 1.2 88/12/11
 *
 *  Contains the external variable definitions used by faces.
 *
 *  Copyright (c) Rich Burridge.
 *                Sun Microsystems, Australia - All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged. 
 *
 *  No responsibility is taken for any errors or inaccuracies inherent
 *  either to the comments or the code of this program, but if
 *  reported to me then an attempt will be made to fix them.
 */

extern char *get_field(), *getname() ;
extern int do_check() ;
struct recinfo *rec_exists() ;

extern struct comminfo *communities ;  /* Community alias/username chain. */
extern struct comminfo *clast ;        /* End of chain of community records. */
extern struct machinfo *machines ;     /* Known machine/communities. */
extern struct machinfo *mlast ;        /* End of chain of machine records. */
extern struct psinfo *psrecs ;   /* List of news.ps animation files. */
extern struct psinfo *plast ;    /* End of chain of NeWS animation files. */
extern struct recinfo *recs ;    /* Mail messages being monitored. */
extern struct recinfo *last ;    /* End of the chain of mail records. */
extern struct stat buf ;         /* Buffer for stat call for file existence. */
extern long lastsize ;           /* Last known size of the mail folder. */

extern enum gr_type gtype ;  /* Indicates what graphics system is being used. */
extern enum mon_type mtype ; /* What type of monitoring we should do. */

extern char bgicon[] ;     /* Alternate background pattern. */
extern char community[] ;  /* Community name ("real" host name). */
extern char facedir[] ;    /* Directory containing face images. */
extern char iconname[] ;   /* Name of the icon file for this person. */
extern char machfile[] ;   /* Name of the machine/community file. */
extern char nextline[] ;   /* Next line from users mail spool file. */
extern char peopfile[] ;   /* Name of the people/username file. */
extern char printer[] ;    /* Printer name to monitor. */
extern char progname[] ;   /* Name of this program. */
extern char spoolfile[] ;  /* Full pathname of users current mail. */

extern int beeps ;         /* Number of beeps for arrival of new mail. */
extern int column ;        /* Column number for next icon. */
extern int dontshowno ;    /* Set if no. of messages shouldn't  be shown. */
extern int dontshowtime ;  /* Set if timestamp shouldn't be shown. */
extern int dontshowuser ;  /* Set if username shouldn't be shown. */
extern int facetype ;      /* Type of face file found. */
extern int firsttime ;     /* Zeroised after first mail/printer check. */
extern int flashes ;       /* Number of flashes for arrival of new mail. */
extern int height ;        /* Height in pixels of faces display. */
extern int iconic ;        /* Start as an icon if set. */
extern int invert ;        /* Set if to use reverse video. */
extern int ix ;            /* Initial X position of the icon. */
extern int iy ;            /* Initial Y position of the icon. */
extern int newmail ;       /* Set if there is new mail this time around. */
extern int noicons ;       /* Number of faces this time around. */
extern int period ;        /* Period in seconds for checking new mail. */
extern int row ;           /* Row number for next icon. */
extern int width ;         /* Width in pixels of faces display. */
extern int wx ;            /* Initial X position of the window. */
extern int wy ;            /* Initial Y position of the window. */
