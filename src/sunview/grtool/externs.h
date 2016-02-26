/*
        declarations for draw.c

	$Header: externs.h,v 1.1 89/07/19 22:41:35 pturner Locked $
*/
extern int devxticl, devyticl;	/* common length for device */
extern int devcharh, devcharw;	/* typical character height and width */
extern int (*devsetcolor) ();	/* routine to set colors */
extern int (*devconvx) ();	/* map world x to device */
extern int (*devconvy) ();	/* map world y to device y */
extern void (*vector) ();	/* device line routine */
extern void (*devwritestr) ();	/* device text drawing */
extern void (*devdrawtic) ();	/* draw ticks using device draw */
extern void (*devleavegraphics) ();	/* device exit */
extern int (*devsetline) ();	/* device set line style */
extern void (*devsetfont) ();	/* set device font */
