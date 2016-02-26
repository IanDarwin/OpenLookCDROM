
/*  @(#)scan_extern.h 1.2 90/04/02
 *
 *  External variables used by the scan program.
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

extern char finalimage[] ; /* Name of uncompressed image file. */
extern char line[] ;
extern char picname[] ;    /* Name of file for raster image. */
extern char progname[] ;   /* This programs name. */
extern char temphead[] ;   /* Temporary filename for header file. */
extern char tempimage[] ;  /* Temporary filename for saved scanned data. */

extern int finished ;      /* Indicates if we have finished uncompressing. */
extern int fwidth ;        /* Final width of rasterfile. */
extern int height ;        /* Height in scan lines of raster file image. */
extern int rc ;            /* Current character from scanned file. */
extern int rcount ;        /* Bit position count within read character. */
extern int wc ;            /* Current char to write to final image file. */
extern int wcount ;        /* Bit position count within write character. */
extern int width ;         /* Width in pixels of rasterfile image. */

extern struct code *whites[] ;  /* White initial starting pointers. */
extern struct code *blacks[] ;  /* Black initial starting pointers. */

extern FILE *rd ;          /* File descriptor for input files. */
extern FILE *wd ;          /* File descriptor for final image. */
