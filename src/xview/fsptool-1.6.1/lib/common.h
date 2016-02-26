/********************************************************************************/
/* lib/common.h --								*/
/********************************************************************************/

#ifndef _FSPtoollib_common_H_
#define _FSPtoollib_common_H_ 1

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include <errno.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/types.h>

#if defined(SVR4) || defined(SYSV)
#include <unistd.h>
#include <stropts.h>
#include <sys/conf.h>
#include <sys/statvfs.h>
#endif

/********************************************************************************/

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024		/* -- reasonable upper limit			*/
#endif

/********************************************************************************/

#define TRUE		1
#define FALSE		0

#define UPLOAD		1
#define DOWNLOAD	2

#define WARNING		0
#define ERROR		1
#define SERIOUS_ERROR	2

/********************************************************************************/

#define FSPTOOL_VERSION	"FSPtool Version 1.6"

/* Changes to values below may necessitate alteration of code in other	*/
/* areas specifically - frame.c/filterframe filter options and it's	*/
/* associated routines - as they work on bit settings.			*/

#define NO_FILETYPES	22		/* -- no of recognised file formats */

#define DIRECTORY	1		/* -- for a directory */
#define FILE_LINK	2		/* -- for a filing sys link */

/* from this point onwards the defined values are used by the */
/* file filter routine with bit mapping for file masking, so  */
/* they should not be changed.				      */

#define TAR_FILE	3		/* -- tar files */
#define Z_FILE		4		/* -- .Z unix compress file */
#define z_FILE		5		/* -- .z gnu zip file */
#define ZIP_FILE	6		/* -- .zip zipped file */

#define TEXT_FILE	7		/* -- text file (default for unknown) */
#define H_FILE		8		/* -- C header file */
#define C_FILE		9		/* -- C source file */
#define GIF_FILE	10		/* -- a GIF file */
#define PBM_FILE	11		/* -- Portable Bitmap file */
#define X11_FILE	12		/* -- X11 Bitmap file */
#define RAS_FILE	13		/* -- Sun Rasterfile */
#define PS_FILE		14		/* -- PostScript file */
#define JPEG_FILE	15		/* -- JPEG format */
#define TIFF_FILE	16		/* -- TIFF format */
#define MPG_FILE	17		/* -- MPG movie format */
#define GL_FILE		18		/* -- GL (grasp) file format */
#define RLE_FILE	19		/* -- RLE (Utah Raster Toolkit) format */
#define AU_FILE		20		/* -- .au file sun-audio */
#define UNKNOWN		21		/* -- defaults for unidentified files */

#endif
/********************************************************************************/
