/*
 * Simple Classing Engine (CE) Interface
 *
 * Author: Jan Andersson - janne@torpa.se
 *
 *	@(#) cei.h 1.1 92/10/15 
 */

#include <desktop/ce.h>
#include <desktop/ce_err.h>

/*
 * Structure used to return CE attributes
 */
typedef struct {
  char           *name;		       /* Name of File Type (TYPE_NAME) */
  char           *open;		       /* Command to open the file (TYPE_OPEN) */
  char           *icon;		       /* Pathname to Icon (TYPE_ICON) */
  char           *icon_mask;	       /* Pathname to Icon mask
				        * (TYPE_ICON_MASK) */
  char           *fgcolor;	       /* Foreground color in RGB values
				        * (TYPE_FGCOLOR) */
  char           *bgcolor;	       /* Background color in RGB values
				        * (TYPE_BGCOLOR) */
}               Cei_Attributes;

/*
 * Functions
 */

/*
 * int cei_open(error_msg)
 *      char *error_msg;
 *
 * Initialize CE, get namespace and attribute id's.
 * Returns 0 on success. On error a message is returned in 'error_msg',
 * which should be at least MAXPATHLEN+64 long.
 */
extern int      cei_open();

/*
 * int cei_get_attr(filename, attr, error_msg)
 *      char *filename;
 *      Cei_Attributes *attr;
 *      char *error_msg;
 *
 * Return CE attributes for file 'filename'.
 * Returns 0 on success. On error a message is returned in 'error_msg',
 * which should be at least MAXPATHLEN+64 long.
 */
extern int      cei_get_attr();

/*
 * cei_close()
 *
 * End CE session.
 */
extern void     cei_close();
