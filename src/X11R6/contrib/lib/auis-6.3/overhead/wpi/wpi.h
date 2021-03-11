/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


/*========================================================================*\
 *                                                                        *
 * header for wpilib, routines for grabbing a WP entry, and validating it *
 *                                                                        *
\*========================================================================*/

/* Data Stream format:
   a mail message will be sent from WPI, the body of which will be
   filtered through wpiupdat's stdin.  This is the format:

   DS            :: VERSION CELL [REQUEST]* [COMMENT]*
   VERSION       :: [COMMENT]* "version:" VERSIONNUMBER "\n"
   CELL          :: [COMMENT]* "cell:" CELLNAME "\n"
   REQUEST       :: [COMMENT]* (ADD | REMOVE | CHANGE)
   ADD           :: "adduser:" LOGINNAME "\n"
   REMOVE        :: "rmuser:" LOGINNAME "\n"
   CHANGE        :: "change:" LOGINNAME ":" WPFIELD ":*:" 
                    QUOTEDVALUE ":" TIMESTAMP "\n"
   COMMENT       :: ("\n" | "> " ANYTEXT " \n")
   VERSIONNUMBER :: <an integer between 1 and 1>
   CELLNAME      :: <any valid NAFS cell name, must match current cell at update>
   LOGINNAME     :: <any valid WP N field>
   WPFIELD       :: <any valid WP field name (e.g. Fwd, Sh, etc.)>
   QUOTEDVALUE   :: <any string, WP.chg quoted (":" --> "+=", '+' --> "++")>
   TIMESTAME     :: <any integer, usually time(0)>
   ANYTEXT       :: <any text string, no newlines>
*/

#define WPI_DS_VERSION 1	/* Data Stream version number */

typedef enum boolean_t {false = 0, true} boolx_t; /* the boolean type */

typedef struct {		/* the entry type */
  int fieldnum;
  boolx_t changed;
  char *fieldname, *value;
} *WPI_entry_t;

typedef enum WPI_can_i_change {	/* the modifiable type */
  ALLOW_MODIFY, 
  PRIVILEDGED_MODIFY,
  GENERATED_FIELD,
  UNKNOWN_FIELD
} change_t;

typedef enum WP_validate_p {	/* result of validation */
  cool, 
  drag, 
  uncool
} validate_t;

extern int errno;		/* gak!  why isn't there a header for this? */

extern int WPI_error_code;	/* error code (if any) from last WPI op */
extern char WPI_error_msg[];	/* error msg (if any) from last WPI op */

#define WPI_OK 0		/* no error */
#define WPI_TEMP_UNCERTAINVALID 2	/* temp. fail during validation */
#define WPI_WARN 1		/* just a warning */
#define WPI_ERR_NOMEM -1	/* out of memory */
#define WPI_ERR_NOTAUTH -2	/* not authenticated in cell */
#define WPI_ERR_NOCELL -3	/* couldn't find a cell */
#define WPI_ERR_BADVALID -4	/* bad validation */
#define WPI_ERR_NOKEY -5	/* no key was found (bad username) */
#define WPI_ERR_MAXERR -10	/* used for scaling WP errs */
				/* get the WP err out of WPI error */
#define WPERRPART(x) (-((x)-(WPI_ERR_MAXERR)))

/*      WPI_SetWorkingDomain
	        Sets the domain in which WP lookups and WPI_Self will work
*/
extern void WPI_SetWorkingDomain(/* char *domain */);

/*      WPI_GetWorkingDomain
	        Returns the domain in which WP lookups and WPI_Self will work
*/
extern char *WPI_GetWorkingDomain(/* void */);


/*	WPI_CanIChange(field) ==> [policy]
		1. ALLOW_MODIFY, go ahead (but still validate)
		2. PRIVILIDGED_MODIFY, for WPAdmins only
		3. GENERATED_FIELD, why would you want to? */
extern change_t WPI_CanIChange(/* char *field */);

/*	WPI_Validate(field, value, entry) ==> [policy]
		1. cool, change is ok (and made)
		2. drag, need WPAdmin assistance (and made)
		3. uncool, change is bad (and not made) */
extern validate_t WPI_Validate(/* char *field, char *value, entry_t entry */);

/*	WPI_Update(field, value, entry) ==>
		sets the value of field in entry */
extern void WPI_Update(/* char *field, char *value, entry_t entry */ );

/*	WPI_Value(fieldname, entry) ==>
		returns the value from the entry corresponding to the fieldname */
extern char *WPI_Value(/* char *field, entry_t entry */ );

/*	WPI_Nice(fieldname)
		returns a human readable string for the 
		abbreviated WP fieldname. */
extern char *WPI_Nice(/* char *fieldname */);

/*      WPI_Example(fieldname)
	        returns an example value for this field. */
extern char *WPI_Example(/* char *fieldname */);

/*      WPI_Description(fieldname)
	        returns the English description of this field. */
extern char *WPI_Description(/* char *fieldname */);

/*	WPI_Self()
		returns the username of the current user. */
extern char *WPI_Self(/* void */);

/*	WPI_Lookup(user, admin_flag, entry) ==>
		fill the array "entry" with the values of the modifiable 
		fields (including admin fields if admin_flag is true) 
		from the WP. */
extern WPI_entry_t WPI_Lookup(/* char *user, boolx_t admin_flag */);
