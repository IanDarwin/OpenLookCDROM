/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/file_io.h,v 1.1 93/01/06 03:27:36 gounares Exp Locker: gounares $
 */
/*
 * file_io.h
 * 
 * Definitions, etc. describing the format of text data & object links etc.  The
 * module object.[ch] handles interface concerns with linking.
 * 
 * written by Alex Gounares 9/22/92
 * 
 * include after X includes
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __file_io_h
#define __file_io_h

#include <sys/param.h>
#include <dirent.h>

typedef struct _Object_list {
	Textsw_mark     begin;
	Textsw_mark     end;
	int             ibegin;
	int             iend;
	int             id;
	char            szLink_filename[MAXPATHLEN + MAXNAMLEN];
	char           *szDesc;
	int             link_id;
	struct _Object_list *next;
	struct _Object_list *prev;	/* a doubly-linked list is used here
					 * so that at some point, we can pick
					 * an arbitrary link and see the one
					 * before or after it */
}               Object_list;

typedef struct _File {

	/*
	 * it may seem odd to duplicate the name information that is in the
	 * Editor structure, but there may arise occasions where we want to
	 * process a file for objects without displaying it in a textsw, or
	 * vice versa.
	 */
	char            szFilename[MAXNAMLEN];
	char            szDirname[MAXPATHLEN];
	char           *szText;
	int             cObjects;
	Object_list    *pOL;
}               File;


/*
 * function prototypes, etc
 */
File           *load_file( /* char *szFilename */ );
File           *update_File( /* File *pFile, Editor *pEditor */ );
File           *update_textsw( /* File *pFile, Editor *pEditor */ );
int             fFiletoTextsw( /* File *pFile, Editor *pEditor */ );
int             save_file( /* char *szFilename, Editor *pEditor */ );
void		remove_apex( /* char *szFilename */);
void		add_apex( /*char *szFilename */);
void		destroy_file( /* File *pFile */ );

/*
 * special characters
 */

#define FIRST_CHAR 	'\n'
#define SECOND_CHAR	'<'
#define THIRD_CHAR 	'!'
#define FOURTH_CHAR 	'@'
#define OBJ_SZFMT	" %d %s %d"
#define OBJ_START 	"\n<!@"
#define OBJ_END 	"@!>\n"
#define APEX_EXT	".apex"
#define APEX_EXT_LEN	5

#endif
