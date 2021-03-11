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


/*
 *      Common declarations, typedefs, and constants for panel handling
 */

typedef struct xtent {
    unsigned char xrow, xcol, xlen, xprt;
    } XTENT;

typedef struct prmpt {
    char plen;
    char *pdata;
    } PRMPT;

struct field {
    struct field *freturn;
    struct field *fup;
    struct field *fdown;
    struct field *fleft;
    struct field *fright;
    XTENT *flocation;
    unsigned char fattr;
    char   ftype;
    char  *fdata;
    PRMPT *fprompt;
    int (*fmodifyhandler)();
    int (*fentryhandler)();
    int (*fexithandler)();
    int (*fexpandhandler)();
    int (*fshrinkhandler)();
    int (*fprotecthandler)();
    };

typedef struct field FIELD;

/*  fattr values  */

#define NORMAL    0                 /* Normal on all displays           */
#define HILITE    1                 /* Hilite on all displays           */
#define RVIDEO    2                 /* Reverse Video                    */
#define BLINKH    3                 /* Blink + HILITE                   */
#define REGDIF    4                 /* RVIDEO unless B/W adapter        */
#define NORMCE    5                 /* Same as NORMAL, unless color     */
#define NORMCI    6                 /* Same as NORMAL, unless color     */
#define HILITC    7                 /* Same as HILITE, unless color     */
#define RVIDHI    8                 /* Reverse Video + HILITE           */
#define WHTBLU    9                 /* While on blue, or normal         */
#define INVISI    10                /* Invisible                        */
#define RINVIS    11                /* Reverse Video & invisible data   */
#define UNDERN    12                /* Normal underline                 */
#define UNDERH    13                /* Hilited underline                */

/*  ftype values  */

#define FTYPE_ASC 1                 /* character string          */
#define FTYPE_INT 2                 /* integer                   */
#define FTYPE_YN  3                 /* yes or no                 */
#define FTYPE_SEL 4                 /* cursor selection field    */
#define FTYPE_WRD 5                 /* word (no blanks */
#define FTYPE_SCR 6                 /* automatic scroll     */

typedef struct panel {
    char prow, pcol, pattr, plen;
    char *pdata;
    } PANEL;

#define PANEL_PROMPT  1
#define PANEL_NOPROMPT 0
#define PANEL_CLEAR   1
#define PANEL_NOCLEAR 0

typedef struct keytab {
    int scan_code;
    FIELD *(*keyhandler)();
    } KEYTAB;

#define MATCHANY 0xFFFF

typedef struct videoparms {
    unsigned char palette;
    unsigned char page;
    unsigned char attr;
    unsigned char mode;
    unsigned char columns;
    } VIDEOPARMS;

FIELD *GetCurrentField ();

#ifndef NULL
#define NULL    0
#endif /* NULL */
#ifndef NIL
#define NIL     (char *)0
#endif /* NIL */

#define NULL_FIELD NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,NULL,NULL,
#define NULL_PANEL 0,0,0,0, NULL

#ifndef TRUE
#define TRUE 1
#endif /* TRUE */
#ifndef FALSE
#define FALSE 0
#endif /* FALSE */

#define PRIVATE static

#define REPLACE 0
#define INSERT  0xFF

#define	DIR_UP_ARROW	    0
#define	DIR_DOWN_ARROW	    1
#define	DIR_HOME_KEY	    2
#define	DIR_END_KEY	    3

#define	MSG_UP_ARROW	    0
#define	MSG_DOWN_ARROW	    1
#define	MSG_HOME_KEY	    2
#define	MSG_END_KEY	    3

/* data structure for handling raw files and message bodies */

typedef struct msgorfile {
    PANEL   *BodyData;
    int	    MaxRow,MaxVisible,BodyLines;
    Boolean ThisIsAMessage;
    long    HeadOffset;
    char    *PageBuf,FileName[MAXPATHLEN+1];
    struct  msg_page_list *FirstPage, *CurrentPage;
} MSG_OR_FILE;

/* structure for keeping marked messages for gloabl operation */

struct marked_msgs {
    long msgno;
    struct marked_msgs *prev;
    struct marked_msgs *next;
};

typedef struct marked_msgs MARKED_MSGS;

