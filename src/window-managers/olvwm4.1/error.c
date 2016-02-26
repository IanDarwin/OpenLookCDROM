#ifdef IDENT
#ident	"@(#)error.c	26.22	93/06/28"
#endif

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *	Sun design patents pending in the U.S. and foreign countries. See
 *	LEGAL_NOTICE file for terms of the license.
 */

#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xproto.h>

#include "i18n.h"
#include "ollocale.h"
#include "error.h"
#include "olwm.h"
#include "globals.h"


/* some syntactic sugar to shut up lint */

#define FPRINTF		(void) fprintf


/* The following defines create bitmasks from the X Error Codes */

#define BReq	(1<<BadRequest)
#define BVal	(1<<BadValue)
#define BWin	(1<<BadWindow)
#define BPix	(1<<BadPixmap)
#define BAtm	(1<<BadAtom)
#define BCur	(1<<BadCursor)
#define BFnt	(1<<BadFont)
#define BMch	(1<<BadMatch)
#define BDrw	(1<<BadDrawable)
#define BAcc	(1<<BadAccess)
#define BAlc	(1<<BadAlloc)
#define BCol	(1<<BadColor)
#define BGC	(1<<BadGC)
#define BIDC	(1<<BadIDChoice)
#define BNam	(1<<BadName)
#define BLen	(1<<BadLength)
#define BImp	(1<<BadImplementation)

#define BAll	(~0)	/* matches all error bitmasks */


/* locals */

static char	*hyperSensitive = NULL;


/*
 * This error table encodes the severity of an error on the basis of the
 * protocol request that generated the error.  For instance if BadWindow is
 * fatal for a certain request, BadColor was a warning, and other errors are
 * to be ignored, the following entry would be used in table slot 
 * corresponding to the request:
 * 
 *	{BWin,	BCol,	~(BWin|BCol)}
 */

typedef struct _ErrorEntry {
	unsigned int	fatal,
			warning,
			ignore;
} ErrorEntry;

static ErrorEntry errorTable[] = {
      /* fatal  warning ignore */
	{BAll,	0,	0},	/* 0,  Not Used */
	{BAll,	0,	0},	/* 1,  X_CreateWindow */
	{0,    ~BWin,	BWin},	/* 2,  X_ChangeWindowAttributes */
	{0,    ~BWin,	BWin},	/* 3,  X_GetWindowAttributes */
	{0,	BAll,	0},	/* 4,  X_DestroyWindow */
	{0,	BAll,	0},	/* 5,  X_DestroySubwindows */
	{0,    ~BWin,	BWin},	/* 6,  X_ChangeSaveSet */
	{0,    ~BWin,	BWin},	/* 7,  X_ReparentWindow */
	{0,    ~BWin,	BWin},	/* 8,  X_MapWindow */
	{0,	BAll,	0},	/* 9,  X_MapSubwindows */
	{0,    ~BWin,	BWin},	/* 10, X_UnmapWindow */
	{0,	BAll,	0},	/* 11, X_UnmapSubwindows */
	{0,    ~BWin,	BWin},	/* 12, X_ConfigureWindow */
	{0,	BAll,	0},	/* 13, X_CirculateWindow */
	{0,    ~BDrw,	BDrw},	/* 14, X_GetGeometry */
	{0,    ~BWin,	BWin},	/* 15, X_QueryTree */
	{0,	BAll,	0},	/* 16, X_InternAtom */
	{0,	BAll,	0},	/* 17, X_GetAtomName */
	{0,    ~BWin,	BWin},	/* 18, X_ChangeProperty */
	{0,	BAll,	0},	/* 19, X_DeleteProperty */
	{0,    ~BWin,	BWin},	/* 20, X_GetProperty */
	{0,    ~BWin,	BWin},	/* 21, X_ListProperties */
	{0,	BAll,	0},	/* 22, X_SetSelectionOwner */
	{0,	BAll,	0},	/* 23, X_GetSelectionOwner */
	{0,	BAll,	0},	/* 24, X_ConvertSelection */
	{0,    ~BWin,	BWin},	/* 25, X_SendEvent */
	{0,	0,	BAll},	/* 26, X_GrabPointer */
	{0,	0,	BAll},	/* 27, X_UngrabPointer */
	{0,	0,	BAll},	/* 28, X_GrabButton */
	{0,	0,	BAll},	/* 29, X_UngrabButton */
	{0,	0,	BAll},	/* 30, X_ChangeActivePointerGrab */
	{0,	BAll,	0},	/* 31, X_GrabKeyboard */
	{0,	BAll,	0},	/* 32, X_UngrabKeyboard */
	{0,	BAll,	0},	/* 33, X_GrabKey */
	{0,	BAll,	0},	/* 34, X_UngrabKey */
	{0,	BAll,	0},	/* 35, X_AllowEvents */
	{BAll,	0,	0},	/* 36, X_GrabServer */
	{0,	BAll,	0},	/* 37, X_UngrabServer */
	{0,	BAll,	0},	/* 38, X_QueryPointer */
	{0,	BAll,	0},	/* 39, X_GetMotionEvents */
	{0,	BAll,	0},	/* 40, X_TranslateCoords */
	{0,	BAll,	0},	/* 41, X_WarpPointer */
	{0,   ~(BMch|BWin),	BMch|BWin},	/* 42, X_SetInputFocus */
	{0,	BAll,	0},	/* 43, X_GetInputFocus */
	{0,	BAll,	0},	/* 44, X_QueryKeymap */
	{0,	BAll,	0},	/* 45, X_OpenFont */
	{0,	BAll,	0},	/* 46, X_CloseFont */
	{0,	BAll,	0},	/* 47, X_QueryFont */
	{0,	BAll,	0},	/* 48, X_QueryTextExtents */
	{0,	BAll,	0},	/* 49, X_ListFonts */
	{0,	BAll,	0},	/* 50, X_ListFontsWithInfo */
	{0,	BAll,	0},	/* 51, X_SetFontPath */
	{0,	BAll,	0},	/* 52, X_GetFontPath */
	{0,	BAll,	0},	/* 53, X_CreatePixmap */
	{0,	BAll,	0},	/* 54, X_FreePixmap */
	{0,	BAll,	0},	/* 55, X_CreateGC */
	{0,	BAll,	0},	/* 56, X_ChangeGC */
	{0,	BAll,	0},	/* 57, X_CopyGC */
	{0,	BAll,	0},	/* 58, X_SetDashes */
	{0,	BAll,	0},	/* 59, X_SetClipRectangles */
	{0,	BAll,	0},	/* 60, X_FreeGC */
	{0,    ~BMch,	BMch},	/* 61, X_ClearArea */
	{0,	BAll,	0},	/* 62, X_CopyArea */
	{0,	BAll,	0},	/* 63, X_CopyPlane */
	{0,	BAll,	0},	/* 64, X_PolyPoint */
	{0,	BAll,	0},	/* 65, X_PolyLine */
	{0,	BAll,	0},	/* 66, X_PolySegment */
	{0,	BAll,	0},	/* 67, X_PolyRectangle */
	{0,	BAll,	0},	/* 68, X_PolyArc */
	{0,	BAll,	0},	/* 69, X_FillPoly */
	{0,	BAll,	0},	/* 70, X_PolyFillRectangle */
	{0,	BAll,	0},	/* 71, X_PolyFillArc */
	{0,	BAll,	0},	/* 72, X_PutImage */
	{0,	BAll,	0},	/* 73, X_GetImage */
	{0,	BAll,	0},	/* 74, X_PolyText8 */
	{0,	BAll,	0},	/* 75, X_PolyText16 */
	{0,	BAll,	0},	/* 76, X_ImageText8 */
	{0,	BAll,	0},	/* 77, X_ImageText16 */
	{0,	BAll,	0},	/* 78, X_CreateColormap */
	{0,	BAll,	0},	/* 79, X_FreeColormap */
	{0,	BAll,	0},	/* 80, X_CopyColormapAndFree */
	{0,    ~BCol,	BCol},	/* 81, X_InstallColormap */
	{0,    ~BCol,	BCol},	/* 82, X_UninstallColormap */
	{0,	BAll,	0},	/* 83, X_ListInstalledColormaps */
	{0,	BAll,	0},	/* 84, X_AllocColor */
	{0,	BAll,	0},	/* 85, X_AllocNamedColor */
	{0,	BAll,	0},	/* 86, X_AllocColorCells */
	{0,	BAll,	0},	/* 87, X_AllocColorPlanes */
	{0,	BAll,	0},	/* 88, X_FreeColors */
	{0,	BAll,	0},	/* 89, X_StoreColors */
	{0,	BAll,	0},	/* 90, X_StoreNamedColor */
	{0,	BAll,	0},	/* 91, X_QueryColors */
	{0,	BAll,	0},	/* 92, X_LookupColor */
	{0,	BAll,	0},	/* 93, X_CreateCursor */
	{0,	BAll,	0},	/* 94, X_CreateGlyphCursor */
	{0,	BAll,	0},	/* 95, X_FreeCursor */
	{0,	BAll,	0},	/* 96, X_RecolorCursor */
	{0,	BAll,	0},	/* 97, X_QueryBestSize */
	{0,	BAll,	0},	/* 98, X_QueryExtension */
	{0,	BAll,	0},	/* 99, X_ListExtensions */
	{0,	BAll,	0},	/* 100, X_ChangeKeyboardMapping */
	{0,	BAll,	0},	/* 101, X_GetKeyboardMapping */
	{0,	BAll,	0},	/* 102, X_ChangeKeyboardControl */
	{0,	BAll,	0},	/* 103, X_GetKeyboardControl */
	{0,	BAll,	0},	/* 104, X_Bell */
	{0,	BAll,	0},	/* 105, X_ChangePointerControl */
	{0,	BAll,	0},	/* 106, X_GetPointerControl */
	{0,	BAll,	0},	/* 107, X_SetScreenSaver */
	{0,	BAll,	0},	/* 108, X_GetScreenSaver */
	{0,	BAll,	0},	/* 109, X_ChangeHosts */
	{0,	BAll,	0},	/* 110, X_ListHosts */
	{0,	BAll,	0},	/* 111, X_SetAccessControl */
	{0,	BAll,	0},	/* 112, X_SetCloseDownMode */
	{0,    ~BVal,	BVal},	/* 113, X_KillClient */
	{0,	BAll,	0},	/* 114, X_RotateProperties */
	{0,	BAll,	0},	/* 115, X_ForceScreenSaver */
	{0,	BAll,	0},	/* 116, X_SetPointerMapping */
	{0,	BAll,	0},	/* 117, X_GetPointerMapping */
	{0,	BAll,	0},	/* 118, X_SetModifierMapping */
	{0,	BAll,	0},	/* 119, X_GetModifierMapping */
	{BAll,	0,	0},	/* 120, Not Used */
	{BAll,	0,	0},	/* 121, Not Used */
	{BAll,	0,	0},	/* 122, Not Used */
	{BAll,	0,	0},	/* 123, Not Used */
	{BAll,	0,	0},	/* 124, Not Used */
	{BAll,	0,	0},	/* 125, Not Used */
	{BAll,	0,	0},	/* 126, Not Used */
	{0,	BAll,	0}	/* 127, X_NoOperation */
}; /* errorTable */


/*
 * printError
 *
 * Print out a nicely formatted error message, prefixed by a tag string.
 */
static void
printError(dpy, err, tag)
    Display *dpy;
    XErrorEvent *err;
    char *tag;
{
    char buf[BUFSIZ];
    char number[32];

    XGetErrorText(dpy, err->error_code, buf, BUFSIZ);
    FPRINTF(stderr, GetString("%s%s\n"), tag, buf);
    sprintf(number, "%d", err->request_code);
    XGetErrorDatabaseText(dpy, "XRequest", number, "", buf, BUFSIZ);

    FPRINTF(stderr, GetString("  request major code:  %d (%s)\n"),
	err->request_code, buf);
    FPRINTF(stderr, GetString("  request minor code:  %d\n"),
	err->minor_code);
    FPRINTF(stderr, GetString("  resource ID in failed request:  0x%x\n"),
	err->resourceid);
    FPRINTF(stderr, GetString("  serial number of failed request:  %d\n"),
	err->serial);
    FPRINTF(stderr, GetString("  current request serial number:  %d\n"),
	dpy->request);
}


/*
 * handleExtensionError -- handle an error that came from an extension 
 * request.  Policy is determined in code instead of being table-driven.  
 * BadWindow errors for SHAPE extension requests are ignored.  All other 
 * errors are treated as warnings.
 */
static void
handleExtensionError(dpy, event)
    Display *dpy;
    XErrorEvent *event;
{
#ifdef SHAPE
    extern int ShapeRequestBase;

    if (event->request_code == ShapeRequestBase
    && event->error_code == BadWindow)
	return;
#endif /* SHAPE */

    if (GRV.PrintWarnings) {
	printError(dpy, event,
	    GetString("olvwm: warning, X extension error -- "));
    }
}



/*
 * ErrorSensitive
 *
 * Cause an exit on all X errors.  If an error occurs, the string is printed 
 * along with the error message.
 */
void
ErrorSensitive(s)
	char *s;
{
	hyperSensitive = s;
}


/*
 * ErrorInsensitive -- perform normal X error processing
 */
/*ARGSUSED*/	/* dpy arg will be used when multiple Displays supported */
void ErrorInsensitive(dpy)
	Display *dpy;
{
	hyperSensitive = NULL;
}


/*
 * ErrorHandler
 *
 * Called whenever an X protocol error is detected.  Prints the contents of
 * the X error event.  If hypersensitive, always exits.  Calls
 * handleExtensionError() and returns if the error was generated by a
 * extension request.  For errors generated by core requests, may exit if
 * errorTable entry dictates that we do so.
 */
int
ErrorHandler(dpy, event)
	Display *dpy;
	XErrorEvent *event;
{
	int		errBitmask;
	ErrorEntry	*entry;

	if (hyperSensitive != NULL) {
		printError(dpy, event,
		    GetString("olvwm: fatal X protocol error -- "));
		FPRINTF(stderr, "%s\n", hyperSensitive);
		exit(1);
		/*NOTREACHED*/
	}

	if (event->request_code > X_NoOperation) {
	    handleExtensionError(dpy, event);
	    return 0;
	}

	errBitmask = 1 << event->error_code;
	entry = &errorTable[event->request_code];

	if (errBitmask & entry->fatal) {
		printError(dpy, event,
			GetString("olvwm: fatal X protocol error -- "));
		exit(1);
		/*NOTREACHED*/
	} else if (errBitmask & entry->warning) {
		if (GRV.PrintWarnings) {
		    printError(dpy, event,
			GetString("olvwm: warning, X protocol error -- "));
		}
	}

	return 0;
}


/*
 * ErrorGeneral
 *
 * Called whenever a fatal error occurs that is not caused by an X protocol 
 * error.  Always exits immediately.
 */
void
ErrorGeneral(txt)
	char *txt;
{
        FPRINTF(stderr, GetString("olvwm: fatal error -- %s\n"), txt);
#ifdef DEBUG
	abort();
	/*NOTREACHED*/
#else
        exit(1);
	/*NOTREACHED*/
#endif
}


/*
 * ErrorWarning
 *
 * Called whenever OLWM needs to issue a warning message.
 */
void
ErrorWarning(txt)
	char *txt;
{
	if (GRV.PrintWarnings)
		FPRINTF(stderr, GetString("olvwm: warning -- %s\n"), txt);
}
