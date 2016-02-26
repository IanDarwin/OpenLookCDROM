/* 
 *    Copyright 1994 O'Reilly and Associates, Inc.
 *
 *    The X Consortium, and any party obtaining a copy of these files from
 *    the X Consortium, directly or indirectly, is granted, free of charge, a
 *    full and unrestricted irrevocable, world-wide, paid up, royalty-free,
 *    nonexclusive right and license to deal in this software and
 *    documentation files (the "Software"), including without limitation the
 *    rights to use, copy, modify, merge, publish, distribute, sublicense,
 *    and/or sell copies of the Software, and to permit persons who receive
 *    copies from any such party to do so.  This license includes without
 *    limitation a license to do the foregoing actions under any patents of
 *    the party supplying this software to the X Consortium.
 */
/*
 * Window flags
 */
#define WTYP_OPER	0x01		/* Operator, +, -, =, etc */
#define WTYP_DIG	0x02		/* Digit 0 - 9, A - F */
#define WTYP_DISP	0x04		/* Display Window */
#define WTYP_CONV	0x08		/* Converter - hex, oct, dec, bin */
#define WTYP_SPEC	0x10		/* Special, CE, CA, CD */

/*
 * Operators
 */
#define OPR_ADD		1
#define OPR_SUB		2
#define OPR_MUL		3
#define OPR_DIV		4
#define OPR_MOD		5
#define OPR_OR		6
#define OPR_AND		7
#define OPR_XOR		8
#define OPR_SHL		9
#define OPR_SHR		10
#define OPR_CLRE	11
#define OPR_CLRD	12
#define OPR_CLRA	13
#define OPR_ASGN	14
#define OPR_NEG		15
#define OPR_NOT		16
#define OPR_UNS		17

/*
 * Colors
 */
#define	WHITE	0
#define	BLACK	1
#define	DARKGRAY	2
#define	LIGHTGRAY	3

static XrmDatabase commandlineDB, rDB;

int pressedColor =	WHITE;
int unpressedColor =	BLACK;
int disabledColor =	LIGHTGRAY;
int displayColor =	WHITE;

char myDisplayName[256];
char *myFontName = 	"8x13";

char		*calcName;

Display		*display;
int		screen_number;
Visual		*visual;
Colormap	colormap;
XFontStruct	*theFont;
Cursor 		theCursor;
Window 		calcWin;
Window 		iconWin;
Window 		dispWin;
Pixmap		lgrayPixmap;
Pixmap		grayPixmap;
Pixmap		iconPixmap;

int		foreground;
int		background;

GC		fgGC;
GC		bgGC;

/*
 * Calculator variables
 */
int		Base = 10; /* default base, updated to command line or .Xdefaults */
int		Winbase;   /* windata offset for current base, set in InitCalc, used in ConvButton */
int 		Digit = 0;
long		Value = 0;		/* current pressed value */
long		Accum = 0; 		/* current results */
Bool		Unsigned = True; 	/* default for U/S key */
int		LastOpt = OPR_ADD; 	/* initial previous operator */
int 		CalcReset = 0;
char		Hexmeasure[] = "   .   .   .   .   .   .   .   ";
char		Octmeasure[] = " .  .  .  .  .  .  .  .  .  .  ";


	
/*
 * Startup options.
 */
Bool iconOnly = False;

char Geostr[20];
char iconGeostr[20];


/* Command line options table.  Only resources are entered here...there is a
   pass over the remaining options after XrmParseCommand is let loose. 
   We don't do anything with many of these resources, but the program is ready for
   expansion */

#define GEOMETRY	"*geometry"
#define ICONGEOMETRY	"*iconGeometry"
#define UNSIGNED	"*unsigned"
#define BASE		"*base"
#define ICONSTARTUP		"*iconStartup"

static int opTableEntries = 25;
static XrmOptionDescRec opTable[] = {
{"-unsigned",		UNSIGNED,		XrmoptionNoArg,		(caddr_t) "off"},
{"-x",		BASE,		XrmoptionNoArg,		(caddr_t) "16"},
{"-hex",		BASE,		XrmoptionNoArg,		(caddr_t) "16"},
{"-dec",		BASE,	 	XrmoptionNoArg,		(caddr_t) "10"},
{"-oct",		BASE,	 	XrmoptionNoArg,		(caddr_t) "8"},
{"-binary",		BASE,	 	XrmoptionNoArg,		(caddr_t) "2"},
{"-geometry",  GEOMETRY,      XrmoptionSepArg,        (caddr_t) NULL},
{"-iconGeometry",  ICONGEOMETRY,      XrmoptionSepArg,        (caddr_t) NULL},
{"-iconic",    ICONSTARTUP,        XrmoptionNoArg, (caddr_t) "on"},
{"-background",        "*background",  XrmoptionSepArg,        (caddr_t) NULL},
{"-bg",        "*background",  XrmoptionSepArg,        (caddr_t) NULL},
{"-fg",        "*foreground",  XrmoptionSepArg,        (caddr_t) NULL},
{"-foreground",        "*foreground",  XrmoptionSepArg,        (caddr_t) NULL},
{"-xrm",       NULL,   XrmoptionResArg,        (caddr_t) NULL},
{"-display",   ".display",     XrmoptionSepArg,        (caddr_t) NULL},
/*remainder not currently supported: */
{"-bd",        "*borderColor", XrmoptionSepArg,        (caddr_t) NULL},
{"-bordercolor",       "*borderColor", XrmoptionSepArg,        (caddr_t) NULL},
{"-borderwidth",       ".borderWidth",   XrmoptionSepArg,        (caddr_t) NULL},
{"-bw",        ".borderWidth",   XrmoptionSepArg,        (caddr_t) NULL},
{"-fn",        "*font",        XrmoptionSepArg,        (caddr_t) NULL},
{"-font",      "*font",        XrmoptionSepArg,        (caddr_t) NULL},
{"-name",      ".name",        XrmoptionSepArg,        (caddr_t) NULL},
{"-title",     ".title", XrmoptionSepArg,        (caddr_t) NULL},
};



/*
 * Keyboard equivalents
 */
struct KeyCode {
	int	kc_char;
	char 	*kc_func;
	int	kc_len;
} KeyCodes[] = {
	{ CERASE,	"CD",	2 },
#ifdef SysV
	{ 027,		"CE",	2 },
#else
	{ CWERASE,	"CE",	2 },
#endif SysV
	{ CKILL,	"CE",	2 },
	{ CINTR,	"CA",	2 },
	{ 0,		0,	0 },
};
char	QuitChar = CQUIT;

#include "bitmaps/xcalc.icon"
#include "bitmaps/lgray"
#include "bitmaps/gray"

/*
 * Placement variables
 */
XSizeHints sizehints = {
	PMinSize | PMaxSize | PPosition | PSize | USSize,
	400, 100,       /* x, y */
	300, 139,       /* width, height */
	300, 139,       /* min_width and min_height */
	300, 139,       /* max_width and max_height */
	0, 0,           /* width and height increments, not set */
	0, 0, 0, 0,     /* aspect ratio, not set */
};
XSizeHints iconsizehints = {
	PMinSize | PMaxSize | PPosition | PSize,
	150, 2,
	icon_width, icon_height,
	icon_width, icon_height,
	icon_width, icon_height,
	0, 0,
	0, 0, 0, 0,
};
XWMHints wmhints = {
	InputHint | StateHint | IconWindowHint,
	True,          /* input model */
	NormalState,    /* starts up in normal state */
	0,              /* icon pixmap - set later */
	0,              /* icon window - created later */
	150, 2,         /* icon position of icon */
	0,              /* icon mask pixmap  - not used */
};


/*
 * Configuration of sub-windows.
 */
typedef struct _OpaqueFrame {
	Window self;			/* window id, filled in later */
	int x, y;			/* where to create the window */
	unsigned int width, height;	/* width and height */
} OpaqueFrame;


#define NBUTTONS 38
OpaqueFrame Buttons[NBUTTONS] = {
	{ 0, 3, 5, 292, 18},	/* display area */

	{ 0, 10, 35, 19, 18},	/* c d e f */
	{ 0, 37, 35, 19, 18},
	{ 0, 63, 35, 19, 18},
	{ 0, 91, 35, 19, 18},

	{ 0, 10, 60, 19, 18},	/* 8 9 a b */
	{ 0, 37, 60, 19, 18},
	{ 0, 63, 60, 19, 18},
	{ 0, 91, 60, 19, 18},

	{ 0, 10, 85, 19, 18},	/* 4 5 6 7 */
	{ 0, 37, 85, 19, 18},
	{ 0, 63, 85, 19, 18},
	{ 0, 91, 85, 19, 18},

	{ 0, 10, 110, 19, 18},	/* 0 1 2 3 */
	{ 0, 37, 110, 19, 18},
	{ 0, 63, 110, 19, 18},
	{ 0, 91, 110, 19, 18},

	{ 0, 261, 110, 28, 18}, /* ca ce, cd, = */
	{ 0, 187, 110, 28, 18},
	{ 0, 224, 110, 28, 18},
	{ 0, 131, 110, 46, 18},

	{ 0, 131, 60, 19, 18}, /* + - * / % */
	{ 0, 158, 60, 19, 18},
	{ 0, 185, 60, 19, 18},
	{ 0, 212, 60, 19, 18},
	{ 0, 239, 60, 19, 18},

	{ 0, 131, 85, 19, 18}, /* | & << >> ^ */
	{ 0, 158, 85, 19, 18},
	{ 0, 185, 85, 19, 18},
	{ 0, 212, 85, 19, 18},
	{ 0, 239, 85, 19, 18},

	{ 0, 131, 35, 32, 18}, /* hex oct bin dec */
	{ 0, 165, 35, 31, 18},
	{ 0, 198, 35, 32, 18},
	{ 0, 232, 35, 31, 18},

	{ 0, 269, 35, 20, 18}, /* UNS */
	{ 0, 269, 60, 20, 18}, /* NEG */
	{ 0, 269, 85, 20, 18}, /* NOT */
};

char dtext[33];

struct windata {
	int	color;	/* color */
	char	*text;	/* pointer to the text string */
	int	x;		/* x coordinate of text */
	int	y;		/* y coordinate of text */
	int	value;	/* 0 - 16 for number, symbol for operator */
	int	type;	/* number, operator, display */
} windata[NBUTTONS] = {
	{ 1, "                               0 ", 2, 3, 0, WTYP_DISP },

	{ 0, "C", 5, 3, 12, WTYP_DIG },
	{ 0, "D", 5, 3, 13, WTYP_DIG },
	{ 0, "E", 5, 3, 14, WTYP_DIG },
	{ 0, "F", 5, 3, 15, WTYP_DIG },

	{ 0, "8", 5, 3,  8, WTYP_DIG },
	{ 0, "9", 5, 3,  9, WTYP_DIG },
	{ 0, "A", 5, 3, 10, WTYP_DIG },
	{ 0, "B", 5, 3, 11, WTYP_DIG },

	{ 0, "4", 5, 3, 4, WTYP_DIG },
	{ 0, "5", 5, 3, 5, WTYP_DIG },
	{ 0, "6", 5, 3, 6, WTYP_DIG },
	{ 0, "7", 5, 3, 7, WTYP_DIG },

	{ 0, "0", 5, 3, 0, WTYP_DIG },
	{ 0, "1", 5, 3, 1, WTYP_DIG },
	{ 0, "2", 5, 3, 2, WTYP_DIG },
	{ 0, "3", 5, 3, 3, WTYP_DIG },

	{ 0, "CA", 6, 3, OPR_CLRA, WTYP_SPEC },
	{ 0, "CE", 6, 3, OPR_CLRE, WTYP_SPEC },
	{ 0, "CD", 6, 3, OPR_CLRD, WTYP_SPEC },
	{ 0, "=", 17, 2, OPR_ASGN, WTYP_OPER },

	{ 0,  "+", 5, 3, OPR_ADD, WTYP_OPER },
	{ 0,  "-", 5, 3, OPR_SUB, WTYP_OPER },
	{ 0,  "*", 5, 4, OPR_MUL, WTYP_OPER },
	{ 0,  "/", 5, 3, OPR_DIV, WTYP_OPER },
	{ 0,  "%", 5, 3, OPR_MOD, WTYP_OPER },

	{ 0,  "|", 5, 3, OPR_OR,  WTYP_OPER },
	{ 0,  "&", 5, 3, OPR_AND, WTYP_OPER },
	{ 0,  ">>",1, 3, OPR_SHR, WTYP_OPER },
	{ 0,  "<<",0, 3, OPR_SHL, WTYP_OPER },
	{ 0,  "^", 5, 3, OPR_XOR, WTYP_OPER },

	{ 0, "HEX", 2, 3, 16, WTYP_CONV },
	{ 0, "DEC", 2, 3, 10, WTYP_CONV },
	{ 0, "OCT", 2, 3,  8, WTYP_CONV },
	{ 0, "BIN", 2, 3,  2, WTYP_CONV },

	{ 0, "U",  5, 3,  OPR_UNS, WTYP_SPEC },
	{ 0, "`",  5, 3,  OPR_NEG, WTYP_OPER },
	{ 0, "~",  5, 3,  OPR_NOT, WTYP_OPER },
};

