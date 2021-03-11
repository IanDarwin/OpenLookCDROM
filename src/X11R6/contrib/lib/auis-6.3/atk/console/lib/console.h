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
 *****************************************************************
 * console.h 
 *****************************************************************
 */

#ifndef boolean
#include <class.h>
#endif /* boolean */

/* needed information for putting together console menus */
#define MAXCONFILES 100

#define MAXLIBS 20

typedef struct {
    char *path;
    char *confile;
} ConLibType;

extern ConLibType conlib[];
extern char *libpaths[];

extern long libnum;
extern long connum;
extern long BasicLib;
extern long LocalLib;
extern long UserLib;
/* --------------------*/

extern char ConFile[1024];
extern int MYDEBUGGING;

#define TRUE 1
#define true 1
#define FALSE 0
#define false 0
#ifndef MAXGETSTATSCOUNTERS
#include <getstats.h>
#endif 

#define MARINERFETCH (MAXGETSTATSCOUNTERS + 1)
#define MARINEROTHER (MAXGETSTATSCOUNTERS + 2)
#define CLOCKHOURS (MAXGETSTATSCOUNTERS + 3)
#define CLOCKHOURFIFTHS (MAXGETSTATSCOUNTERS + 4)
#define CLOCKMINUTES (MAXGETSTATSCOUNTERS + 5)
#define CLOCKSECONDS (MAXGETSTATSCOUNTERS + 6)
#define CLOCKALL (MAXGETSTATSCOUNTERS + 7)
#define DATE (MAXGETSTATSCOUNTERS + 8)
#define MAIL (MAXGETSTATSCOUNTERS + 9)
#define ERRORS (MAXGETSTATSCOUNTERS + 10)
#define ALARM (MAXGETSTATSCOUNTERS + 11)
#define ALWAYS (MAXGETSTATSCOUNTERS + 12)
#define TROUBLE (MAXGETSTATSCOUNTERS + 13)
#define ERRORLOG (MAXGETSTATSCOUNTERS + 14)
#define REPORTLOG (MAXGETSTATSCOUNTERS + 15)
#define USERLOG (MAXGETSTATSCOUNTERS + 16)
#define SILLYLOG (MAXGETSTATSCOUNTERS + 17)
#define WINDOWUSE (MAXGETSTATSCOUNTERS + 18)
#define DIRECTORY1 (MAXGETSTATSCOUNTERS + 19)
#define DIRECTORY2 (MAXGETSTATSCOUNTERS + 20)
#define DIRECTORY3 (MAXGETSTATSCOUNTERS + 21)
#define DIRECTORY4 (MAXGETSTATSCOUNTERS + 22)
#define DIRECTORY5 (MAXGETSTATSCOUNTERS + 23)
#define DIRECTORY6 (MAXGETSTATSCOUNTERS + 24)
#define DIRECTORY7 (MAXGETSTATSCOUNTERS + 25)
#define DIRECTORY8 (MAXGETSTATSCOUNTERS + 26)
#define DIRECTORY9 (MAXGETSTATSCOUNTERS + 27)
#define DIRECTORY10 (MAXGETSTATSCOUNTERS + 28)
#define DIRECTORY11 (MAXGETSTATSCOUNTERS + 29)
#define DIRECTORY12 (MAXGETSTATSCOUNTERS + 30)
#define DIRECTORY13 (MAXGETSTATSCOUNTERS + 31)
#define DIRECTORY14 (MAXGETSTATSCOUNTERS + 32)
#define DIRECTORY15 (MAXGETSTATSCOUNTERS + 33)
#define DIRECTORY16 (MAXGETSTATSCOUNTERS + 34)
#define DIRECTORY17 (MAXGETSTATSCOUNTERS + 35)
#define DIRECTORY18 (MAXGETSTATSCOUNTERS + 36)
#define DIRECTORY19 (MAXGETSTATSCOUNTERS + 37)
#define DIRECTORY20 (MAXGETSTATSCOUNTERS + 38)
#define DIRECTORY21 (MAXGETSTATSCOUNTERS + 39)
#define DIRECTORY22 (MAXGETSTATSCOUNTERS + 40)
#define DIRECTORY23 (MAXGETSTATSCOUNTERS + 41)
#define DIRECTORY24 (MAXGETSTATSCOUNTERS + 42)
#define DIRECTORY25 (MAXGETSTATSCOUNTERS + 43)
#define DIRECTORY26 (MAXGETSTATSCOUNTERS + 44)
#define DIRECTORY27 (MAXGETSTATSCOUNTERS + 45)
#define DIRECTORY28 (MAXGETSTATSCOUNTERS + 46)
#define DIRECTORY29 (MAXGETSTATSCOUNTERS + 47)
#define DIRECTORY30 (MAXGETSTATSCOUNTERS + 48)
#define DIRECTORY31 (MAXGETSTATSCOUNTERS + 49)
#define DIRECTORY32 (MAXGETSTATSCOUNTERS + 50)
#define DIRECTORY33 (MAXGETSTATSCOUNTERS + 51)
#define DIRECTORY34 (MAXGETSTATSCOUNTERS + 52)
#define DIRECTORY35 (MAXGETSTATSCOUNTERS + 53)
#define DIRECTORY36 (MAXGETSTATSCOUNTERS + 54)
#define DIRECTORY37 (MAXGETSTATSCOUNTERS + 55)
#define DIRECTORY38 (MAXGETSTATSCOUNTERS + 56)
#define DIRECTORY39 (MAXGETSTATSCOUNTERS + 57)
#define DIRECTORY40 (MAXGETSTATSCOUNTERS + 58)
#define DIRECTORY41 (MAXGETSTATSCOUNTERS + 59)
#define LASTDIRECTORY (MAXGETSTATSCOUNTERS + 59)
#define PRINTQUEUE (MAXGETSTATSCOUNTERS + 60)
#define PRINTSENT (MAXGETSTATSCOUNTERS + 61)
#define PRINTERRORS (MAXGETSTATSCOUNTERS + 62)
#define MARINERFINISHED (MAXGETSTATSCOUNTERS + 63)
#define VICEPERSONAL (MAXGETSTATSCOUNTERS + 64)
#define VICEPARTITION (MAXGETSTATSCOUNTERS + 65)
#define NETRESPONSES (MAXGETSTATSCOUNTERS + 66)
#define UDPIDLE (MAXGETSTATSCOUNTERS + 67)
#define UNAUTHENTICATED (MAXGETSTATSCOUNTERS + 68)
#define OUTGOINGMAIL (MAXGETSTATSCOUNTERS + 69)

#define DisplayTypeCount (MAXGETSTATSCOUNTERS + 69)
/* Should agree with last of above defines */
/* Also, needs to agree with several arrays in setup.c:
		DefaultClickStrings
		FunctionTable
 */
#define EXTERNAL1 LASTDIRECTORY
#define EXTERNAL2 (LASTDIRECTORY-1)
#define EXTERNAL3 (LASTDIRECTORY-2)
#define EXTERNAL4 (LASTDIRECTORY-3)
#define EXTERNAL5 (LASTDIRECTORY-4)
#define EXTERNAL6 (LASTDIRECTORY-5)
#define EXTERNAL7 (LASTDIRECTORY-6)
#define EXTERNAL8 (LASTDIRECTORY-7)
#define EXTERNAL9 (LASTDIRECTORY-8)
#define EXTERNAL10 (LASTDIRECTORY-9)
#define EXTERNAL11 (LASTDIRECTORY-10)
#define EXTERNAL12 (LASTDIRECTORY-11)
#define EXTERNAL13 (LASTDIRECTORY-12)
#define EXTERNAL14 (LASTDIRECTORY-13)
#define EXTERNAL15 (LASTDIRECTORY-14)
#define EXTERNAL16 (LASTDIRECTORY-15)
#define EXTERNAL17 (LASTDIRECTORY-16)
#define EXTERNAL18 (LASTDIRECTORY-17)
#define EXTERNAL19 (LASTDIRECTORY-18)
#define NUMEXTERNALS 20 /* Change this if you add externals */
#define EXTERNAL20 (LASTDIRECTORY-19)

/* the maximum number of child processes; this used to be hardcoded in setup.c as 127 */
#define MAXCHILDPIDS 255
/* defined as an int, it's really of type pid; ugh. --MKM */
extern int theGetstatsPid;

extern int errno, sys_nerr;
extern char *sys_errlist[];

extern FILE *ErrorsIn,
    *VenusIn,
    *ConsoleIn;
extern char ErrTxt[256];
extern boolean ErrorInputPending,
    VenusInputPending,
    WindowInputPending,
    ConsoleSocketInputPending;
extern int LastMailMod;
extern char *envmail;

#define DATAMAX 100
#define DATAMIN 75
/* Flags for Labels */
#define NO_LABEL 0
#define TOP_LABEL 1
#define RIGHT_LABEL 2
#define LEFT_LABEL 3
#define BOTTOM_LABEL 4
/* Flags for Click actions */
#define ERRORREGIONLOG 0
#define REPORTREGIONLOG 1
#define USERREGIONLOG 2
#define SILLYREGIONLOG 3
#define NUMREGIONLOGS 3 /* Must agree with above line */

#define BIT(x) :x

struct datum {
    struct display *FirstDisplay;
    int Value, *Valuelist;
    char *RawText, *ExtName;
    short ValueCtr;
    int IsDisplaying BIT(1), /* Whether or not to bother */
        AnAlwaysUpdate BIT(1),
        NeedsUpdate BIT(1);
};
struct display {
    struct datum *WhatToDisplay; /* pointer to actual datum */
    struct display *NextDisplay; /* a linked list, all for the thing displayed */
    struct display *NextOfAllDisplays; /* a linked list of ALL displays */
    struct RegionLog *AssociatedLog; /* includes text (log) data object */
    struct logview *AssociatedLogView;/* view of text (log) data object */
    struct scroll *ScrollLogView; /* temporary */
    struct fontdesc *Labelfont, *Textfont;
    char *ClickStringLeft, *NameOfTextFont, *NameOfLabelFont,
	*disptext,  *label;
    int (*PreDrawFunction)();
    int (*DrawFunction)();  /* To init, draw, or redraw */
    int ValueMax,  /* Alleged maximum value, for %. */
        Threshhold,  /* Threshold for displaying */
        Ceiling,    /* Ceiling for displaying */
        LastClickValue, /* To temporarily turn off flashing */
        FlashMin, FlashMax, displayparam1, displayparam2;
    short FlashStyle,  /* really highlights instead of flashing if non-zero */
        WhichVariable, HandLength, HandWidth, DisplayStyle,
        RelXmin, RelXmax, RelYmin, RelYmax,
        Xmin, Xmax, Ymin, Ymax,
        Width, FullHeight, XCenter, YCenter,
        MaxLabelFontSize, MaxTextFontSize,
        IsLabelling, /* Actually not a boolean -- takes several values */
        AdjustLabelFont, /* ditto */
        AdjustTextFont; /* ditto */
    int ParseDisplayText BIT(1),
        Boxed BIT(1),
        Clipped BIT(1),
        InRange BIT(1),
        WhiteOut BIT(1),
        Iconic BIT(1),
        IsTexting BIT(1),
        Trouble BIT(1),
        Inverted BIT(1),
        MayFlash BIT(1),
        ClickWhenInvisible BIT(1),
        DependentUponVariables BIT(1),
        UpdateAlways BIT(1),
        AppearIfTrue BIT(1);
} ;

struct InternalVariable {
    int InUse BIT(1), Value BIT(1);
    char *turnon, *turnoff;
};
extern int (*(functions[]))();
extern int (*(prefunctions[]))();

#define NUMINTERNALVARIABLES 20
extern int IntVarCount[NUMINTERNALVARIABLES + 1];
extern struct InternalVariable IntrnlVars[NUMINTERNALVARIABLES + 1];

extern struct datum Numbers[DisplayTypeCount + 1];
extern struct display *VeryFirstDisplay;
extern struct fontdesc *PromptFont;
extern struct fontdesc *EventFont;
extern char PromptFontName[50];
extern int Period,
    MinHeight,
    MaxHeight,
    MinWidth,
    MaxWidth,
    VMPollFreq,
    DiskPollFreq,
    MailPollFreq,
    WindowPollFreq,
    DirPollFreq,
    ClockPollFreq,
    VenusPollFreq,
    NetPollFreq,
    PrintPollFreq,
    FPAPollFreq;
extern boolean DoTroubleChecking,
    DoVenusChecking,
    DoVMStat,
    DoDiskFreeStat,
    DoVenusMonitoring,
    DoMailChecking,
    DoPrintChecking,
    DoDirChecking,
    DoCheckClock,
    DoWindows,
    DoLOAD,
    DoCPU,
    DoVM,
    DoPAGING,
    DoMEM,
    DoQUEUE,
    DoINTS,
    DoNDSTAT,
    DoNetChecking,
    DoPROCESSES;

extern char*FunctionTable[];
extern char*DisplayTypeTable[];

#define NEWVAL 1
#define REDRAW 2
#define MAXNUMFONTS 20
/* Display styles */
#define LOGINDICATOR 1
#define REPEATINDICATOR 2
#define DIALHIDDEN 3
#define LEFTINDICATOR 4
#define REVERSESCROLLING 5
#define SQUAREDIAL 6
#define HORIZONTAL 7
#define MENUCARDMAX 10 /* To limit the size of each menu card */

extern struct fontdesc *FontsAvail[MAXNUMFONTS];
extern char *AvailFontNames[MAXNUMFONTS];
extern int AvailFontPts[MAXNUMFONTS];
extern int FontCount;
extern int ScaleFactor;
extern boolean DidInitGVM,
    DidInitVenus,
    DidInitErrLog,
    DidInitPrinting;
extern int VMPollCt,
    DiskPollCt,
    MailPollCt,
    DirPollCt,
    WindowPollCt,
    ClockPollCt,
    VenusPollCt,
    NetPollCt,
    PrintPollCt,
    FPAPollCt;

extern char FontFamily[20];

#define REGIONLOGSIZE 200
struct RegionLog {
    struct text *TextLog;
    struct datum *WhichDatum;
    boolean ScrollReverse;
} ;

#define MAXCONSOLES 50
#define MAXCONSOLEPATHS 10
/* new for dealing with multiple paths by means of an array --MKM */
extern char *xPaths[MAXCONSOLEPATHS];
/* new for dealing with libNames as an array --MKM */
extern char *xNames[MAXCONSOLEPATHS];
extern char *ConLib[MAXCONSOLES];
extern int ConNum;

extern boolean PauseEnqueuedEvents;
extern boolean FPAchecked;
extern boolean RingingAlarm;
extern struct RegionLog RegionLogs[NUMREGIONLOGS + 1];

extern int ExternalsInUse;

#define mydbg(text) if (MYDEBUGGING) {printf text ;fflush(stdout);}
#define arrgh(text) {printf text ;fflush(stdout);}

extern int DefaultErrorPriority;
extern int HighestPriority;

extern char *Nullity;
extern char *StatusServer; /* for vopcons server machine */


