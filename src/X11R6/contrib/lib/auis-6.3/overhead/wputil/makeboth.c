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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wputil/RCS/makeboth.c,v 5.40 1993/08/31 15:57:48 rr2b Exp $";
#endif

/* ************************************************************ *\
	makeboth.c
	Construct the White Pages database.

		Use the ``runmakeboth'' script for production.
		Tracks a passwd file as it changes, applies a file of changes to its data,
		and writes a Grits base file containing the information from that file.

	Usage: makebt [-Dd] [-c conffile] wp passwd.chg newpasswd.chg oldetcpasswd newetcpasswd
		wp is updated, and newpasswd.chg then contains the updated change file.
		exits:	0 for all OK
			1 for bad argument syntax
			2 for errors in file reading
			3 for errors in file writing

\* ************************************************************ */

#include <andrewos.h>
#include <fdplumb.h>
#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>
#include <errno.h>
#include <truth.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <util.h>
#ifdef AMS_ENV
#include <mailconf.h>
#else /* AMS_ENV */
#include <svcconf.h>
#endif /* AMS_ENV */
/* #include <mail.h> */
#include <bt.h>
extern int errno;

#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

#include <wp.h>
#include "wppasswd.h"
static int WPOpen;
static struct wp_cd *cd;
static wp_ErrorCode WPOpenErr, WPerr;
static wp_FieldIndex FwdIx;
static wp_ErrorCode BTErrDum;

static int Debugging = 0;	/* 1 if -d given, 2 if -D, 0 if neither */
static int GiveStatus = 1;
static int TempSleeps = 0;
static char	*OutTreeDir,  /* filename arguments */
		*OutTreeFileInDir,
		*ConfFileName,
		*InChgFileName,
		*OutChgFileName,
		*OldPWFileName,
		*NewPWFileName;

static FILE	*ConfFile, *InChgFile, *OutChgFile, *OldPWFile, *NewPWFile;
static FILE	*InFwdFile;
static int InChgCommentSeen;
static char	InFwdFileName[400], InBoxFileName[400];
static struct BTree	*Tree;
static struct btCursor	*Curs;

static struct osi_Times TimeBuff;
static struct stat StatBuff;
static struct BTHead BTBuff;
static int UpdatePeriod = -1, UpdateSlot = -1;
static int NewAcctsOnly = 0, ThisNewBoring = 0, ForgetForward = 0;

/* Bits defining which streams still have live input waiting. */
#define NewPWActive 001
#define OldPWActive 002
#define InChgActive 004
#define AllActive (NewPWActive | OldPWActive | InChgActive)
static int	ActiveStreams;

static int ThisPass;

#define SHORTSTRINGSIZE	51
#define MEDIUMSTRINGSIZE	201
#define LONGSTRINGSIZE	2501


/* Data in the old and new password files */
static char	OldPWName[SHORTSTRINGSIZE], NewPWName[SHORTSTRINGSIZE],
		OldPWPass[SHORTSTRINGSIZE], NewPWPass[SHORTSTRINGSIZE],
		OldPWGecos[MEDIUMSTRINGSIZE], NewPWGecos[MEDIUMSTRINGSIZE],
		OldPWDir[MEDIUMSTRINGSIZE], NewPWDir[MEDIUMSTRINGSIZE],
		OldPWShell[MEDIUMSTRINGSIZE], NewPWShell[MEDIUMSTRINGSIZE];

struct wppasswd OldPW, NewPW;

/* Data from the change file */
static char	InChgEntry[SHORTSTRINGSIZE],
		InChgField[SHORTSTRINGSIZE],
		InChgOldVal[LONGSTRINGSIZE],
		InChgNewVal[LONGSTRINGSIZE],
		InChgStamp[MEDIUMSTRINGSIZE];
static long int	InChgStampValue;

static char	QuoteBuff1[LONGSTRINGSIZE], QuoteBuff2[LONGSTRINGSIZE];

#define NUMBIGGEST 10
static struct BigOnes {
	int Size;
	char Key[MEDIUMSTRINGSIZE];
} Bigs[NUMBIGGEST] = {0};

/* The NameChar table tells whether we believe that a given character should be part of, or separate, the individual whole-names in a whole-name index.  1=>part of a whole name; 0=>separate whole names. */
static char	NameChar[256] = {
	0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,	/* 0000-0017 ^@^a^b^c^d^e^f^g^h^i^j^k^l^m^n^o*/
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0020-0037 ^p^q^r^s^t^u^v^w^x^y^z^[^\^]^^^_*/
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0040-0057  !"#$%&'()*+,-./ */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1,	/* 0060-0077 0123456789:;<=>? */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0100-0117 @ABCDEFGHIJKLMNO */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0120-0137 PQRSTUVWXYZ[\]^_ */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0140-0157 `abcdefghijklmno */
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,	/* 0160-0177 pqrstuvwxyz{|}~^? */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0200-0217 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0220-0237 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0240-0257 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0260-0277 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0300-0317 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0320-0337 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,	/* 0340-0357 */
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}; /* 0360-0377 */

#include <btwp.h>

static char *BTTokTbls[NumBTIndices] = {wpTokenChar, NameChar, NameChar, NameChar,
					NameChar, wpTokenChar, wpTokenChar,
					NameChar, NameChar};
/* not used:
static char *oldBTTokTbls[NumBTIndices] = {wpTokenChar, NameChar, NameChar, NameChar,
					NameChar, NameChar, wpTokenChar,
					NameChar, NameChar};
static int BTCheckOld[NumBTIndices] = {0, 0, 0, 0, 0, 1, 0, 0, 0};
*/
static int BTIsSurn[NumBTIndices] = {0, 0, 0, 0, 1, 0, 0, 0, 0};

static int BTFieldSource[NumBTIndices] = {-1, -1, -1, -1, -1, FldN, FldTk, -1, -1};
static int BTFieldAltSource[NumBTIndices] = {-1, -1, -1, -1, -1, FldWN, -1, -1, -1};
static int BTFieldToWrite[NumBTIndices] = {-1, -1, -1, -1, -1, FldD, FldX, -1, -1};
static char BTWantSurn[NumBTIndices] = {0, 0, 0, 0, 0, 1, 0, 0, 0};

static char BTInhibit[NumBTIndices] = {0, 0, 0, 0, 0, 0, 0, 0, 0};

static char *BTFieldTokTbls[NumBTIndices] = {wpTokenChar, NameChar, NameChar, NameChar,
					NameChar, NameChar, wpTokenChar,
					NameChar, NameChar};

static int ConfigurableBTIndex[NumBTIndices] = {0, 0, 0, 0, 0, 0, 0, 1, 1};
#define MAXPasses 8
static int ConfigurablePassRequirements[MAXPasses+1] = {-1, -1, 0, 1, 1, 1, 1, 1, 1};

static int FieldBufSizes[FldCOUNT] = {
	MEDIUMSTRINGSIZE,	/* N */
	LONGSTRINGSIZE,	/* Tk */
	MEDIUMSTRINGSIZE,	/* WN */
	SHORTSTRINGSIZE,	/* ID */
	SHORTSTRINGSIZE,	/* EK */
	SHORTSTRINGSIZE,	/* NI */
	SHORTSTRINGSIZE,	/* GI */
	MEDIUMSTRINGSIZE,	/* passwd */
	MEDIUMSTRINGSIZE,	/* homedir */
	MEDIUMSTRINGSIZE,	/* Sh */
	MEDIUMSTRINGSIZE,	/* Af */
	LONGSTRINGSIZE,	/* Fwd */
	LONGSTRINGSIZE,	/* DK */
	LONGSTRINGSIZE,	/* DP */
	MEDIUMSTRINGSIZE,	/* D */
	MEDIUMSTRINGSIZE,	/* X */
	SHORTSTRINGSIZE,	/* SI */
	SHORTSTRINGSIZE,	/* NPA */
	SHORTSTRINGSIZE,	/* Dt */
	MEDIUMSTRINGSIZE,	/* A2 */
	LONGSTRINGSIZE,	/* HA */
	MEDIUMSTRINGSIZE,	/* HP */
	LONGSTRINGSIZE,	/* CA */
	MEDIUMSTRINGSIZE,	/* CX */
	LONGSTRINGSIZE,	/* OA */
	MEDIUMSTRINGSIZE,	/* OP */
	MEDIUMSTRINGSIZE,	/* FAX */
	MEDIUMSTRINGSIZE,	/* HFX */
	MEDIUMSTRINGSIZE,	/* CAF */
};
static char
	flN[MEDIUMSTRINGSIZE],
	flTk[LONGSTRINGSIZE],
	flWN[MEDIUMSTRINGSIZE],
	flID[SHORTSTRINGSIZE],
	flEK[SHORTSTRINGSIZE],
	flNI[SHORTSTRINGSIZE],
	flGI[SHORTSTRINGSIZE],
	flPW[MEDIUMSTRINGSIZE],
	flHD[MEDIUMSTRINGSIZE],
	flSh[MEDIUMSTRINGSIZE],
	flAf[MEDIUMSTRINGSIZE],
	flFwd[LONGSTRINGSIZE],
	flDK[LONGSTRINGSIZE],
	flDP[LONGSTRINGSIZE],
	flD[MEDIUMSTRINGSIZE],
	flX[MEDIUMSTRINGSIZE],
	flSI[SHORTSTRINGSIZE],
	flNPA[SHORTSTRINGSIZE],
	flDt[SHORTSTRINGSIZE],
	flA2[MEDIUMSTRINGSIZE],
	flHA[LONGSTRINGSIZE],
	flHP[MEDIUMSTRINGSIZE],
	flCA[LONGSTRINGSIZE],
	flCX[MEDIUMSTRINGSIZE],
	flOA[LONGSTRINGSIZE],
	flOP[MEDIUMSTRINGSIZE],
	flFAX[MEDIUMSTRINGSIZE],
	flHFX[MEDIUMSTRINGSIZE],
	flCAF[MEDIUMSTRINGSIZE];
static char *FieldContent[FldCOUNT] = {
	flN,
	flTk,
	flWN,
	flID,
	flEK,
	flNI,
	flGI,
	flPW,
	flHD,
	flSh,
	flAf,
	flFwd,
	flDK,
	flDP,
	flD,
	flX,
	flSI,
	flNPA,
	flDt,
	flA2,
	flHA,
	flHP,
	flCA,
	flCX,
	flOA,
	flOP,
	flFAX,
	flHFX,
	flCAF};
static char *OldPWFields[FldCOUNT] = {
	OldPWGecos,
	NULL,
	NULL,
	OldPWName,
	NULL,
	NULL,	/* really the pw_uid */
	NULL,	/* really the pw_gid */
	OldPWPass,
	OldPWDir,
	OldPWShell,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL };
static int FieldChanged[FldCOUNT];
static int HaveAnEntry;

/* And now for a descriptor for the old content of the B-tree entry */
static char
	oflN[MEDIUMSTRINGSIZE],
	oflTk[LONGSTRINGSIZE],
	oflWN[MEDIUMSTRINGSIZE],
	oflID[SHORTSTRINGSIZE],
	oflEK[SHORTSTRINGSIZE],
	oflNI[SHORTSTRINGSIZE],
	oflGI[SHORTSTRINGSIZE],
	oflPW[MEDIUMSTRINGSIZE],
	oflHD[MEDIUMSTRINGSIZE],
	oflSh[MEDIUMSTRINGSIZE],
	oflAf[MEDIUMSTRINGSIZE],
	oflFwd[LONGSTRINGSIZE],
	oflDK[LONGSTRINGSIZE],
	oflDP[LONGSTRINGSIZE],
	oflD[MEDIUMSTRINGSIZE],
	oflX[MEDIUMSTRINGSIZE],
	oflSI[SHORTSTRINGSIZE],
	oflNPA[SHORTSTRINGSIZE],
	oflDt[SHORTSTRINGSIZE],
	oflA2[MEDIUMSTRINGSIZE],
	oflHA[LONGSTRINGSIZE],
	oflHP[MEDIUMSTRINGSIZE],
	oflCA[LONGSTRINGSIZE],
	oflCX[MEDIUMSTRINGSIZE],
	oflOA[LONGSTRINGSIZE],
	oflOP[MEDIUMSTRINGSIZE],
	oflFAX[MEDIUMSTRINGSIZE],
	oflHFX[MEDIUMSTRINGSIZE],
	oflCAF[MEDIUMSTRINGSIZE];
static char *OldFieldContent[FldCOUNT] = {
	oflN,
	oflTk,
	oflWN,
	oflID,
	oflEK,
	oflNI,
	oflGI,
	oflPW,
	oflHD,
	oflSh,
	oflAf,
	oflFwd,
	oflDK,
	oflDP,
	oflD,
	oflX,
	oflSI,
	oflNPA,
	oflDt,
	oflA2,
	oflHA,
	oflHP,
	oflCA,
	oflCX,
	oflOA,
	oflOP,
	oflFAX,
	oflHFX,
	oflCAF };

/*VARARGS1*/
static void DebOut(Fmt, p1, p2, p3, p4, p5, p6, p7, p8, p9)
char *Fmt, *p1, *p2, *p3, *p4, *p5, *p6, *p7, *p8, *p9;
{/* Timestamped debugging trace. */
	struct tm *This;
	struct osi_Times TV;

	osi_GetTimes(&TV);
	This = localtime(&TV.Secs);
	fprintf(stdout, "%02d:%02d:%02d.%02d ", This->tm_hour, This->tm_min,
		This->tm_sec, TV.USecs / 10000);
	fprintf(stdout, Fmt, p1, p2, p3, p4, p5, p6, p7, p8, p9);
}

static char ToLower(C)
char C;
{ if (C <= 'Z' && C >= 'A') return C + ('a' - 'A');
   else return C;
}

static void LowerAll(S)
char *S;
{  while (*S != '\0') {
	if (*S <= 'Z' && *S >= 'A') *S += ('a' - 'A');
	++S;
    }
}

static void TrackBig(Key, Size)
char *Key; int Size;
{/* Record that we saw a value in the B-tree of size Size with key Key in the table of the biggest values. */
	int B, C;

	if ((Size + (3*PKLEN)) < Bigs[NUMBIGGEST-1].Size) return;

	for (B = 0; B < NUMBIGGEST; ++B) {
		if (strcmp(Key, Bigs[B].Key) == 0) {
			if (Size == Bigs[B].Size) return;	/* already there, no change needed */
			for (C = B+1; C < NUMBIGGEST; ++C) {	/* delete B record */
				Bigs[C-1].Size = Bigs[C].Size;
				strncpy(Bigs[C-1].Key, Bigs[C].Key, MEDIUMSTRINGSIZE);
			}
			/* Now fall through and re-insert current record in proper place for new size */
			break;
		}
	}
	for (B = 0; B < NUMBIGGEST; ++B) {
		if (Size > Bigs[B].Size) {
			for (C = NUMBIGGEST-1; C > B; --C) {
				Bigs[C].Size = Bigs[C-1].Size;
				strncpy(Bigs[C].Key, Bigs[C-1].Key, MEDIUMSTRINGSIZE);
			}
			Bigs[B].Size = Size;
			C = strlen(Key) + 1;
			if (C > MEDIUMSTRINGSIZE) C = MEDIUMSTRINGSIZE;
			strncpy(Bigs[B].Key, Key, C);
			break;
		}
	}
}

static void PrintBig(desc)
FILE *desc;
{/* Print (on desc) the biggest N values. */
	int B;

	fprintf(desc, "The biggest %d values encountered in this run were for keys:\n",
			NUMBIGGEST);
	for (B = 0; B < NUMBIGGEST; ++B) {
		if (Bigs[B].Size > 0) fprintf(desc, "\t%d:\t``%s''\n", Bigs[B].Size, Bigs[B].Key);
	}
}

static void ParseArguments(argc,argv)
    int	argc;
    char	**argv;
{
    static char UsageArgs[] = " [-dDrRwWNfFE] [-Mmalloclevel] [-c conf] [-Pperiod -pwhich] [-s treerootname] outtreedir inchg outchg oldpass curpasswd";
    int	thisarg, argval;
    char *Swch;

    OutTreeDir = OutChgFileName = InChgFileName = ConfFileName =
		OldPWFileName = NewPWFileName = NULL;
    OutTreeFileInDir = "wp";

    for (thisarg = 1; thisarg < argc && argv[thisarg][0] == '-'; ++thisarg) {
		Swch = &argv[thisarg][1];
		if (strcmp(Swch, "d") == 0) Debugging = 1;
		else if (strcmp(Swch, "D") == 0) Debugging = 2;
		else if (strcmp(Swch, "r") == 0) (void) btr_SetDebugging(1);
		else if (strcmp(Swch, "R") == 0) (void) btr_SetDebugging(2);
		else if (strcmp(Swch, "w") == 0) (void) btw_SetDebugging(1);
		else if (strcmp(Swch, "W") == 0) (void) btw_SetDebugging(2);
		else if (*Swch == 'M') {
			argval = 4;	/* default value */
			++Swch;
			if (*Swch >= '0' && *Swch <= '9') argval = atoi(Swch);
			else if (*Swch != '\0') {
				fprintf(stderr, "Bad argument to -M: ``%s''\nusage: %s %s\n",
					Swch, argv[0], UsageArgs);
				exit(1);
			}
#ifdef ANDREW_MALLOC_ENV
			(void) SetMallocCheckLevel(argval);
#else /* #ifdef ANDREW_MALLOC_ENV */
			(void) fprintf(stderr, "``-M%s'' switch ineffectual: program constructed without ANDREW_MALLOC_ENV defined.\n", Swch);
#endif /* #ifdef ANDREW_MALLOC_ENV */
		}
		else if (strcmp(Swch, "c") == 0) {
			++thisarg;
			if (thisarg >= argc) {
				fprintf(stderr, "Missing config file name.\nusage: %s %s\n",
					argv[0], UsageArgs);
				exit(1);
			}
			ConfFileName = NewString(argv[thisarg]);
		} else if (*Swch == 'P') UpdatePeriod = atoi(++Swch);
		else if (*Swch == 'p') UpdateSlot = atoi(++Swch);
		else if (strcmp(Swch, "N") == 0) NewAcctsOnly = 1;
		else if (strcmp(Swch, "F") == 0) ForgetForward = 1;
		else if (strcmp(Swch, "f") == 0) ForgetForward = 0;
		else if (strcmp(Swch, "E") == 0) BTInhibit[BTIxNWN] = 1;
		else if (strcmp(Swch, "s") == 0) {
		    ++thisarg;
		    if (thisarg >= argc) {
			fprintf(stderr, "Missing file name suffix.\nusage: %s %s\n", argv[0], UsageArgs);
			exit(1);
		    }
		    OutTreeFileInDir = argv[thisarg];
		} else {
			fprintf(stdout, "Unrecognized option: ``%s''\nusage: %s%s\n",
				argv[thisarg], argv[0], UsageArgs);
			exit(1);
		}
    }

    if (thisarg >= argc) {
	    fprintf(stderr, "Missing output tree name.\nusage: %s %s\n",
		argv[0], UsageArgs);
	    exit(1);
    }
    OutTreeDir = NewString(argv[thisarg]);

    thisarg++;
    if (thisarg >= argc) {
	    fprintf(stderr, "Missing input change file name.\nusage: %s %s\n",
		argv[0], UsageArgs);
	    exit(1);
    }
    InChgFileName = NewString(argv[thisarg]);

    thisarg++;
    if (thisarg >= argc) {
	    fprintf(stderr, "Missing output change file name.\nusage: %s %s\n",
		argv[0], UsageArgs);
	    exit(1);
    }
    OutChgFileName = NewString(argv[thisarg]);

    thisarg++;
    if (thisarg >= argc) {
	    fprintf(stderr, "Missing old password file name.\nusage: %s %s\n",
		argv[0], UsageArgs);
	    exit(1);
    }
    OldPWFileName = NewString(argv[thisarg]);

    thisarg++;
    if (thisarg >= argc) {
	    fprintf(stderr, "Missing new password file name.\nusage: %s %s\n",
		argv[0], UsageArgs);
	    exit(1);
    }
    NewPWFileName = NewString(argv[thisarg]);

    thisarg++;
    if (thisarg < argc) {
	    fprintf(stderr, "Extra arguments beginning with ``%s''.\nusage: %s %s\n",
		argv[thisarg], argv[0], UsageArgs);
	    exit(1);
    }
}

static void OpenGlobalFiles()
{
    int WhichHeader;
    struct tm *Now;
    char WriteWPName[1000];

    sprintf(WriteWPName, "%s/%s", OutTreeDir, OutTreeFileInDir);

    BTErrDum = bt_Open(&Tree, WriteWPName, "w");
    if (BTErrDum != bterr_NoError) {
	fprintf(stderr, "Cannot open B-tree ``%s'': %s.\n", WriteWPName,
		bt_ErrorString(BTErrDum));
	exit(3);
    }
    BTErrDum = bt_NewCursor(Tree, &Curs);
    if (BTErrDum != bterr_NoError) {
	fprintf(stderr, "Cannot create cursor into B-tree ``%s'': %s.\n", WriteWPName,
		bt_ErrorString(BTErrDum));
	exit(3);
    }

    WPOpen = FALSE;
    if (strcmp(OutTreeFileInDir, "wp") == 0) {
	WPOpenErr = wp_InitializeDir(OutTreeDir, &cd);
	if (WPOpenErr == wperr_NoError) {
	    FwdIx = wp_FieldNameToIndex("Fwd");
	    if (FwdIx < 0) {WPOpenErr = wperr_InternalError; cwp_Terminate(cd);}
	    else WPOpen = TRUE;
	}
    }

    ConfFile = NULL;
    if (ConfFileName != NULL) {
      ConfFile = fopen(ConfFileName, "r");
      if (ConfFile == NULL) {
	fprintf(stderr, "Cannot read configuration file ``%s'': %s.\n", ConfFileName,
		UnixError(errno));
	exit(2);
      }
    }

    osi_GetTimes(&TimeBuff);

    if (UpdatePeriod > 0) {
	if (UpdateSlot >= 0) UpdateSlot = UpdateSlot % UpdatePeriod;
	else {
		Now = localtime(&TimeBuff.Secs);
		UpdateSlot = Now->tm_yday % UpdatePeriod;
	}
    }

    if (GiveStatus) {
	BTErrDum = bt_GetFixedHead(Tree, &BTBuff, sizeof(BTBuff));
	if (BTErrDum == wperr_NoError) {
		printf("It is now %s.\n", NiceTime(TimeBuff.Secs));
		printf("Updating the B-tree ``%s'', which was created on %s, and whose root ",
			WriteWPName, NiceTime(BTBuff.bthCTime1));
		printf("was last modified by Vice UID %d on machine %x at %s.\n",
			BTBuff.bthMWho, BTBuff.bthMWhere,
			NiceTime(BTBuff.bthMTime1));
		printf("The B-tree is currently %d levels deep, with maximum node size %d.\n",
			BTBuff.bthDepth+1, BTBuff.bthMaxFileSize);
		printf("The B-tree format is version %d.\n", BTBuff.bthVersion);
		printf("\nThe database field names are:\n");
		for (WhichHeader = 0; WhichHeader <= FldMAX; WhichHeader++)
			printf("\t%s\n", wpFieldName[WhichHeader]);
	}
	if (ConfFile != NULL && fstat(fileno(ConfFile), &StatBuff) == 0)
		printf("\nUsing configuration file ``%s'' written by UID %d with length %d on %s.\n",
		ConfFileName, StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));
	if (UpdatePeriod > 0) {
		printf("\nDoing full validation for unchanged entries only when source-id = %d mod %d.\n",
			UpdateSlot, UpdatePeriod);
	}
	if (NewAcctsOnly) printf("\nNot even checking the old accounts in the password file.\n");
	if (ForgetForward) printf("\n**Not checking ~/.forward files for forwarding addresses.**\n");
	if (BTInhibit[BTIxNWN]) printf("\n**Not generating exact-name-match indices**\n");
    }
    bzero(&OldPW, sizeof(OldPW));
    OldPW.pw_name = OldPWName;
    OldPW.pw_passwd = OldPWPass;
    OldPW.pw_uid = 0;
    OldPW.pw_gid = 0;
    OldPW.pw_gecos = OldPWGecos;
    OldPW.pw_dir = OldPWDir;
    OldPW.pw_shell = OldPWShell;

    bzero(&NewPW, sizeof(NewPW));
    NewPW.pw_name = NewPWName;
    NewPW.pw_passwd = NewPWPass;
    NewPW.pw_uid = 0;
    NewPW.pw_gid = 0;
    NewPW.pw_gecos = NewPWGecos;
    NewPW.pw_dir = NewPWDir;
    NewPW.pw_shell = NewPWShell;
}

static void OpenPass1Files()
{
    InChgFile = fopen(InChgFileName, "r");
    if (InChgFile == NULL) {
	fprintf(stderr, "Cannot read input change file ``%s'': %s.\n", InChgFileName,
		UnixError(errno));
	exit(2);
    }
    InChgCommentSeen = 0;

    OldPWFile = fopen(OldPWFileName, "r");
    if (OldPWFile == NULL) {
	fprintf(stderr, "Cannot read old password file ``%s'': %s.\n", OldPWFileName,
		UnixError(errno));
	exit(2);
    }

    NewPWFile = fopen(NewPWFileName, "r");
    if (NewPWFile == NULL) {
	fprintf(stderr, "Cannot read current password file ``%s'': %s.\n", NewPWFileName,
		UnixError(errno));
	exit(2);
    }

    OutChgFile = fopen(OutChgFileName, "w");
    if (OutChgFile == NULL) {
	fprintf(stderr, "Cannot write output change file ``%s'': %s.\n", OutChgFileName,
		UnixError(errno));
	exit(3);
    }

    if (GiveStatus) {
	printf("\nPass 1 source files:\n");

	if (fstat(fileno(OldPWFile), &StatBuff) == 0)
		printf("\tOld password file, ``%s'', written by UID %d with length %d on %s.\n",
		OldPWFileName, StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));

	if (fstat(fileno(NewPWFile), &StatBuff) == 0)
		printf("\tNew password file, ``%s'', written by UID %d with length %d on %s.\n",
		NewPWFileName, StatBuff.st_uid, StatBuff.st_size,
		NiceTime(StatBuff.st_mtime));

	if (fstat(fileno(InChgFile), &StatBuff) == 0)
		printf("\tChange list file, ``%s'', written by UID %d with length %d on %s.\n",
		InChgFileName, StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));

	if (stat("/etc/passwd", &StatBuff) == 0) printf(
		"\tFor your information:  Current /etc/passwd file was written by UID %d with length %d on %s.\n\t(/etc/passwd is NOT explicitly used in makeboth.)\n",
		StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));

	if (stat("hist/passwd.chg", &StatBuff) == 0) printf(
		"\tFor your information: Current passwd.chg file was written by UID %d with length %d on %s.\n\t(passwd.chg is not used directly by makeboth.)\n",
		StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));

	printf("\n");
    }

    ActiveStreams = AllActive;

    HaveAnEntry = 0;
}

static void ClosePass1Files()
{
    int Rslt;

    ActiveStreams = 0;

    Rslt = fclose(OldPWFile);
    if (Rslt != 0) {
	fprintf(stderr, "Error closing old password file ``%s'': %s.\n", OldPWFileName,
		UnixError(errno));
	exit(2);
    }

    Rslt = fclose(NewPWFile);
    if (Rslt != 0) {
	fprintf(stderr, "Error closing current password file ``%s'': %s.\n", NewPWFileName,
		UnixError(errno));
	exit(2);
    }

    Rslt = fclose(InChgFile);
    if (Rslt != 0) {
	fprintf(stderr, "Error closing input change file ``%s'': %s.\n", InChgFileName,
		UnixError(errno));
	exit(2);
    }

    if (InChgCommentSeen == 0) fputs("# This comment will force a non-zero-length passwd.chg.\n", OutChgFile);
    if (ferror(OutChgFile)) {
	fprintf(stderr, "Error writing output change file ``%s'': %s.\n", OutChgFileName,
		UnixError(errno));
	fclose(OutChgFile);
	exit(3);
    }
    Rslt = vfclose(OutChgFile);
    if (Rslt != 0) {
	fprintf(stderr, "Error closing output change file ``%s'': %s.\n", OutChgFileName,
		UnixError(errno));
	exit(3);
    }
}

static void CloseGlobalFiles()
{
    int Rslt;

    if (GiveStatus) {
	printf("\nB-tree size summary:\n\n");
 	PrintBig(stdout);
	printf("\n");
	printf("Slept for temporary B-tree failures %d time%s.\n",
		TempSleeps, (TempSleeps == 1 ? "" : "s"));
	printf("\n");
    }

    BTErrDum = bt_Close(Tree);
    if (BTErrDum != bterr_NoError) {
	fprintf(stderr, "Error closing tree ``%s/%s'': %s.\n", OutTreeDir, OutTreeFileInDir, bt_ErrorString(BTErrDum));
	exit(3);
    }

    if (ConfFile != NULL) {
      Rslt = fclose(ConfFile);
      if (Rslt != 0) {
	fprintf(stderr, "Error closing configuration file ``%s'': %s.\n", ConfFileName,
		UnixError(errno));
	exit(2);
      }
    }

    if (WPOpen) {
	cwp_Terminate(cd);
	cd = NULL;
	WPOpen = FALSE;
    }
}

static void Trim(str, toElim)
char *str, *toElim;
{
/* Eliminate leading and trailing characters in the toElim set from the string str. */
	int LastChar, FirstChar, ThisChar;
	if (Debugging == 3) DebOut("Trim(%d:``%s'', %d:``%s'')\n",
		strlen(str), str, strlen(toElim), toElim);
	for (LastChar = strlen(str) - 1; LastChar >= 0; --LastChar) {
		if (index(toElim, str[LastChar]) == NULL) break;
		str[LastChar] = '\0';
	}
	for (FirstChar = 0; FirstChar < LastChar; ++FirstChar) {
	    if (index(toElim, str[FirstChar]) != NULL) str[FirstChar] = toElim[0];
	}
	for (FirstChar = 0; FirstChar < LastChar; ++FirstChar) {
		if (index(toElim, str[FirstChar]) == NULL) break;
	}
	if (FirstChar != 0) {
		LastChar++;	/* get the null, too */
		if (Debugging) DebOut(
			"Eliminating %d chars from beginning of ``%s''.\n", FirstChar, str);
		for (ThisChar = FirstChar; ThisChar <= LastChar; ++ThisChar) {
			str[ThisChar - FirstChar] = str[ThisChar];
		}
	}
}

static void StripMultiSpaces(str)
char *str;
{
/* Replace sequences of white space characters with a single space in the string str. */
	char *Src, *Dst;
	int InText, Changed;

	InText = Changed = FALSE;
	for (Src = Dst = str;*Src != '\0'; ++Src) {
		if (isspace(*Src)) {
			if (InText) {
				*Dst++ = ' ';
				InText = FALSE;
				if (*Src != ' ') Changed = TRUE;
			} else Changed = TRUE;
		} else {
			*Dst++ = *Src;
			InText = TRUE;
		}
	}
	if (Dst > str && Dst[-1] == ' ') {--Dst; Changed = TRUE;}
	*Dst = '\0';
	if (Debugging && Changed) DebOut("StripMultiSpaces produces ``%s''.\n", str);
}

static int GetOnePW(pwFile, Ptr, Lim, NLOK)
FILE *pwFile;
char *Ptr;
int Lim, NLOK;
{/* Read the next part of a passwd(5) entry. */
    char *XPtr; int InChar;

    XPtr = Ptr;
    for (;;) {
	InChar = getc(pwFile);
	if (InChar == EOF || InChar == ':' || InChar == '\n') break;
	*XPtr++ = InChar;
	if (--Lim <= 0) break;
    }
    *XPtr++ = '\0';
    if (NLOK) return (InChar == '\n' ? 0 : -1);
    else return (InChar == ':' ? 0 : -1);
}

static int ReadPWEntry(pwFile, pwStruct)
	FILE	*pwFile; struct passwd *pwStruct;
{
/* Returns 0 if got all entries, +n if masked entries were missing, -1 on EOF, -2
	on input read error, and -3 if some fields were unexpectedly missing. */

    int Res, OnesMissing, InChar, Dum;
    char Buff[50];

    pwStruct->pw_name[0] = pwStruct->pw_passwd[0] =
	pwStruct->pw_gecos[0] = pwStruct->pw_dir[0] = pwStruct->pw_shell[0] = '\0';
    pwStruct->pw_uid = pwStruct->pw_gid = -1;

    Res = 0;
    Dum = GetOnePW(pwFile, pwStruct->pw_name, 50, 0);
    if (Dum == 0) ++Res;
    Dum = GetOnePW(pwFile, pwStruct->pw_passwd, 50, 0);
    if (Dum == 0) ++Res;
    Dum = GetOnePW(pwFile, Buff, sizeof(Buff), 0);
    if (Dum == 0) {++Res; pwStruct->pw_uid = atoi(Buff);}
    Dum = GetOnePW(pwFile, Buff, sizeof(Buff), 0);
    if (Dum == 0) {++Res; pwStruct->pw_gid = atoi(Buff);}
    Dum = GetOnePW(pwFile, pwStruct->pw_gecos, 200, 0);
    if (Dum == 0) ++Res;
    Dum = GetOnePW(pwFile, pwStruct->pw_dir, 200, 0);
    if (Dum == 0) ++Res;
    Dum = GetOnePW(pwFile, pwStruct->pw_shell, 200, 1);
    if (Dum == 0) ++Res;

    if (Res != 7) {
	if (feof(pwFile) && ! ferror(pwFile)) return(-1);
	fprintf(stderr, "Error reading one of the password files, user ``%s'': %s.\n", pwStruct->pw_name, UnixError(errno));
	InChar = getc(pwFile);
	while (InChar != EOF && InChar != '\n') {InChar = getc(pwFile);}
	return(-2);
    }
    Trim(pwStruct->pw_name, " \b\t\n\v\f\r");
    Trim(pwStruct->pw_gecos, " \b\t\n\v\f\r");
    StripMultiSpaces(pwStruct->pw_gecos);
    Trim(pwStruct->pw_dir, " \b\t\n\v\f\r");
    Trim(pwStruct->pw_shell, " \b\t\n\v\f\r");
    if (pwStruct->pw_dir[0] != '/') pwStruct->pw_dir[0] = '\0';
    if (pwStruct->pw_shell[0] != '/') pwStruct->pw_shell[0] = '\0';

    OnesMissing = 0;
    if (pwStruct->pw_shell[0] == '\0') OnesMissing |= 01;
    if (pwStruct->pw_gecos[0] == '\0') OnesMissing |= 02;
    if (pwStruct->pw_passwd[0] == '\0') OnesMissing |= 04;
    return(OnesMissing);
}

static int GetNextOldPW()
{
    int R;

    for (;;) {
	R = ReadPWEntry(OldPWFile, &OldPW);
	if (R > -3) break;
    }
    if (R < 0) ActiveStreams &= ~OldPWActive;
    return(R);
}


static int GetNextNewPW()
{
    int R;

    for (;;) {
	R = ReadPWEntry(NewPWFile, &NewPW); 
	if (R > -3) break;
    }
    if (R < 0) ActiveStreams &= ~NewPWActive;
    return(R);
}

static void p1CycleOld()
{
    int R;

    R = GetNextOldPW();
    if (R < -1) exit (2);
}


static void p1CycleNew()
{
    int R;

    R = GetNextNewPW();
    if (R < -1) exit (2);
    ThisNewBoring = 1;
}

/* unneeded at the moment
static void TestPWReads()
{
    int R;

    while (feof(NewPWFile) == 0 && ferror(NewPWFile) == 0) {
	R = ReadPWEntry(NewPWFile, &NewPW);
	fprintf(stdout, "ReadPWEntry got %d; entries: %s; %s; %d; %d; %s; %s; %s.\n",
		R, NewPW.pw_name, NewPW.pw_passwd, NewPW.pw_uid, NewPW.pw_gid,
		NewPW.pw_gecos, NewPW.pw_dir, NewPW.pw_shell);
    }
}
*/


static int Quote(SrcPtr, DestPtr, MaxChars)
    char *SrcPtr, *DestPtr;
{
    register char C;

    if (*SrcPtr == '\0') { *DestPtr++ = '+'; *DestPtr++ = ' '; *DestPtr++ = '\0';}
    else do {C = *SrcPtr++;
	  if (C == '+') {*DestPtr++ = '+'; if (--MaxChars <= 0) return (-1);}
	  if (C == ':') {*DestPtr++ = '+'; C = '='; if (--MaxChars <= 0) return (-1);}
	  *DestPtr++ = C; if (--MaxChars < 0) return(-1);
	 } while (C != '\0');
    return(0);
}

static void DeQuote(SrcPtr, DestPtr)
    char *SrcPtr, *DestPtr;
{
    register char C;
    if (strcmp(SrcPtr, "+ ") == 0) {*DestPtr++ = '\0';}
    else do {C = *SrcPtr++;
	  if (C == '+') {
		C = *SrcPtr++;
		if (C == '=') C = ':';
	  }
	  *DestPtr++ = C;
	 } while (C != '\0');
}


static int ReadChgEntry()
{
    int Res;
    static char InBuf[5000];

    InChgEntry[0] = InChgField[0] = InChgOldVal[0] = InChgNewVal[0] =
		InChgStamp[0] = '\0';
    InChgStampValue = 0;
    QuoteBuff1[0] = QuoteBuff2[0] = '\0';

    for (;;) {
	if (fgets(InBuf, sizeof(InBuf), InChgFile) == NULL) {
	    Res = -1;
	    break;
	} else {
	    if (InBuf[0] == '#') {
		fputs(InBuf, OutChgFile);
		if (ferror(OutChgFile)) {
		    fprintf(stderr, "Error writing change file: %s.\n", UnixError(errno));
		    exit(3);
		}
		InChgCommentSeen = 1;
	    } else {
		Res = 0;
		break;
	    }
	}
    }

    if (Res == 0) {
	Res = sscanf(InBuf, "%50[^:\n]:%50[^:\n]:%2500[^:\n]:%2500[^:\n]:%200[0123456789]\n",
		InChgEntry, InChgField, QuoteBuff1, QuoteBuff2, InChgStamp);
    }
    if (Res < 0) {
	ActiveStreams &= ~InChgActive;
	if (feof(InChgFile) && ! ferror(InChgFile)) return(-1);
	fprintf(stderr, "Error reading change file ``%s'': %s.\n", InChgFileName, UnixError(errno));
	return(-2);
	}

    if (Res != 5) {
	ActiveStreams &= ~InChgActive;
	fprintf(stderr, "Bad format for change file ``%s''.  Expected five fields, got %d.\n", InChgFileName, Res);
	return(-3);
	}
    DeQuote(QuoteBuff1, InChgOldVal);
    DeQuote(QuoteBuff2, InChgNewVal);
    InChgStampValue = atoi(InChgStamp);
    return(0);
}

static void p1CycleChg()
{
    int R;
    R = ReadChgEntry();
    if (R >= -1) return;
    exit (2);
}


/* unneeded at the moment
static void TestChgReads()
{
    int R;

    while (feof(InChgFile) == 0 && ferror(InChgFile) == 0) {
	R = ReadChgEntry();
	fprintf(stdout, "ReadChgEntry got %d; entries: %s; %s; %s; %s; %s.\n",
		R, InChgEntry, InChgField, InChgOldVal, InChgNewVal, InChgStamp);
    }
}
*/

static void PrintChangeEntry(strm)
	FILE *strm;
{
/* Print the current change entry in nice format to stream strm..
 */
	fprintf(strm, "User %s, %s field", InChgEntry, InChgField);
	if (InChgStamp[0] != '\0') fprintf(strm, ", time %s", InChgStamp);
	fprintf(strm, "; ``%s'' ==> ``%s''.\n", InChgOldVal, InChgNewVal);
}


static void LoadDBFields()
{
/* Load the stuff from NewPW into the database fields.
 */
    int i;

    for (i=0; i <= FldMAX; i++) FieldContent[i][0] = '\0';

    strcpy(flID, NewPW.pw_name);	/* Unix login name becomes ID */
    strcpy(flPW, NewPW.pw_passwd);	/* Unix password becomes PW */
    sprintf(flNI, "%d", NewPW.pw_uid);	/* Unix login UID becomes NI */
    sprintf(flGI, "%d", NewPW.pw_gid);	/* Unix group ID becomes GI */
    strcpy(flN, NewPW.pw_gecos);	/* Famous GECOS field becomes N */
    strcpy(flHD, NewPW.pw_dir);	/* Login directory becomes HD */
    strcpy(flSh, NewPW.pw_shell);	/* ...and the shell remains the Sh. */

    HaveAnEntry = 1;
    for (i=0; i <= FldMAX; i++) FieldChanged[i] = 0;

    strcpy(flWN, flN);	/* WN gets pre-set from N. */
}

static void AddTokensFromName(DestPtr, NamePtr)
char *DestPtr, *NamePtr;
{
/* Add the new tokens in ``NamePtr'' string to the DestPtr set.
 */
    char *Src, C, *WholeSrc, *STok, *DTok, C1, C2;

	WholeSrc = NamePtr;
	for (;;) {
	   while ( (C = *WholeSrc) != 0 && wpTokenChar[C] == 0) WholeSrc++;
	   if (C == 0) break;
	   STok = WholeSrc;
	   DTok = DestPtr;
	   for (;;) {
		while ( (C1 = *DTok++) != 0 && wpTokenChar[C1] == 0) ;
		if (C1 == 0) {	/* not there--append to Tk */
		   DTok = DestPtr + strlen(DestPtr);
		   if (DestPtr[0] != '\0') *DTok++ = ' ';
		   while (wpTokenChar[C = *STok++]) *DTok++ = C;
		   *DTok++ = '\0';
		   break;
		} else {
		--DTok;
		Src = STok;
		for (;;) {
		   C1 = *Src;
		   C2 = *DTok;
		   if (! wpTokenChar[C1] && ! wpTokenChar[C2]) break;
		   if (ToLower(C1) != ToLower(C2)) break;
		   Src++;
		   DTok++;
		}
		if (! wpTokenChar[C1] && ! wpTokenChar[C2]) break;
		while (wpTokenChar[*DTok]) DTok++;
		}
	   }
	   while (wpTokenChar[*WholeSrc]) WholeSrc++;
	}
}

static void FillOutCurrentEntry(isNew, Skippable)
int isNew, Skippable;
{
/* There's an entry from the source files in the database fields.  isNew is true iff this is a new entry in the passwd file.  Skippable is true if this entry has probably not changed.  Fill in the derived fields for this entry, presuming that they haven't been changed explicitly.  If making a Grits file, write that entry.
 */
    int InName, FwdTries, GotField, ErrorCopy;
    char *Src, *Dst, *EndP, C;
    static char *PrevailingDomain = NULL;
    static char ErrorReason[500];
    wp_SearchToken Tok;
    int OutMatch, AnyFound;
    wp_PrimeKey KeyValue;
    char *NewFwd;
    struct stat FwdStat;
    static int VFLen = 0, CCPLen = 0, PrevDomLen = 0;
    static char TreeCell[125];

    if (FieldChanged[FldWN] == 0) {
	FieldContent[FldWN][0] = '\0';
    }

    if (VFLen == 0) VFLen = strlen(ViceFile);
    if (CCPLen == 0) CCPLen = strlen(CellCommonPrefix);
    if (PrevDomLen == 0) {
	PrevailingDomain = NULL;
	if (GetCellFromFileName(OutTreeDir, TreeCell, sizeof(TreeCell)) == 0) PrevailingDomain = TreeCell;
	if (PrevailingDomain == NULL || *PrevailingDomain == '\0') PrevailingDomain = WorkstationCell;
	if (PrevailingDomain == NULL || *PrevailingDomain == '\0') PrevailingDomain = ThisDomain;
	PrevDomLen = strlen(PrevailingDomain);
    }
    if (FieldChanged[FldAf] == 0) {	/* generate Af entry from HD */
	strcpy(FieldContent[FldAf], FieldContent[FldHD]);
	Src = Dst = FieldContent[FldAf];
	EndP = rindex(Src, '/');	/* flush user name from home directory */
	if (EndP != NULL) *EndP = '\0';
		/* Now get rid of uninformative prefixes. */
	if ((strncmp(Src, ViceFile, VFLen) == 0) && (Src[VFLen] =='/')) Src += VFLen;
	if (strncmp(Src, CellCommonPrefix, CCPLen) == 0) Src += CCPLen;
	if (*Src == '/') ++Src;
	if (ULstlmatch(Src, PrevailingDomain) && (Src[PrevDomLen] == '/'))
				Src += (PrevDomLen+1);
	EndP = index(Src, '/');
	if (ULstlmatch(Src, "usr") || ULstlmatch(Src, "user")) Src = (EndP != NULL ? EndP : "");
	if (*Src == '/') ++Src;
	while (C = *Src++) {
		if (C == '/') C = ' ';
		*Dst++ = C;
	}
	*Dst++ = '\0';
    }

    if (NewAcctsOnly && Skippable) return;

    if (ForgetForward) return;

#ifdef AMS_ENV
    if (FieldChanged[FldFwd] == 0) {	/* generate Fwd entry by looking for file */
	strcpy(InFwdFileName, FieldContent[FldHD]);
	strcat(InFwdFileName, "/");
	strcat(InFwdFileName, ForwardingName);

	strcpy(InBoxFileName, FieldContent[FldHD]);
	strcat(InBoxFileName, "/");
	strcat(InBoxFileName, MailboxName);

/* Look for ~/.forward file.  If it's there, use it; if not, look for home directory.  If that's not there, presume Vice is screwy and we can't tell.  If home dir is there, look again for the .forward file (in case Vice just came back up).  If we try several times and always find the home dir there and the forward file not there, presume that there is no forward file.  Once Vice error codes can tell us if Vice is down, all this will go away.  In fact, we'll believe the ENOENT code as telling us that the file isn't there; some other code will tell us that Vice is down or unavailable. */

	GotField = 0;
	for (FwdTries = 0; FwdTries < 3; FwdTries++) {
		InFwdFile = fopen(InFwdFileName, "r");
		if (InFwdFile != NULL) {
			OutMatch = fstat(fileno(InFwdFile), &FwdStat);
			if (OutMatch != 0) {
				OutMatch = errno;
				fclose(InFwdFile);
				InFwdFile = NULL;
				errno = OutMatch;
			}
		}
		if (InFwdFile != NULL) {
			if (((FwdStat.st_mode) & S_IFMT) != S_IFREG) {
				DebOut(
				    "***Forward file ``%s'' for user %s is mode %#o, which is not a regular file--ignoring it.\n",
				    InFwdFileName, FieldContent[FldID], FwdStat.st_mode);
				GotField = 0;	/* cancel the forwarding */
				fclose(InFwdFile);
				VenusFlush(InFwdFileName);
				break;
			}
			InName = fread(FieldContent[FldFwd], sizeof(char),
					LONGSTRINGSIZE, InFwdFile);
			if (InName >= LONGSTRINGSIZE
			  || FwdStat.st_size >= LONGSTRINGSIZE) {
				fprintf(stderr, "Forward info overflow for user %s.\n",
					FieldContent[FldID]);
				exit(2);
			}
			if (fclose(InFwdFile) != 0) {
				fprintf(stderr, "Error closing file ``%s'' after reading--%s.\n",
					InFwdFileName, UnixError(errno));
				exit(2);
			}
			VenusFlush(InFwdFileName);	/* ignore errors */
			InFwdFile = NULL;
			GotField = 1;
			FieldContent[FldFwd][InName] = '\0';
			Trim(FieldContent[FldFwd], " \b\t\n\v\f\r");
			break;
		} else if (vdown(errno)		/* Connection to Venus died */
			|| errno == EACCES) {	/* Can't read file for now */
			if (vdown(errno)) strcpy(ErrorReason, "Vice down");
			else sprintf(ErrorReason, "***%s", UnixError(errno));
			GotField = 3;	/* recoverable ``unk'' */
			break;
		} else if (errno != ENOENT) {
			DebOut("***Unexpected error reading file ``%s''--%s.\n",
				InFwdFileName, UnixError(errno));
			GotField = 2;	/* ``unk'' */
			break;
		} else {
			if (stat(FieldContent[FldHD], &StatBuff) != 0) { /* home dir gone */
				ErrorCopy = errno;
				if (ErrorCopy == EACCES) { /* Perm denied--try mailbox */
					if (stat(InBoxFileName, &StatBuff) == 0
					    || errno == EACCES) {
						DebOut(
						  "***Permission denied on home dir ``%s,'' but %s there or protected: no %s file.\n",
						   FieldContent[FldHD], MailboxName, ForwardingName);
						VenusFlush(InBoxFileName);
						break;	/* really no .forward file. */
					}
					DebOut("***Permission denied on home dir ``%s'' but no %s: leaving it unknown.\n",
						FieldContent[FldHD], MailboxName);

				} else if (vdown(ErrorCopy)) {
					DebOut("Vice down for dir ``%s''--%s.\n",
					    FieldContent[FldHD], UnixError(ErrorCopy));
				} else if (ErrorCopy != ENOENT) {
					DebOut(
					   "***Unexpected error stat'ing home dir ``%s''--%s.\n",
					   FieldContent[FldHD], UnixError(ErrorCopy));
				}
				GotField = 2;	/* ``unk'' */
				break;
			}
			/* if home there and .forward not, try again. */
		}				
	}
	if (GotField == 3) {	/* Try to find the old field contents */
		if (!WPOpen) {
			fprintf(stderr,
				"%s, need to use WP for user %s, but couldn't open it: %s\n",
				ErrorReason, FieldContent[FldID],
				wp_ErrorString(WPOpenErr));
			exit(2);
		}
		WPerr = wp_SetUp(FieldContent[FldID], LookupUIDOnly, &Tok);
		if (WPerr != wperr_NoError) {
			if (Debugging) DebOut(
			    "***%s, couldn't set up WP to look for fwd addr for %s: (%d) %s\n",
			    ErrorReason, FieldContent[FldID], WPerr, wp_ErrorString(WPerr));
		} else {
			WPerr = cwp_Lookup(cd, Tok, &OutMatch, MatchIDExact,
						&OutMatch, &KeyValue);
			(void) wp_DeAllocate(Tok);
			if (WPerr >= (wperr_BTreeBaseValue + bterr_FileSystemErrorBegin)
			 && WPerr <= (wperr_BTreeBaseValue + bterr_FileSystemErrorEnd)
			 && (tfail(WPerr -
				(wperr_BTreeBaseValue + bterr_FileSystemErrorEnd))
			      || (WPerr - (wperr_BTreeBaseValue + bterr_FileSystemErrorEnd)) == ENODEV)) {
			    fprintf(stderr,
			      "%s; need to use WP for user %s, but WP is inaccessible (%s): %s\n",
			      ErrorReason, FieldContent[FldID],
			      (vdown(WPerr -
				(wperr_BTreeBaseValue + bterr_FileSystemErrorBegin)) ?
					"Vice down" : "temp failure"),
			      wp_ErrorString(WPerr));
			    exit(2);
			} else if (WPerr != wperr_NoError) {
				if (Debugging) DebOut(
					"***%s, couldn't cwp_Lookup(%s): (%d) %s\n",
					ErrorReason, FieldContent[FldID],
					WPerr, wp_ErrorString(WPerr));
			} else {
				WPerr = cwp_Read(cd, KeyValue, FwdIx, &NewFwd);
				if (WPerr == wperr_NoError) {
				    if (strcmp(NewFwd, "**unknown**") != 0
					&& (strlen(NewFwd) < LONGSTRINGSIZE)) {
					strcpy(FieldContent[FldFwd], NewFwd);
					GotField = 1;
					if (Debugging) DebOut(
						"%s, found old forwarding address for user ``%s'' of ``%s''\n",
						ErrorReason,
						FieldContent[FldID], NewFwd);
				    }
				} else if (WPerr == wperr_NoSuchField) {
					GotField = 0;
					if (Debugging) DebOut(
						"%s, found null forwarding address for user ``%s''.\n",
						ErrorReason, FieldContent[FldID]);
				} else if (WPerr >=
				    (wperr_BTreeBaseValue + bterr_FileSystemErrorBegin)
				 && WPerr <=
				    (wperr_BTreeBaseValue + bterr_FileSystemErrorEnd) &&
					(tfail(WPerr -
				(wperr_BTreeBaseValue + bterr_FileSystemErrorEnd))
				      || (WPerr - (wperr_BTreeBaseValue + bterr_FileSystemErrorEnd)) == ENODEV)) {
				    fprintf(stderr,
				      "%s; need to use WP for user %s, but WP is inaccessible (%s): %s\n",
				      ErrorReason, FieldContent[FldID],
				      (vdown(WPerr -
				     (wperr_BTreeBaseValue + bterr_FileSystemErrorBegin)) ?
					"Vice down" : "temp failure"),
				      wp_ErrorString(WPerr));
				    exit(2);
				} else if (Debugging) DebOut(
					"***%s, couldn't cwp_Read the $Fwd for %s: (%d) %s\n",
					ErrorReason, FieldContent[FldID],
					WPerr, wp_ErrorString(WPerr));
			}
		}
	}
	if (GotField == 2 && isNew) GotField = 0;	/* Accounts initially don't forward mail */
	if (GotField == 0) strcpy(FieldContent[FldFwd], "");	/* no forwarding address */
	if (GotField == 2) strcpy(FieldContent[FldFwd], "**unknown**");
	if (GotField == 3) {
		strcpy(FieldContent[FldFwd], "**unknown**");
		if (Debugging) DebOut(
			"***%s, couldn't find or use old forwarding info for user ``%s''.\n",
			ErrorReason, FieldContent[FldID]);
	}
	if (GotField == 1) {
		AnyFound = 0;
		for (Src = FieldContent[FldFwd]; *Src != '\0'; ++Src) {
			if (isprint(*Src)) {AnyFound = 1; break;}
		}
		if (AnyFound == 0) {
			if (Debugging) DebOut(
		"***Clearing null forwarding address for user %s (%s of %s); was ``%s''.\n",
				FieldContent[FldID], FieldContent[FldN],
				FieldContent[FldAf], FieldContent[FldFwd]);
			strcpy(FieldContent[FldFwd], "");	/* null it out */
			GotField = 0;
		}
	}
	if (Debugging && GotField != 0) {
		DebOut("Forwarding entry for user %s, %s of %s, is ``%s''.\n",
			FieldContent[FldID], FieldContent[FldN],
			FieldContent[FldAf], FieldContent[FldFwd]);
	}
	VenusFlush(FieldContent[FldHD]);	/* flush home dir from cache, ignore errors */
    }
#endif /* AMS_ENV */
}

static char *MakePrimeKey(nam, NID)
char *nam; int NID;
{/* Make the canonical 8-character prime key for the given login name and NID.  Return a pointer to it in static storage. */
	static char Base64Chars[64] = {
		'0', '1', '2', '3', '4', '5', '6', '7',
		'8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
		'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N',
		'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',
		'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd',
		'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l',
		'm', 'n', 'o', 'p', 'q', 'r', 's', 't',
		'u', 'v', 'w', 'x', 'y', 'z', '+', '=' };
	static char OutBuf[PKLEN+1];
	register int Temp, Ctr;
	register char *Ptr;

#if (PKLEN != 8)
Implementation of MakePrimeKey needs to change!
#endif /* (PKLEN != 8) */
	strcpy(OutBuf, "    ");	/* four spaces and a NUL */
	Temp = strlen(nam);
	if (Temp > 4) Temp = 4;	/* only use four chars */
	strncpy(OutBuf, nam, Temp);	/* replace leftmost spaces */
	switch (ThisPass) {
    case 1:	LowerAll(OutBuf);
		break;
    case 2:	LowerAll(OutBuf);
		OutBuf[3] = 'A';		/* tag as from the additional source */
		break;
    default:	fprintf(stderr, "Bad value for ThisPass: %d (doing key %s/%d)\n",
			ThisPass, nam, NID);
		exit(2);
	}
	Temp = NID; Ptr = &OutBuf[7];
	for (Ctr = 3; Ctr >= 0; --Ctr) {
		*Ptr-- = Base64Chars[Temp & 077];
		Temp >>= 6;
	}
	OutBuf[PKLEN] = '\0';
	if (Debugging > 1) DebOut("MakePrimeKey(%s, %d), pass=%d: ``%s''\n",
				nam, NID, ThisPass, OutBuf);
	return OutBuf;
}

static int LookupFieldName(fldN)
char *fldN;
{
/* Find this program's index for the field name that fldN points to.
   Return -1 if it's not a match. */
    int flIx;

    for (flIx = 0; flIx <= FldMAX; flIx++)
	if (strcmp(fldN, wpFieldName[flIx]) == 0) return(flIx);
    return (-1);
}


/* Return the field index for the name at fldN, or halt. */
/* unneeded at the moment
static int NeedFieldIx (fldN)
char *fldN;
{
    int fldI;
    fldI = LookupFieldName (fldN);
    if (fldI >= 0) return(fldI);
    fprintf(stderr, "Field name ``%s'' not known to the program.\n", fldN);
    exit(2);
}
*/

static void BurstEntry(ContentArr, ent, entlen)
char *ContentArr[];
char *ent;
int entlen;
{/* Take an old B-tree prime value, pointed to by ent, and separate it into its component fields (in ContentArr). */
	int Fld;
	char *FPtr, *FSpan, *FEnd, *namStart, *nPtr;
	char FldNam[100];

	for (Fld = 0; Fld <= FldMAX; ++Fld) ContentArr[Fld][0] = '\0';	/* null 'em out */
	FPtr = ent; FEnd = ent + entlen;
	while (FPtr < FEnd) {	/* for each named field */
		FSpan = FPtr;
		for (;;) {	/* find end of this entry */
			FSpan = index(FSpan, '\n');
			if (FSpan >= FEnd) FSpan = NULL;
			if (FSpan == NULL) {FSpan = FEnd; break;}
			if (++FSpan >= FEnd) {FSpan = FEnd; break;}
			if (*FSpan == '$') break;	/* leave it pointing at '$' */
		}
		namStart = FPtr;
		if (*namStart == '$') ++namStart;
		nPtr = FldNam;
		while (*namStart != ' ' && namStart < FSpan) *nPtr++ = *namStart++;
		if (nPtr >= &FldNam[sizeof FldNam]) {
			fprintf(stderr, "FldNam overflow: %s\n", FPtr);
			exit(2);
		}
		*nPtr = '\0';
		Fld = LookupFieldName(FldNam);
		if (Fld < 0) {
			fprintf(stderr, "Unknown field: %s; ignoring it.\n", FldNam);
		} else {
			while (*namStart == ' ' && namStart < FSpan) ++namStart;
			if ((FSpan - namStart) >= FieldBufSizes[Fld]) {
				fprintf(stderr, "Buffer overflow with field %s\n", FldNam);
				exit(2);
			}
			strncpy(ContentArr[Fld], namStart, (FSpan - namStart));
			ContentArr[Fld][FSpan - namStart] = '\0';
			Trim(ContentArr[Fld], " \b\t\n\v\f\r");
		}
		FPtr = FSpan;	/* now look for the next field */
	}
	ContentArr[FldTk][0] = '\0';		/* Generate $Tk field from scratch */
	if (ContentArr[FldWN][0] != '\0')
		AddTokensFromName(ContentArr[FldTk], ContentArr[FldWN]);
	if (ContentArr[FldN][0] !='\0')
		AddTokensFromName(ContentArr[FldTk], ContentArr[FldN]);
	Trim(ContentArr[FldTk], " \b\t\n\v\f\r");
	if ((ContentArr[FldWN][0] != '\0') && (Debugging == 2)) DebOut(
			"Name ``%s'' and WN ``%s'' produced tokens ``%s''.\n",
			ContentArr[FldN], ContentArr[FldWN],
			ContentArr[FldTk]);
}

#define MAXTOKENS 30
static void AddToks(TkChar, FldPtr, Toks, NumToksP, OnlySurn)
char *TkChar, *FldPtr;
char Toks[MAXTOKENS][MEDIUMSTRINGSIZE];
int *NumToksP, OnlySurn;
{
	char *FldEnd;
	char *Last, *Prev, *LastEnd, *PrevEnd, *NStart, *NEnd;
	int ThisStart, WhichTok, IsSfx;

	PrevEnd = NULL;
	ThisStart = *NumToksP;
		/* find beginning of token */
	while (TkChar[*FldPtr] == '\0' && *FldPtr != '\0') ++FldPtr;
	FldEnd = FldPtr;
	for (; *FldPtr != '\0'; FldPtr = FldEnd) {
		/* find end of the token */
		while (TkChar[*FldEnd] != '\0' && *FldEnd != '\0') ++FldEnd;
		if (*NumToksP >= MAXTOKENS) {
			fprintf(stderr, "No more room for token %s\n", FldPtr);
			exit(2);
		}
		if ((FldEnd - FldPtr) >= MEDIUMSTRINGSIZE) {
			fprintf(stderr, "Token too big: %s\n", FldPtr);
			exit(2);
		}
		strncpy(&Toks[*NumToksP][0], FldPtr, FldEnd - FldPtr);
		Toks[*NumToksP][FldEnd - FldPtr] = '\0';
		++*NumToksP;
		while (TkChar[*FldEnd] == '\0'&& *FldEnd != '\0') ++FldEnd;
	}
	if (OnlySurn) {	/* We're only interested in the surname */
	    for (WhichTok = (*NumToksP - 1); WhichTok >= ThisStart; --WhichTok) {
		Last = LastEnd = Prev = NULL;
		NStart = Toks[WhichTok];
		while (wpTokenChar[*NStart] == '\0' && *NStart != '\0') ++NStart;
		NEnd = NStart;
		for (; *NStart != '\0'; NStart = NEnd) {
		    while (wpTokenChar[*NEnd] != '\0' && *NEnd != '\0') ++NEnd;
		    Prev = Last; PrevEnd = LastEnd;
		    Last = NStart; LastEnd = NEnd;		    
		    while (wpTokenChar[*NEnd] == '\0' && *NEnd != '\0') ++NEnd;
		}
		if (Last == NULL) {Toks[WhichTok][0] = '\0'; continue;}  /* nothing to do */
		IsSfx = TRUE;
		*LastEnd = '\0';
		if (Prev == NULL) IsSfx = FALSE;
		else if (*PrevEnd != ',') {
			if (ULstrcmp(Last, "Jr") != 0
			  && ULstrcmp(Last, "III") != 0
			  && ULstrcmp(Last, "IV") != 0
			  && ULstrcmp(Last, "II") != 0) IsSfx = FALSE;
		}
		if (IsSfx) {Last = Prev; *PrevEnd = '\0';}
		strcpy(Toks[WhichTok], Last);
	    }
	}
}

static int IsTempFail(bterr, oper, whr)
bt_ErrorCode bterr; char *oper, *whr;
{/* If the bterr value represented in bterr is a temporary condition, this routine will sleep for a while and return TRUE.  If it's not a temporary condition, it will return FALSE immediately.  oper and whr describe the operation that failed, for the sake of possible debugging output. */

	int UnixErr;
	if (bterr != bterr_OutOfMemory) {
		if (bterr < bterr_FileSystemErrorBegin
		    || bterr > bterr_FileSystemErrorEnd) return FALSE;
		UnixErr = bterr - bterr_FileSystemErrorBegin;
		switch (UnixErr) {
		case ENXIO:	return FALSE;	/* temp fail, but need reboot */

		case EINTR: case ENOMEM: case ENFILE: case ENOSPC:
		case ENOBUFS:	break;

/* This next case is because Venus will return EIO if you open a file for reading but the servers go down after you start reading the file (if it's letting you proceed after the open and block only when you go to read a part of the file that's not yet been read from the server). */
		case EIO:	break;

		default:	if (vdown(UnixErr)) break;
			return FALSE;
		}
	}
	if (Debugging) DebOut("Sleeping on temp failure in %s %s: %s.\n",
				oper, whr, bt_ErrorString(bterr));
	++TempSleeps;
	sleep(30);
	return TRUE;
}

static void EnsureKV(btix, ixkey, ixval)
int btix;
char *ixkey, *ixval;
{/* Ensure that the key/value pair ixkey/ixval is present in B-tree index btix. */
	static char TaggedKey[MEDIUMSTRINGSIZE + 10];
	bt_ErrorCode BTerr;
	char *OIxValue;
	int ValLen, ValDum;

	sprintf(TaggedKey, "%s%c%c", ixkey, KeyTagSep, BTIxTags[btix]);
	for (;;) {		/* until successful through the Vice failures */
		LowerAll(TaggedKey);
		BTerr = bt_Search(Curs, TaggedKey);
		if (BTerr != bterr_NoError) {
			if (IsTempFail(BTerr, "ekv: searching for", TaggedKey)) continue;
			fprintf(stderr, "Can't bt_Search for ``%s'': %s\n", TaggedKey,
				bt_ErrorString(BTerr));
			exit(2);
		}
		if (bt_GetCursorState(Curs) == AtKey) {	/* there's an old one */
			BTerr = bt_GetCursorValueLen(Curs, &ValLen);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr,
					"ekv: getting value-len for", TaggedKey)) continue;
				fprintf(stderr, "Can't get length of value for ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(2);
			}
			OIxValue = malloc(ValLen+1);
			if (OIxValue == NULL) {
				if (!IsTempFail(bterr_OutOfMemory, "ekv: getting space for", TaggedKey)) sleep(120);
				continue;
				/* fprintf(stderr, "Out of memory handling ``%s''/%d bytes\n",
					TaggedKey, ValLen);
				exit(3); */
			}
			BTerr = bt_GetCursorValueData(Curs, OIxValue, ValLen, &ValDum);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ekv: gettting data for", TaggedKey))
					{free(OIxValue); continue;}
				fprintf(stderr, "Can't get value for ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(2);
			}
			if (ValDum < ValLen) {
				fprintf(stderr, "Returned value for ``%s'' too short: %d/%d.\n",
					TaggedKey, ValDum, ValLen);
				exit(2);
			}
			OIxValue[ValLen] = '\0';
			/* Compare our value with what's there. */
			if (strcmp(OIxValue, ixval) == 0) {
				TrackBig(TaggedKey, ValLen);
				free(OIxValue);
				return;
			}
			TrackBig(TaggedKey, strlen(ixval));
			if (Debugging)
				DebOut("Replacing value for key ``%s''; old=%s, new=%s.\n",
					TaggedKey, OIxValue, ixval);
			BTerr = bt_CondReplace(Tree, TaggedKey, OIxValue, ValLen,
						ixval, strlen(ixval));
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ekv: replacing value for", TaggedKey))
					{free(OIxValue); continue;}
				fprintf(stderr, "Can't replace value for key ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(3);
			}
			free(OIxValue);
		} else {		/* must create one */
			TrackBig(TaggedKey, strlen(ixval));
			if (Debugging) DebOut("Inserting value for key ``%s'': %s.\n",
				TaggedKey, ixval);
			BTerr = bt_Insert(Tree, TaggedKey, ixval, strlen(ixval));
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ekv: inserting", TaggedKey)) continue;
				fprintf(stderr, "Can't bt_Insert ``%s''/``%s'': %s\n",
					TaggedKey, ixval, bt_ErrorString(BTerr));
				exit(3);
			}
		}
		break;	/* All done! */
	}
}

static void DeleteKV(btix, ixkey, ixval)
int btix;
char *ixkey, *ixval;
{/* Delete the key/value pair ixkey/ixval from the B-tree index btix. */
	static char TaggedKey[MEDIUMSTRINGSIZE + 10];
	bt_ErrorCode BTerr;

	sprintf(TaggedKey, "%s%c%c", ixkey, KeyTagSep, BTIxTags[btix]);
	LowerAll(TaggedKey);
	for (;;) {		/* until Vice doesn't cause a temp fail */
		BTerr = bt_Search(Curs, TaggedKey);
		if (BTerr != bterr_NoError) {
			if (IsTempFail(BTerr, "dkv: searching for", TaggedKey)) continue;
			fprintf(stderr, "Can't bt_Search for ``%s'': %s\n", TaggedKey,
				bt_ErrorString(BTerr));
			exit(2);
		}
		if (bt_GetCursorState(Curs) == AtKey) {	/* there's an old one */
			if (Debugging) DebOut("Deleting value for key ``%s'' (%s).\n",
					TaggedKey, ixval);
			BTerr = bt_CondDelete(Tree, TaggedKey, ixval, strlen(ixval));
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "dkv: deleting", TaggedKey)) continue;
				fprintf(stderr,
					"Can't delete value for key ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(3);
			}
		}
		break;
	}
}

static void EnsureKey(ixkey, tag, mainkey)
char *ixkey, tag, *mainkey;
{/* Ensure that mainkey is indexed by ixkey (tagged as ``tag''). */
	static char TaggedKey[MEDIUMSTRINGSIZE + 10];
	bt_ErrorCode BTerr;
	char *OIxValue, *NIxValue, *VPtr;
	int ValLen, ValDum, NewValLen;

	sprintf(TaggedKey, "%s%c%c", ixkey, KeyTagSep, tag);
	LowerAll(TaggedKey);
	for (;;) {		/* until temp Vice failures don't screw us */
		BTerr = bt_Search(Curs, TaggedKey);
		if (BTerr != bterr_NoError) {
			if (IsTempFail(BTerr, "ek: searching for", TaggedKey)) continue;
			fprintf(stderr, "Can't bt_Search for ``%s'': %s\n", TaggedKey,
				bt_ErrorString(BTerr));
			exit(2);
		}
		if (bt_GetCursorState(Curs) == AtKey) {	/* there's an old one */
			BTerr = bt_GetCursorValueLen(Curs, &ValLen);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ek: getting value-len for", TaggedKey))
					continue;
				fprintf(stderr, "Can't get length of value for ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(2);
			}
			OIxValue = malloc(ValLen);
			NIxValue = malloc(ValLen + PKLEN);    /* hold space for new key */
			if (OIxValue == NULL || NIxValue == NULL) {
				if (OIxValue != NULL) free(OIxValue);
				if (NIxValue != NULL) free(NIxValue);
				if (!IsTempFail(bterr_OutOfMemory, "ek: getting space for", TaggedKey)) sleep(120);
				continue;
				/* fprintf(stderr, "Out of memory handling ``%s''/%d bytes\n",
					TaggedKey, ValLen);
				exit(3); */
			}
			BTerr = bt_GetCursorValueData(Curs, OIxValue, ValLen, &ValDum);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ek: getting data for", TaggedKey))
					{free(OIxValue); free(NIxValue); continue;}
				fprintf(stderr, "Can't get value for ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(2);
			}
			if (ValDum < ValLen) {
				fprintf(stderr, "Returned value for ``%s'' too short: %d/%d.\n",
					TaggedKey, ValDum, ValLen);
				exit(2);
			}
			/* Now see if the prime key is already there. */
			for (VPtr = OIxValue; (VPtr - OIxValue) < ValLen; VPtr += PKLEN) {
			    if (strncmp(VPtr, mainkey, PKLEN) == 0) {	/* already there */
				TrackBig(TaggedKey, ValLen);
				free(OIxValue); free(NIxValue);
				return;
			    }
			}
			if (Debugging) DebOut("Appending PK ``%s'' to index for ``%s''.\n",
						mainkey, TaggedKey);
			strncpy(NIxValue, OIxValue, ValLen);	/* make new copy */
			strncpy(&NIxValue[ValLen], mainkey, PKLEN);
			NewValLen = ValLen + PKLEN;
			NewValLen = wpSortIxValue(NIxValue, NewValLen / PKLEN) * PKLEN;
			TrackBig(TaggedKey, NewValLen);
			BTerr = bt_CondReplace(Tree, TaggedKey, OIxValue, ValLen,
						NIxValue, NewValLen);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ek: replacing value of", TaggedKey))
					{free(OIxValue); free(NIxValue); continue;}
				fprintf(stderr, "Can't append PK ``%s'' to index ``%s'': %s\n",
					mainkey, TaggedKey, bt_ErrorString(BTerr));
				exit(3);
			}
			free(OIxValue); free(NIxValue);
		} else {		/* must create one */
			TrackBig(TaggedKey, PKLEN);
			if (Debugging) DebOut("PK ``%s'' creates index ``%s''.\n",
					mainkey, TaggedKey);
			BTerr = bt_Insert(Tree, TaggedKey, mainkey, PKLEN);
				/* only the first PKLEN characters (prime key length) of the key */
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ek: inserting", TaggedKey)) continue;
				fprintf(stderr, "Can't bt_Insert ``%s''/``%s'': %s\n",
					TaggedKey, mainkey, bt_ErrorString(BTerr));
				exit(3);
			}
		}
		break;	/* done the loop */
	}
}

static void RemoveKey(ixkey, tag, mainkey)
char *ixkey, tag, *mainkey;
{/* Remove any instances of mainkey from the index for ixkey with tag ``tag''. */
	static char TaggedKey[MEDIUMSTRINGSIZE + 10];
	bt_ErrorCode BTerr;
	char *OIxValue, *NIxValue, *VSrc, *VDest;
	int ValLen, ValDum, NewValLen;

	sprintf(TaggedKey, "%s%c%c", ixkey, KeyTagSep, tag);
	LowerAll(TaggedKey);
	for (;;) {		/* until Vice temp failures don't stop us */
		BTerr = bt_Search(Curs, TaggedKey);
		if (BTerr != bterr_NoError) {
			if (IsTempFail(BTerr, "rk: searching for", TaggedKey)) continue;
			fprintf(stderr, "Can't bt_Search for ``%s'': %s\n", TaggedKey,
				bt_ErrorString(BTerr));
			exit(2);
		}
		if (bt_GetCursorState(Curs) == AtKey) {	/* there's an old one */
			BTerr = bt_GetCursorValueLen(Curs, &ValLen);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "rk: getting value-len for", TaggedKey))
					continue;
				fprintf(stderr, "Can't get length of value for ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(2);
			}
			OIxValue = malloc(ValLen);
			NIxValue = malloc(ValLen);		/* hold space for new key */
			if (OIxValue == NULL || NIxValue == NULL) {
				if (OIxValue != NULL) free(OIxValue);
				if (NIxValue != NULL) free(NIxValue);
				if (!IsTempFail(bterr_OutOfMemory, "rk: getting space for", TaggedKey)) sleep(120);
				continue;
				/* fprintf(stderr, "Out of memory handling ``%s''/%d bytes\n",
					TaggedKey, ValLen);
				exit(3); */
			}
			BTerr = bt_GetCursorValueData(Curs, OIxValue, ValLen, &ValDum);
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "rk: getting value of", TaggedKey))
					{free(OIxValue); free(NIxValue); continue;}
				fprintf(stderr, "Can't get value for ``%s'': %s\n",
					TaggedKey, bt_ErrorString(BTerr));
				exit(2);
			}
			if (ValDum < ValLen) {
				fprintf(stderr, "Returned value for ``%s'' too short: %d/%d.\n",
					TaggedKey, ValDum, ValLen);
				exit(2);
			}
			/* Now see if the prime key is already there. */
			ValDum = 0;
			VDest = NIxValue;
			for (VSrc = OIxValue; (VSrc - OIxValue) < ValLen; VSrc += PKLEN) {
			    if (strncmp(VSrc, mainkey, PKLEN) != 0) {	/* not this one */
				strncpy(VDest, VSrc, PKLEN);
				VDest += PKLEN;
			    } else {				/* already there */
				ValDum = 1;
			    }
			}
			if (ValDum == 0) {		/* not there--we're done */
				TrackBig(TaggedKey, ValLen);
				free(OIxValue); free(NIxValue);
				return;
			}
			if (VDest == NIxValue) {	/* new index is empty */
				if (Debugging) DebOut("PK ``%s'': deleting index ``%s''.\n",
						mainkey, TaggedKey);
				BTerr = bt_CondDelete(Tree, TaggedKey, OIxValue, ValLen);
				free(OIxValue); free(NIxValue);
				if (BTerr != bterr_NoError) {
					if (IsTempFail(BTerr, "rk: deleting",
						TaggedKey)) continue;
					fprintf(stderr,
						"Can't delete index ``%s'', PK ``%s'': %s\n",
						TaggedKey, mainkey,
						bt_ErrorString(BTerr));
					exit(3);
				}
			} else {			/* remove element */
				if (Debugging)
					DebOut("Removing PK ``%s'' from index for ``%s''.\n",
						mainkey, TaggedKey);
				NewValLen = VDest - NIxValue;
				NewValLen = wpSortIxValue(NIxValue, NewValLen/PKLEN) * PKLEN;
				TrackBig(TaggedKey, NewValLen);
				BTerr = bt_CondReplace(Tree, TaggedKey, OIxValue, ValLen,
							NIxValue, NewValLen);
				if (BTerr != bterr_NoError) {
					if (IsTempFail(BTerr, "rk: replacing value of", TaggedKey))
						{free(OIxValue); free(NIxValue); continue;}
					fprintf(stderr,
						"Can't remove PK ``%s'' from index ``%s'': %s\n",
						mainkey, TaggedKey,
						bt_ErrorString(BTerr));
					exit(3);
				}
				free(OIxValue); free(NIxValue);
			}
		}
		break;
	}
}

static void UpdateIndices(MainKey)
char *MainKey;
{/* Given the entry in FieldContent and the old one in OldFieldContent, and the key in MainKey, make the index entries correspond to the main entry that's already in the B-tree. */
	int WhichIx, OTok, NTok, XTok;
	char *FldPtr;
	static char OldToks[MAXTOKENS][MEDIUMSTRINGSIZE];
	static char NewToks[MAXTOKENS][MEDIUMSTRINGSIZE];
	int NumOldToks, NumNewToks;

	for (WhichIx = 0; WhichIx < NumBTIndices; ++WhichIx) {	/* add all indices */
	    if (BTIndices[WhichIx] < 0) continue;
	    for (XTok = 0; XTok < MAXTOKENS; ++XTok) {
		OldToks[XTok][0] = '\0';
		NewToks[XTok][0] = '\0';
	    }
	    NumOldToks = NumNewToks = 0;
	    FldPtr = FieldContent[BTIndices[WhichIx]];
	    if (FldPtr != NULL && FldPtr[0] != '\0' && !BTInhibit[WhichIx])
		AddToks(BTTokTbls[WhichIx], FldPtr, NewToks, &NumNewToks, BTIsSurn[WhichIx]);
	    FldPtr = OldFieldContent[BTIndices[WhichIx]];
	    if (FldPtr != NULL && FldPtr[0] != '\0') {
		AddToks(BTTokTbls[WhichIx], FldPtr, OldToks, &NumOldToks, BTIsSurn[WhichIx]);
/* 		if (BTCheckOld[WhichIx])
		    AddToks(oldBTTokTbls[WhichIx], FldPtr, OldToks, &NumOldToks, BTIsSurn[WhichIx]); */
	    }
	    if (BTAuxIndices[WhichIx] >= 0) {
		FldPtr = FieldContent[BTAuxIndices[WhichIx]];
		if (FldPtr != NULL && FldPtr[0] != '\0' && !BTInhibit[WhichIx])
		    AddToks(BTTokTbls[WhichIx], FldPtr, NewToks, &NumNewToks, BTIsSurn[WhichIx]);
		FldPtr = OldFieldContent[BTAuxIndices[WhichIx]];
		if (FldPtr != NULL && FldPtr[0] != '\0') {
		    AddToks(BTTokTbls[WhichIx], FldPtr, OldToks, &NumOldToks, BTIsSurn[WhichIx]);
/* 		    if (BTCheckOld[WhichIx])
			AddToks(oldBTTokTbls[WhichIx], FldPtr, OldToks, &NumOldToks,
					BTIsSurn[WhichIx]); */
		}
	    }
	    if (Debugging > 1 && (NumOldToks > 0 || NumNewToks > 0)) {
		DebOut("Field %d (%s): %d old, %d new;\n", WhichIx,
			btIndexNames[WhichIx], NumOldToks, NumNewToks);
		DebOut("old:");
		for (OTok = 0; OTok < NumOldToks; ++OTok)
			fprintf(stdout, " ``%s''", &OldToks[OTok][0]);
		fprintf(stdout, "\n");
		DebOut("new:");
		for (NTok = 0; NTok < NumNewToks; ++NTok)
			fprintf(stdout, " ``%s''", &NewToks[NTok][0]);
		fprintf(stdout, "\n");
	    }
	    for (NTok = 0; NTok < NumNewToks; ++NTok) {	/* ensure each new one */
	      if (NewToks[NTok][0] != '\0') {
		for (OTok = 0; OTok < NumOldToks; ++OTok) {	/* and note old */
		    if (ULstrcmp(&OldToks[OTok][0], &NewToks[NTok][0]) == 0)
			OldToks[OTok][0] = '\0';
		}
		for (XTok = NTok+1; XTok < NumNewToks; ++XTok) {    /* flush dups */
		    if (ULstrcmp(&NewToks[XTok][0], &NewToks[NTok][0]) == 0)
			NewToks[XTok][0] = '\0';
		}
		EnsureKey(&NewToks[NTok][0], BTIxTags[WhichIx], MainKey);
	      }
	    }
	    for (OTok = 0; OTok < NumOldToks; ++OTok) {	/* remove obsolete ones */
	      if (OldToks[OTok][0] != '\0') {		/* if it's obsolete */
		RemoveKey(&OldToks[OTok][0], BTIxTags[WhichIx], MainKey);
	      }
	    }
	}
}

static void BuildCanonSoundEntries()
{/* Given the fields in FieldContent, build the FldD and FldX entries with their canonicalized-sound contents, suitable for storage and indexing. */
	int WhichIx, OTok, NTok, XTok;
	char *FldPtr, *Can;
	static char OrigToks[MAXTOKENS][MEDIUMSTRINGSIZE];
	static char *CanonToks[MAXTOKENS];
	int NumOrigToks, NumCanonToks;

	for (WhichIx = 0; WhichIx < NumBTIndices; ++WhichIx) {
	    if (BTFieldToWrite[WhichIx] < 0) continue;
	    for (XTok = 0; XTok < MAXTOKENS; ++XTok) {
		OrigToks[XTok][0] = '\0';
		CanonToks[XTok] = NULL;
	    }
	    NumOrigToks = NumCanonToks = 0;
	    FldPtr = FieldContent[BTFieldSource[WhichIx]];
	    if (FldPtr != NULL && FldPtr[0] != '\0')
		AddToks(BTFieldTokTbls[WhichIx], FldPtr, OrigToks, &NumOrigToks,
				BTWantSurn[WhichIx]);
	    if (BTFieldAltSource[WhichIx] >= 0) {
	      FldPtr = FieldContent[BTFieldAltSource[WhichIx]];
	      if (FldPtr != NULL && FldPtr[0] != '\0')
		AddToks(BTFieldTokTbls[WhichIx], FldPtr, OrigToks, &NumOrigToks,
				BTWantSurn[WhichIx]);
	    }
	    if (Debugging > 1 && NumOrigToks > 0) {
		DebOut("Building field %s, %d toks:",
			wpFieldName[BTFieldToWrite[WhichIx]], NumOrigToks);
		for (OTok = 0; OTok < NumOrigToks; ++OTok)
			fprintf(stdout, " ``%s''", &OrigToks[OTok][0]);
		fprintf(stdout, "\n");
	    }
	    for (OTok = 0; OTok < NumOrigToks; ++OTok) {
		if (BTWantSurn[WhichIx])
			Can = CanonSurn(&OrigToks[OTok][0]);
		else
			Can = CanonGiven(&OrigToks[OTok][0]);
		if (Can == NULL) {
			fprintf(stderr, "Out of memory in BuildCanonSoundEntries\n");
			exit(2);
		}
		for (NTok = 0; NTok < NumCanonToks; ++NTok) { /* flush dups */
			if (strcmp(Can, CanonToks[NTok]) == 0) {
				free(Can); Can = NULL;
				break;
			}
		}
		if (Can != NULL) {
			CanonToks[NumCanonToks] = Can;
			++NumCanonToks;
		}
	    }
	    FldPtr = FieldContent[BTFieldToWrite[WhichIx]];
	    FldPtr[0] = '\0';
	    for (NTok = 0; NTok < NumCanonToks; ++NTok) {
		if (FldPtr[0] != '\0') strcat(FldPtr, " ");
		strcat(FldPtr, CanonToks[NTok]);
		free(CanonToks[NTok]);
	    }
	    if (Debugging > 1) DebOut("Field %s: ``%s''\n",
			wpFieldName[BTFieldToWrite[WhichIx]], FldPtr);
	}
}

static void ValidateEntry(MainKey, Skippable, Deleting, SrcID)
char *MainKey; int Skippable, Deleting, SrcID;
{
/* Make the entry for MainKey either present or absent, according to Deleting.  If Deleting is false, construct a new entry from the FieldContent set (already filled out) and push that into the database, then update the indices.  Skippable is true if it's believed that this is probably an unchanged entry.  If Deleting is true, just update the indices and delete the main key when done.
 */
    int WhichFld, OldValueLen, EKVal, BitToUse, OldSameAsNew, ForceUpdate;
    bt_ErrorCode BTerr;
    static char MainBuf[4 * LONGSTRINGSIZE];
    char UserName[MEDIUMSTRINGSIZE];
    char *OldValue, *MBEnd;

    if (NewAcctsOnly && Skippable && (! Deleting)) return;

    FieldContent[FldTk][0] = '\0';		/* fill in the $Tk field from scratch */
    if (FieldContent[FldWN][0] != '\0')
	AddTokensFromName(FieldContent[FldTk], FieldContent[FldWN]);
    if (FieldContent[FldN][0] !='\0')
	AddTokensFromName(FieldContent[FldTk], FieldContent[FldN]);
    Trim(FieldContent[FldTk], " \b\t\n\v\f\r");
    if ((FieldContent[FldWN][0] != '\0') && (Debugging == 2)) DebOut(
		"Name ``%s'' and WN ``%s'' produced tokens ``%s''.\n",
		FieldContent[FldN], FieldContent[FldWN],
		FieldContent[FldTk]);
    BuildCanonSoundEntries();		/* create the $D and $X fields */

    for (;;) {	/* until Vice doesn't cause temp failures */
	    OldValue = NULL;		/* will serve as flag also */
	    EKVal = 0;
	    BTerr = bt_Search(Curs, MainKey);
	    if (BTerr != bterr_NoError) {
	    	if (IsTempFail(BTerr, "ve: searching for", MainKey)) continue;
		fprintf(stderr, "Error searching for key ``%s'': %s.\n",
			MainKey, bt_ErrorString(BTerr));
		exit(2);
	    }
	    BurstEntry(OldFieldContent, "", 0);	/* null out the OldFieldContent record */
	    if (bt_GetCursorState(Curs) == AtKey) {	/* one is there already */
		BTerr = bt_GetCursorValue(Curs, &OldValue, &OldValueLen);
		if (BTerr != bterr_NoError) {
		    if (IsTempFail(BTerr, "ve: getting value-len for", MainKey)) continue;
		    fprintf(stderr, "Cannot retrieve old value for key ``%s'', user %s: %s\n",
				MainKey, FieldContent[FldID], bt_ErrorString(BTerr));
		    exit(2);
		}
		BurstEntry(OldFieldContent, OldValue, OldValueLen);
			/* Now grab the old ``sources'' value */
		if (OldFieldContent[FldEK][0] != '\0') EKVal = atoi(OldFieldContent[FldEK]);
	    }
	/* Decide whether we insert, keep, replace, or remove this entry. */
	    BitToUse = 1 << (ThisPass - 1);	/* Bit that reflects presence or absence of this source */
	    if (Deleting) {
		EKVal &= ~BitToUse;
	    } else {	/* inserting */
		EKVal |= BitToUse;
	    }
	    sprintf(FieldContent[FldEK], "%d", EKVal);
	    if (FieldContent[FldID][0] != '\0')	/* for error reporting */
		strcpy(UserName, FieldContent[FldID]);
	    else
		strcpy(UserName, OldFieldContent[FldID]);
	    if (EKVal == 0) {	/* delete the sucker */
		for (WhichFld = 0; WhichFld <= FldMAX; WhichFld++)
			FieldContent[WhichFld][0] = '\0';
		HaveAnEntry = 0;
	    }

    /* Build the canonical character-stream representation */
	    MBEnd = MainBuf;
	    for (WhichFld = 0; WhichFld <= FldMAX; WhichFld++) {
	      if (WhichFld != FldTk) {
		if (FieldContent[WhichFld][0] != '\0') {
		    sprintf(MBEnd, "$%s %s\n",
				wpFieldName[WhichFld], FieldContent[WhichFld]);
		    MBEnd += strlen(MBEnd);		/* point to the NUL */
		    if (MBEnd >= &MainBuf[sizeof MainBuf]) {
			fprintf(stderr, "MainBuf overflow ensuring user %s!\n", UserName);
			exit(2);
		    }
		}
	      }
	    }

/* If inserting, insert PK first and then indices; if replacing or deleting, remove indices, then change or delete the PK.  This strategy makes the update idempotent, since the old index values are derived from the old PK. */
	    if (EKVal != 0) {	/* we want there to be a value there */
		if (OldValue == NULL) {	/* need to insert this one */
		    if (Debugging) DebOut("Inserting PK for user %s, key %s.\n",
				UserName, MainKey);
		    TrackBig(MainKey, MBEnd - MainBuf);
		    BTerr = bt_Insert(Tree, MainKey, MainBuf, MBEnd - MainBuf);
		    if (BTerr != bterr_NoError) {
		    	if (IsTempFail(BTerr, "ve: inserting", MainKey)) continue;
			fprintf(stderr, "Cannot insert new ``%s''/MainBuf for user %s: %s.\n",
				MainKey, UserName, bt_ErrorString(BTerr));
			exit(3);
		    }
		}
	    }
	/* Now there's some value there: the old one if it existed, the new one if it didn't. */
	    OldSameAsNew = 0;
	    if (OldValue != NULL && EKVal != 0)
	      if (OldValueLen == (MBEnd - MainBuf)
		&& (strncmp(OldValue, MainBuf, OldValueLen) == 0)) OldSameAsNew = 1;
	    ForceUpdate = 0;
	    if (OldSameAsNew == 0) ForceUpdate = 1;
	    else if (UpdatePeriod <= 0) ForceUpdate = 1;
	    else if ((SrcID % UpdatePeriod) == UpdateSlot) ForceUpdate = 1;
	    else if (Skippable == 0) ForceUpdate = 1;	/* eventually want an IsNew here */
	    else if (Debugging > 1) DebOut("Bypassing UpdateIndices(%s).\n", MainKey);

	    if (ForceUpdate) UpdateIndices(MainKey);	/* works in relation to the old value */

	    if (OldValue != NULL) {		/* something was there */
		if (EKVal != 0) {		/* want something to stay there; has value changed? */
		    if (OldValueLen != (MBEnd - MainBuf)
		      || (strncmp(OldValue, MainBuf, OldValueLen) != 0)) {	/* changed: replace */
			if (Debugging) DebOut("Replacing old PK for user %s, key %s.\n",
				UserName, MainKey);
			TrackBig(MainKey, MBEnd - MainBuf);
			BTerr = bt_CondReplace(Tree, MainKey, OldValue, OldValueLen,
								MainBuf, (MBEnd - MainBuf));
			if (BTerr != bterr_NoError) {
				if (IsTempFail(BTerr, "ve: replacing value for", MainKey))
					{free(OldValue); continue;}
				fprintf(stderr, "Cannot replace old value for key ``%s'', user %s: %s\n",
					MainKey, UserName, bt_ErrorString(BTerr));
				exit(3);
			}
		    }	/* else: not changed, leave it alone */
		} else {			/* EKVal == 0: don't want anything there; delete old. */
		    BTerr = bt_Search(Curs, MainKey);
		    if (BTerr != bterr_NoError) {
		    	if (IsTempFail(BTerr, "ve: searching to delete", MainKey))
				{free(OldValue); continue;}
			fprintf(stderr, "Error searching for key ``%s'' to delete it: %s.\n",
				MainKey, bt_ErrorString(BTerr));
			exit(2);
		    }
		    if (bt_GetCursorState(Curs) == AtKey) {	/* it's still there */
			if (Debugging) DebOut("Deleting old PK for user %s, key ``%s''.\n",
					UserName, MainKey);
			BTerr = bt_CondDelete(Tree, MainKey, OldValue, OldValueLen);
			if (BTerr != bterr_NoError) {
			    if (IsTempFail(BTerr, "ve: deleting", MainKey))
				{free(OldValue); continue;}
			    fprintf(stderr, "Cannot delete old PK for key ``%s'', user %s: %s\n",
				MainKey, OldPW.pw_name, bt_ErrorString(BTerr));
			    exit(3);
			}
		    }
		}
	    }
	    if (OldValue == NULL && EKVal == 0) {
		fprintf(stderr, "Strangeness in ValidateEntry: no old PK, don't want a new one, for key ``%s''.\n", MainKey);
	    }
	    if (OldValue != NULL) free(OldValue);
	    break;	/* finish the while-no-Vice-errors loop */
    }
}

static void EnsureCurrentEntry(isNew, Skippable)
{
/* There's an entry from the source file in the database fields; insert it into the output Tree.  isNew is true iff the entry is a new password entry.  Skippable is true iff this entry is probably unchanged from the previous version.  All the fields have been derived.
 */
    char MainKey[PKLEN+2+1];
    int Number;

    Number = atoi(FieldContent[FldNI]);
    sprintf(MainKey, "%s%c%c",
		MakePrimeKey(FieldContent[FldID], Number),
		KeyTagSep, KeyTagR);

    ValidateEntry(MainKey, (Skippable && (! isNew)), 0, Number);

    HaveAnEntry = 0;
}

static void DeleteCurrentEntry()
{
/* There's an entry from the source file in the OldPW; delete it from the output Tree.
 */
    int WhichFld;
    char MainKey[PKLEN+2+1];

    for (WhichFld = 0; WhichFld <= FldMAX; ++WhichFld)
	FieldContent[WhichFld][0] = '\0';	/* null new ptrs out */

    sprintf(MainKey, "%s%c%c",
		MakePrimeKey(OldPW.pw_name, OldPW.pw_uid),
		KeyTagSep, KeyTagR);

    ValidateEntry(MainKey, 0, 1, OldPW.pw_uid);

    HaveAnEntry = 0;
}

static void WriteChange(ID, WhatField, OldVal, NewVal, Stamp)
char *ID, *WhatField, *OldVal, *NewVal, *Stamp;
{
/* Write the change entry given by parameters out to the new change file.
 */
    if ((Debugging > 1) || (Debugging && InChgStampValue >= 100)) {
	DebOut("Recording change: User %s, %s field", ID, WhatField);
	if (Stamp[0] != '\0') fprintf(stdout, ", time %s", Stamp);
	fprintf(stdout, "; ``%s'' ==> ``%s''.\n", OldVal, NewVal);
    }
    fprintf(OutChgFile, "%s:%s:", ID, WhatField);
    if (Quote(OldVal, QuoteBuff1, LONGSTRINGSIZE) != 0) {
	fprintf(stderr, "QuoteBuff1 overflow writing change.\n");
	exit(3);
    }
    if (Quote(NewVal, QuoteBuff2, LONGSTRINGSIZE) != 0) {
	fprintf(stderr, "QuoteBuff2 overflow writing change.\n");
	exit(3);
    }
    fprintf(OutChgFile, "%s:%s:%s\n", QuoteBuff1, QuoteBuff2, Stamp);
    if (ferror(OutChgFile)) {
	fprintf(stderr, "Error writing change file: %s.\n", UnixError(errno));
	exit(3);
    }
}

static void WriteCurrentChange()
{ /* Copy current change entry to the output file. */

    WriteChange (InChgEntry, InChgField, InChgOldVal, InChgNewVal, InChgStamp);
}

static void ReplaceField (fldInd, UserPtr, ContPtr)
int fldInd; char *UserPtr, *ContPtr;
{
/* Replace field fldInd in the current Grits entry with contents ContPtr. */

	if (strlen(ContPtr) >= FieldBufSizes[fldInd]) {
		fprintf(stderr, "Buffer overflow writing %s field for user %s.\n",
			wpFieldName[fldInd], UserPtr);
		exit(2);
	}
	strcpy(FieldContent[fldInd], ContPtr);
	FieldChanged[fldInd] = 1;
}

static void p1HandleChg()
{
/* We have a change entry for a user who appears in neither old nor new PW files.  Let it expire; delete the unnecessary change entry by not copying it over.
 */
    if (Debugging) {
	DebOut("Deleting expired change: ");
	PrintChangeEntry(stdout);
    }
    p1CycleChg();
}

static void p1HandleNew()
{
/* We have a new PW entry that isn't in the old one and has no change entries either.
 */
	if (Debugging) DebOut("New PW entry for user %s.\n", NewPW.pw_name);
	FillOutCurrentEntry(1, 0);
	EnsureCurrentEntry(1, 0);
	p1CycleNew();
}

static void p1HandleChgNew()
{
/* An entry with a change but no old data.  Write and apply the change.
 */
    int flIx;

    flIx = LookupFieldName(InChgField);
    if (flIx < 0) {
	fprintf(stderr, "Field, %s, named in InChg file, not in database--ignoring.\n",
		InChgField);
    } else {
	/* Comparing InChgOldVal, InChgNewVal, and FieldContent[flIx] */
	WriteCurrentChange();
	if (strcmp(InChgOldVal, FieldContent[flIx]) == 0) {
		ReplaceField(flIx, InChgEntry, InChgNewVal);
		if (Debugging)
			DebOut("New user %s's %s field was %s--changed to %s.\n",
				InChgEntry, InChgField, InChgOldVal, InChgNewVal);
	}
	else if (strcmp(InChgNewVal, FieldContent[flIx]) == 0) {
		if (Debugging)
			DebOut("New user %s's %s field was already %s.\n",
				InChgEntry, InChgField, InChgNewVal);
	}
	else {
		fprintf(stderr, "Change on new user %s's %s field: existing (%s) is neither old (%s) nor new (%s).\n",
			InChgEntry, InChgField, FieldContent[flIx],
			InChgOldVal, InChgNewVal);
	}
	if (InChgStampValue >= 100) ThisNewBoring = 0;

    }
    p1CycleChg();
}

static void p1HandleOld()
{
/* The new PW file has nothing to say about this entry, and neither does the change log.  Let it go.
 */
	if (Debugging)
		DebOut("Old PW entry for user %s expired.\n", OldPW.pw_name);
	DeleteCurrentEntry();
	p1CycleOld();
}

static void p1HandleChgOld()
{
/* An entry that no longer appears in the new PW file.  Guess our changes were obsolete.
 */
    int flIx;

    flIx = LookupFieldName(InChgField);
    if (flIx < 0) {
	fprintf(stderr, "Expiring change, old user %s's %s field, refers to no field in database.\n",
			InChgEntry, InChgField);
    } else {
	if (OldPWFields[flIx] != NULL) {
		if (strcmp(InChgOldVal, OldPWFields[flIx]) != 0
		&& strcmp(InChgNewVal, OldPWFields[flIx]) != 0) {
			fprintf(stderr, "Bizarre expiring change to old user %s's %s field; PW value %s is neither old %s nor new %s.\n",
				InChgEntry, InChgField, OldPWFields[flIx],
				InChgOldVal, InChgNewVal);
		}
	}
    }
    if (Debugging) {
	DebOut("Change to entry in old but not new; letting it go: ");
		PrintChangeEntry(stdout);
    }
    p1CycleChg();
    /* p1CycleOld(); */
}

static void p1HandleOldNew()
{
/* No more changes to the given field.
 */
	int ReallyBoring;

	if (Debugging == 2)
		DebOut("Done with entry for user %s.\n", NewPW.pw_name);
	ReallyBoring = ThisNewBoring;
	if (ReallyBoring) {	/* Make sure we note changed entries, really. */
		if (strcmp(OldPW.pw_name, NewPW.pw_name) != 0) ReallyBoring = 0;
		if (strcmp(OldPW.pw_passwd, NewPW.pw_passwd) != 0) ReallyBoring = 0;
		if (strcmp(OldPW.pw_gecos, NewPW.pw_gecos) != 0) ReallyBoring = 0;
		if (strcmp(OldPW.pw_dir, NewPW.pw_dir) != 0) ReallyBoring = 0;
		if (strcmp(OldPW.pw_shell, NewPW.pw_shell) != 0) ReallyBoring = 0;
		if (OldPW.pw_uid != NewPW.pw_uid) ReallyBoring = 0;
		if (OldPW.pw_gid != NewPW.pw_gid) ReallyBoring = 0;
		if (Debugging && ReallyBoring == 0)
			DebOut("User %s wasn't so boring after all.\n", NewPW.pw_name);
	}
	FillOutCurrentEntry(0, ReallyBoring);
	EnsureCurrentEntry(0, ReallyBoring);
	p1CycleNew();
	p1CycleOld();
}

static void p1HandleChgOldNew()
{
/* The complex case.  Both old and new PW entries, and a change to apply to them.
	We handle here all four cases of two distinct values among the four values oldPW, newPW, oldVal, and newVal.
 */
    int flIx;

    flIx = LookupFieldName(InChgField);
    if (flIx < 0) {
	fprintf(stderr, "Field ``%s'' in change request for user %s does not exist!\n",
			InChgField, InChgEntry);
	WriteCurrentChange();
    } else {
	if (InChgStampValue >= 100) ThisNewBoring = 0;
	if (OldPWFields[flIx] == NULL) {	/* no useful OldPW fields--same as ChgNew */
		/* Comparing InChgOldVal, InChgNewVal, and FieldContent[flIx] */
		if (strcmp(InChgOldVal, "*") == 0 || strcmp(InChgOldVal, FieldContent[flIx]) == 0) {
			WriteCurrentChange();
			ReplaceField(flIx, InChgEntry, InChgNewVal);
			if ((Debugging > 1) || (Debugging && InChgStampValue >= 100))
				DebOut("User %s's %s field was %s, changed to %s.\n",
					InChgEntry, InChgField, InChgOldVal, InChgNewVal);
		}
		else if (strcmp(InChgNewVal, FieldContent[flIx]) == 0) {
			if (Debugging)
				DebOut("User %s's %s field was already %s--removing change.\n",
					InChgEntry, InChgField, InChgNewVal);
		}
		else {
			WriteCurrentChange();
			fprintf(stderr, "Weird change on user %s's %s field: existing (%s) is neither old (%s) nor new (%s).\n",
				InChgEntry, InChgField, FieldContent[flIx],
				InChgOldVal, InChgNewVal);
		}	
	}
	else {	/* there's a relevant OldPW field value in OldPWFields[flIx] */
		/* Comparing InChgOldVal, InChgNewVal, FieldContent[flIx],
				and OldPWFields[flIx]. */
		if (	strcmp(OldPWFields[flIx], FieldContent[flIx]) == 0
		   &&	strcmp(FieldContent[flIx], InChgNewVal) == 0) {
			/* New field has been incorporated into PW file; change obsolete.
				Leave field alone, remove change. */
			if (Debugging) {
				DebOut("Change incorporated, obsolete, and removed: ");
				PrintChangeEntry(stdout);
			}
		} else if (	strcmp(OldPWFields[flIx], InChgNewVal) == 0
		   &&	strcmp(FieldContent[flIx], InChgOldVal) == 0) {
			/* Some inconsistency: chg x to y, old is y, new is x.
				Possibly a reversion of the PW file.
				Handle by writing y (new val), keeping rule. */
			WriteCurrentChange();
			ReplaceField(flIx, InChgEntry, InChgNewVal);
			if (Debugging)
				DebOut("User %s's %s field had reverted to %s; restored to %s.\n",
					InChgEntry, InChgField, InChgOldVal, InChgNewVal);
		} else if (	strcmp(OldPWFields[flIx], FieldContent[flIx]) == 0
		   &&	(strcmp(InChgOldVal, "*") == 0 || strcmp(FieldContent[flIx], InChgOldVal) == 0)) {
			/* New value only recorded in change file.
				Write the new value, record the rule again. */
			WriteCurrentChange();
			ReplaceField(flIx, InChgEntry, InChgNewVal);
			if (Debugging)
				DebOut("Changing user %s's %s field from %s to %s.\n",
					InChgEntry, InChgField, InChgOldVal, InChgNewVal);
		} else if (	(strcmp(InChgOldVal, "*") == 0 || strcmp(OldPWFields[flIx], InChgOldVal) == 0)
		   &&	strcmp(FieldContent[flIx], InChgNewVal) == 0) {
			/* New value in new PW, old value in old.
				Write new value but don't obsolete the change quite yet. */
			WriteCurrentChange();
			if (Debugging) {
				DebOut("Change incorporated only in new PW file: ");
				PrintChangeEntry(stdout);
			}
		} else {
			WriteCurrentChange();
			fprintf(stderr, "Unmanageable change for user %s, %s field",
				InChgEntry, InChgField);
			if (InChgStamp[0] != '\0') fprintf(stderr, ", stamp %s", InChgStamp);
			fprintf(stderr, ";\n  Old PW entry:	``%s''\n", OldPWFields[flIx]);
			fprintf(stderr, "  New PW entry:	``%s''\n", FieldContent[flIx]);
			fprintf(stderr, "  Chg entry from:	``%s''\n", InChgOldVal);
			fprintf(stderr, "  Chg entry into:	``%s''\n", InChgNewVal);
		}
	}
    }
    p1CycleChg();
}

static void p1MergeActiveStreams()
{
    int CmpRes;

    while (1) {
	if (HaveAnEntry == 0 && ((ActiveStreams & NewPWActive) != 0)) LoadDBFields();
	switch (ActiveStreams) {
	case 0: return;

	case OldPWActive:
		p1HandleOld();
		break;
	case NewPWActive:
		p1HandleNew();
		break;
	case InChgActive:
		p1HandleChg();
		break;
	case OldPWActive | NewPWActive:
		CmpRes = strcmp(OldPW.pw_name, NewPW.pw_name);
		if (CmpRes < 0) p1HandleOld();
		else if (CmpRes == 0) {
			if (OldPW.pw_uid < NewPW.pw_uid) p1HandleOld();
			else if (OldPW.pw_uid == NewPW.pw_uid) p1HandleOldNew();
			else p1HandleNew();
		} else p1HandleNew();
		break;
	case InChgActive | OldPWActive:
		CmpRes = strcmp(InChgEntry, OldPW.pw_name);
		if (CmpRes < 0) p1HandleChg();
		else if (CmpRes == 0) p1HandleChgOld();
		else p1HandleOld();
		break;
	case InChgActive | NewPWActive:
		CmpRes = strcmp(InChgEntry, NewPW.pw_name);
		if (CmpRes < 0) p1HandleChg();
		else if (CmpRes == 0) p1HandleChgNew();
		else p1HandleNew();
		break;
	case InChgActive | OldPWActive | NewPWActive:
		CmpRes = strcmp(InChgEntry, NewPW.pw_name);
		if (CmpRes < 0) {
			CmpRes = strcmp(InChgEntry, OldPW.pw_name);
			if (CmpRes < 0) p1HandleChg();			/* chg < old & chg < new */
			else if (CmpRes == 0) p1HandleChgOld();	/* chg == old < new */
			else p1HandleOld();					/* old < chg < new */
		} else if (CmpRes > 0) {
			CmpRes = strcmp(OldPW.pw_name, NewPW.pw_name);
			if (CmpRes < 0) p1HandleOld();			/* old < new < chg */
			else if (CmpRes == 0) {				/* old == new < chg (but...) */
				if (OldPW.pw_uid < NewPW.pw_uid) p1HandleOld();
											/* oldQual < newQual */
				else if (OldPW.pw_uid == NewPW.pw_uid) p1HandleOldNew();
											/* oldQual == newQual */
				else p1HandleNew();				/* newQual < oldQual */
			} else p1HandleNew();				/* new < old & new < chg */
		} else {
			CmpRes = strcmp(InChgEntry, OldPW.pw_name);
			if (CmpRes < 0) p1HandleChgNew();		/* chg == new < old */
			else if (CmpRes == 0) {				/* chg == new == old (but...) */
				if (OldPW.pw_uid < NewPW.pw_uid) p1HandleChgOld();
											/* oldQual < newQual */
				else if (OldPW.pw_uid == NewPW.pw_uid) p1HandleChgOldNew();
											/* oldQual == newQual */
				else p1HandleChgNew();			/* newQual < oldQual */
			} else p1HandleOld();					/* old < chg == new */
		}
		break;
	default:
		fprintf(stderr, "*** Unknown value of ActiveStreams (%d); NewPW.pw_name is %s.\n",
				ActiveStreams, NewPW.pw_name);
		break;
	}
    }
}

/* Variables to describe the configured work */
static int cPass;
static char *cCurrFileName, *cPrevFileName;
static FILE *cCurrFile, *cPrevFile;
static enum EntryBoundaryTypes {bdy_null, bdy_line, bdy_grits} cBoundary;
static int cBTIndex; static char *cBTIxTag;

static int ReadGrits(AddFile)
FILE *AddFile;
{/* Read a single entry from the AddFile into the FieldContent arrays.  Return 1 if it got one, 0 at EOF. */
    int Chr;
    char *MBPtr, *MBEnd;
    static char MainBuf[4 * LONGSTRINGSIZE];

    Chr = '\0';
    while (Chr != EOF) {		/* for each entry in the AddFile */
	strcpy(MainBuf, "\n\n\n");
	MBEnd = MBPtr = &MainBuf[strlen(MainBuf)];
	do {
		Chr = getc(AddFile);
		if (Chr == EOF) break;
		*MBEnd++ = Chr;
	} while (strncmp(&MBEnd[-3], "\n$$", 3) != 0);
	/* Got the end-of-entry tag; canonicalize the entry. */
	if (Chr == EOF) --MBEnd; else MBEnd -= 3;
	for (;;) {
		if (index("\n\t \r", *MBEnd) == NULL) break;
		if (MBEnd <= MainBuf) break;
		--MBEnd;
	}
	for (;;) {
		if (index("\n\t \r$", *MBPtr) == NULL) break;
		if (*MBPtr == '$' && MBPtr[1] != '$') break;
		if (MBPtr >= MBEnd) break;
		++MBPtr;
	}
	if (MBEnd <= MBPtr) continue;	/* just a null entry */
	*++MBEnd = '\n'; *++MBEnd = '\0';
	/* OK, the entry begins at MBPtr and continues up to (not including) MBEnd. */
	if (Debugging == 2) DebOut("Grits entry: ``%s''.\n", MBPtr);

	BurstEntry(FieldContent, MBPtr, MBEnd - MBPtr);
	return 1;	/* Entry left in FieldContent */
    }
    return 0;	/* At end of file */
}

static void ProcessConfigGrits()
{/* Do any Grits files.  We basically have only one of these. */
	struct OldBuf {int SID; char PKey[PKLEN]; char InNew;} *Old;
	int NumOld, OldMax, NumNew;
	int Chr, SourceID, j, WasInOld;
	char ThisKey[PKLEN+2+1];

/* First, store away the keys and source-ID fields for the entries in the old file. */
	NumOld = 0; OldMax = 9;
	Old = (struct OldBuf *) malloc((OldMax + 1) * sizeof(struct OldBuf));
	if (Old == NULL) {
		fprintf(stderr, "Out of space for old Grits buffer\n");
		exit(2);
	}
	while (ReadGrits(cPrevFile) > 0) {
		if (NumOld >= OldMax) {
			SourceID = (NumOld * 2) + 2;
			Old = (struct OldBuf *) realloc(Old, (SourceID + 1) * sizeof(struct OldBuf));
			if (Old == NULL) {
				fprintf(stderr, "Out of space for old Grits buffer\n");
				exit(2);
			}
			OldMax = SourceID;
		}
		Chr = FieldContent[FldSI][0];
		if (Chr >= '0' && Chr <= '9')
			SourceID = atoi(FieldContent[FldSI]);
		else
			SourceID = atoi(FieldContent[FldNI]);
		Old[NumOld].SID = SourceID;
		strncpy(Old[NumOld].PKey,
			MakePrimeKey(FieldContent[FldID], SourceID),
			PKLEN);
		Old[NumOld].InNew = 0;
		++NumOld;
	}
	if (ferror(cPrevFile)) {
		fprintf(stderr, "Error reading old file %s: %s\n",
			cPrevFileName, UnixError(errno));
		exit(2);
	}
	if (Debugging) DebOut("%d keys in old file (%s).\n", NumOld, cPrevFileName);
/* Now read the new file and compare the keys and source IDs with the set of old ones. */
	NumNew = 0;
	while (ReadGrits(cCurrFile) > 0) {
		++NumNew;
		Chr = FieldContent[FldSI][0];
		if (Chr >= '0' && Chr <= '9')
			SourceID = atoi(FieldContent[FldSI]);
		else
			SourceID = atoi(FieldContent[FldNI]);
		sprintf(ThisKey, "%s%c%c",
			MakePrimeKey(FieldContent[FldID], SourceID),
			KeyTagSep, KeyTagR);
		WasInOld = FALSE;
		for (j = 0; j < NumOld; ++j) {
			if (Old[j].SID == SourceID
			    && strncmp(ThisKey, Old[j].PKey, PKLEN) == 0) {
				if (Old[j].InNew != 0) {
					fprintf(stderr,
						"Duplicate entry in Grits file %s!  Key=%s, SourceID=%d.\n",
						cCurrFileName, ThisKey, SourceID);
					exit(2);
				}
				Old[j].InNew = 1;
				WasInOld = TRUE;
			}
		}
		if (WasInOld) {
			if (Debugging > 1) DebOut("Ensuring addl entry with key ``%s''.\n",
				ThisKey);
		} else {
			if (Debugging) DebOut("Ensuring new addl entry with key ``%s''.\n",
				ThisKey);
		}
		ValidateEntry(ThisKey, WasInOld, 0, SourceID);
	}
	if (ferror(cCurrFile)) {
		fprintf(stderr, "Error reading new addl file %s: %s\n",
			cCurrFileName, UnixError(errno));
		exit(2);
	}
	if (Debugging) DebOut("%d keys in new file (%s).\n", NumNew, cCurrFileName);	
/* Having read both files, delete the entries that were in the old file but not the new one. */
	for (j = 0; j < NumOld; ++j) {
		if (Old[j].InNew == 0) {
			strncpy(ThisKey, Old[j].PKey, PKLEN);
			sprintf(&ThisKey[PKLEN], "%c%c", 	KeyTagSep, KeyTagR);
			if (Debugging) DebOut("Removing addl entry with key ``%s''.\n",
					ThisKey);
			FieldContent[FldID][0] = '\0';	/* No misleading msgs */
			OldFieldContent[FldID][0] = '\0';
			ValidateEntry(ThisKey, 0, 1, Old[j].SID);
		}
	}
}

static void ProcessConfigLine()
{
#define LBSIZ 1000
	char CurrBuff[LBSIZ+3];
	char PrevBuff[LBSIZ+3];
	char *CPtr;
	char *CurrV, *PrevV;
	int CmpRes;

	CurrV = PrevV = NULL;
	CurrBuff[0] = PrevBuff[0] = '\0';
	for (;;) {	/* Read lines of the current and previous files */
	    if (cCurrFile != NULL && CurrBuff[0] == '\0') {
		errno = 0;
		if (fgets(CurrBuff, LBSIZ, cCurrFile) == NULL) {
			if (ferror(cCurrFile)) {
				fprintf(stderr, "Error reading current file ``%s'': %s\n",
					cCurrFile, UnixError(errno));
				exit(2);
			}
			fclose(cCurrFile);
			cCurrFile = NULL;
		} else {		/* condition data in CurrBuff */
			CurrBuff[LBSIZ] = CurrBuff[LBSIZ+1] = CurrBuff[LBSIZ+2] = '\0';
			CPtr = index(CurrBuff, '\n');
			if (CPtr != NULL) *CPtr = '\0';
			else {
				fprintf(stderr,
				"CurrBuff too small for input file: increase LBSIZ and recompile.\n");
				exit(2);
			}
			CurrV = CurrBuff;
			while (*CurrV != '\0' && index(" \t", *CurrV) == NULL) ++CurrV;
			*CurrV++ = '\0';	/* terminate left argument; move to right argument */
			while (*CurrV != '\0' && index(" \t", *CurrV) != NULL) ++CurrV;
			LowerAll(CurrV);
		    }
	    }
	    if (cPrevFile != NULL && PrevBuff[0] == '\0') {
		errno = 0;
		if (fgets(PrevBuff, LBSIZ, cPrevFile) == NULL) {
			if (ferror(cPrevFile)) {
				fprintf(stderr, "Error reading previous file ``%s'': %s\n",
					cPrevFile, UnixError(errno));
				exit(2);
			}
			fclose(cPrevFile);
			cPrevFile = NULL;
		} else {		/* condition data in PrevBuff */
			PrevBuff[LBSIZ] = PrevBuff[LBSIZ+1] = PrevBuff[LBSIZ+2] = '\0';
			CPtr = index(PrevBuff, '\n');
			if (CPtr != NULL) *CPtr = '\0';
			else {
				fprintf(stderr,
				"PrevBuff too small for input file: increase LBSIZ and recompile.\n");
				exit(2);
			}
			PrevV = PrevBuff;
			while (*PrevV != '\0' && index(" \t", *PrevV) == NULL) ++PrevV;
			*PrevV++ = '\0';	/* terminate left argument; move to right argument */
			while (*PrevV != '\0' && index(" \t", *PrevV) != NULL) ++PrevV;
			LowerAll(PrevV);
	  	  }
	    }
	    if (cCurrFile == NULL && cPrevFile == NULL) return;

					/* Compare the two values and take action */
	    if (cCurrFile == NULL) CmpRes = 1;
	    else if (cPrevFile == NULL) CmpRes = -1;
	    else CmpRes = ULstrcmp(CurrBuff, PrevBuff);
	    if (CmpRes <= 0)
		EnsureKV(cBTIndex, CurrBuff, CurrV);
	    else
		DeleteKV(cBTIndex, PrevBuff, PrevV);
					/* Decide which entry(ies) to consume */
	    if (CmpRes <= 0) CurrBuff[0] = '\0';	/* advance cCurrFile */
	    if (CmpRes >= 0) PrevBuff[0] = '\0';	/* advance cPrevFile */
	}
}

static void DoConfiguration()
{
#define IBSIZ 1000
	static char InBuf[IBSIZ+2];
	char *CPtr, *NextPtr;
	int OldPass;

	for (;;) {	/* Read lines of the config file */
	    errno = 0;
	    if (fgets(InBuf, IBSIZ, ConfFile) == NULL) {
		if (ferror(ConfFile)) {
			fprintf(stderr, "Error reading config file ``%s'': %s\n",
				ConfFileName, UnixError(errno));
			exit(2);
		}
		return;
	    }
	    InBuf[IBSIZ] = InBuf[IBSIZ+1] = '\0';	/* ensure terminated twice */
	    CPtr = index(InBuf, '\n');
	    if (CPtr != NULL) *CPtr = '\0';
	    else {
		fprintf(stderr, "InBuf too small for config file; increase IBSIZ and recompile.\n");
		exit(1);
	    }
			/* Now parse the input line */
	    cPass = cBTIndex = -1; cCurrFileName = cPrevFileName = NULL;
	    cBTIxTag = NULL;
	    cCurrFile = cPrevFile = NULL; cBoundary = bdy_null;
	    CPtr = InBuf;
	    do {
		while (*CPtr != '\0' && index(" \t", *CPtr) != NULL) ++CPtr;
		NextPtr = CPtr;
		while (*NextPtr != '\0' && index(" \t", *NextPtr) == NULL) ++NextPtr;
		*NextPtr++ = '\0';	/* field termination */
		if (ULstlmatch(CPtr, "pass=")) cPass = atoi(CPtr+5);
		else if (ULstlmatch(CPtr, "curr=")) cCurrFileName = NewString(CPtr+5);
		else if (ULstlmatch(CPtr, "prev=")) cPrevFileName = NewString(CPtr+5);
		else if (ULstlmatch(CPtr, "entry=")) {
			CPtr += 6;
			if (ULstrcmp(CPtr, "line") == 0) cBoundary = bdy_line;
			else if (ULstrcmp(CPtr, "grits") == 0) cBoundary = bdy_grits;
			else {
				fprintf(stderr,
				    "Config file ``%s'' has bad entry-delim spec ``%s''.\n",
				    ConfFileName, CPtr);
				exit(2);
			}
		} else if (ULstlmatch(CPtr, "btix=")) cBTIndex = atoi(CPtr+5);
		else if (ULstlmatch(CPtr, "bttag=")) cBTIxTag = NewString(CPtr+6);
		else {
			fprintf(stderr, "Config file ``%s'' has bad field name: ``%s''\n",
					ConfFileName, CPtr);
			exit(2);
		}
		CPtr = NextPtr;
		while (*CPtr != '\0' && index(" \t", *CPtr) != NULL) ++CPtr;
	    } while (*CPtr != '\0');

	    if (cPass < 0 || cPass > MAXPasses) {
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Unreasonable pass number: %d\n", cPass);
		exit(2);
	    }
	    if (ConfigurablePassRequirements[cPass] < 0) {
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Unconfigurable pass number: %d\n", cPass);
		exit(2);
	    }
	    if (ConfigurablePassRequirements[cPass] > 0) {
	      if (cBTIndex < 0 || cBTIndex >= NumBTIndices) {
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Unknown BT index value: %d\n", cBTIndex);
		exit(2);
	      }
	      if (ConfigurableBTIndex[cBTIndex] == 0) {
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Unconfigurable BT index value: %d\n", cBTIndex);
		exit(2);
	      }
	      if (cBTIxTag == NULL) {
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Missing ``bttag'' field.\n");
		exit(2);
	      }
	      if (strlen(cBTIxTag) != 1 || *cBTIxTag != BTIxTags[cBTIndex]){
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "BT tag mismatch; index %d is tag ``%c'', not ``%s''.\n",
				cBTIndex, BTIxTags[cBTIndex], cBTIxTag);
		exit(2);
	      }
	    }
	    if (cCurrFileName == NULL || cPrevFileName == NULL) {
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Missing current or previous file name.\n");
		exit(2);
	    }
	    cCurrFile = fopen(cCurrFileName, "r");
	    if (cCurrFile == NULL) {
		fprintf(stderr, "Can't open configured current file ``%s'': %s\n",
			cCurrFileName, UnixError(errno));
		exit(2);
	    }
	    cPrevFile = fopen(cPrevFileName, "r");
	    if (cPrevFile == NULL) {
		fprintf(stderr, "Can't open configured previous file ``%s'': %s\n",
			cPrevFileName, UnixError(errno));
		exit(2);
	    }
	    if (GiveStatus) {
		printf("\nPass %d, based on configuration file ``%s'':\n", cPass, ConfFileName);
		if (cBTIndex >= 0) printf("\tWriting BT index %d, whose tag is ``%c''.\n",
				cBTIndex, BTIxTags[cBTIndex]);

		if (fstat(fileno(cCurrFile), &StatBuff) == 0) printf(
		"\tCurrent file, ``%s'', written by UID %d with length %d on %s.\n",
		cCurrFileName, StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));

		if (fstat(fileno(cPrevFile), &StatBuff) == 0) printf(
		"\tPrevious file, ``%s'', written by UID %d with length %d on %s.\n",
		cPrevFileName, StatBuff.st_uid, StatBuff.st_size, NiceTime(StatBuff.st_mtime));

		printf("\n");
	    }

	    OldPass = ThisPass;
	    ThisPass = cPass;
	    switch (cBoundary) {
	    case bdy_line:
		ProcessConfigLine();
		break;
	    case bdy_grits:
		ProcessConfigGrits();
		break;
	    default:
		fprintf(stderr, "Error in config file ``%s'': ", ConfFileName);
		fprintf(stderr, "Entry types other than line or grits are not yet implemented\n");
		exit(2);
	    }
	    ThisPass = OldPass;

	    if (cCurrFile != NULL) {fclose(cCurrFile); cCurrFile = NULL;}
	    if (cPrevFile != NULL) {fclose(cPrevFile); cPrevFile = NULL;}
	    if (cCurrFileName != NULL) {free(cCurrFileName); cCurrFileName = NULL;}
	    if (cPrevFileName != NULL) {free(cPrevFileName); cPrevFileName = NULL;}
	    if (cBTIxTag != NULL) {free(cBTIxTag); cBTIxTag = NULL;}
	}
}

static void DoPass1()
{
    OpenPass1Files();

    p1CycleNew();
    p1CycleOld();
    p1CycleChg();

    p1MergeActiveStreams();

    ClosePass1Files();
}

main(argc, argv)
    int	argc;
    char	**argv;
{
#ifdef AMS_ENV
    CheckAMSConfiguration();
#else /* AMS_ENV */
    CheckServiceConfiguration();
#endif /* AMS_ENV */
    ParseArguments(argc, argv);

    OpenGlobalFiles();

    if (ConfFile != NULL) DoConfiguration();

    ThisPass = 1;
    DoPass1();

    CloseGlobalFiles();

#ifdef PLUMBFDLEAKS
    fdplumb_SpillGuts();
#endif /* PLUMBFDLEAKS */

    exit(0);
}
