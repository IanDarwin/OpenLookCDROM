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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wputil/RCS/genaccts.c,v 5.19 1992/12/15 21:12:21 rr2b R6tape $";
#endif

/* ************************************************************ *\
	genaccts.c
	Generate a copy of our names database for use at CS.
\* ************************************************************ */

#include <andrewos.h>
#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>
#include <truth.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <pwd.h>
#include <svcconf.h>

extern int errno;

#include <bt.h>
#include <wp.h>

main (argc, argv)
int	argc;
char	**argv;
{
    int EKValue;
    wp_ErrorCode RetVal;
    char *FldVal, *FldNext;
    wp_PrimeKey KeyValue = NULL;
    wp_FieldIndex idxID, idxN, idxWN, idxFwd, idxEK;
    char IDCopy[100], FwdCopy[10000];
    char *Src, *Dst;
    char *CellName;
    struct wp_cd *CD;
    int AccRes, ForceList, AffilFmt, Dum;
    static char Usage[] = "[-a] [-f] [cellname]";

    CheckServiceConfiguration();
    AffilFmt = ForceList = AccRes = 0;
    CellName = NULL;
    for (Dum = 1; Dum < argc; ++Dum) {
	if (argv[Dum][0] == '-') {
	    if (strcmp(argv[Dum], "-a") == 0) AffilFmt = 1;
	    else if (strcmp(argv[Dum], "-f") == 0) ForceList = 1;
	    else {
		fprintf(stderr, "No such option ``%s'';\nusage: %s %s\n", argv[Dum], argv[0], Usage);
		exit(2);
	    }
	} else {
	    if (CellName != NULL) {
		fprintf(stderr, "Extra argument ``%s'';\nusage: %s %s\n", argv[Dum], argv[0], Usage);
		exit(2);
	    }
	    CellName = argv[Dum];
	}
    }
    if (CellName == NULL || CellName[0] == '\0') CellName = WorkstationCell;
    if (CellName == NULL || CellName[0] == '\0') CellName = ThisDomain;
    if (AffilFmt == 0) {
	errno = 0;
#ifdef AMS_ENV
	AccRes = CheckAMSDelivery(CellName);
	if (AccRes <= 0) AccRes = CheckAMSUseridPlusWorks(CellName);
#else /* AMS_ENV */
	AccRes = 0;
#endif /* AMS_ENV */
	if (AccRes == 0) {
	    fprintf(stderr, "Can't determine whether cell %s runs AMDS, the AMS delivery system: %s\n",
		CellName, UnixError(errno));
	    if (ForceList == 0) exit(2);
	} else if (AccRes < 0) {
	    fprintf(stderr, "Cell %s does not run AMDS, the AMS Delivery System: %s\n", CellName, UnixError(errno));
	    if (ForceList == 0) exit(1);
	}
    }

    RetVal = wp_InitializeCell(CellName, &CD);
    if (RetVal != wperr_NoError) {
	fprintf(stderr, "wp_InitializeCell(%s) failed: %d, %s\n", CellName, RetVal, wp_ErrorString(RetVal));
	exit(1);
    }

    idxID =	wp_FieldNameToIndex("ID");
    idxN =	wp_FieldNameToIndex("N");
    idxWN =	wp_FieldNameToIndex("WN");
    idxFwd =	wp_FieldNameToIndex("Fwd");
    idxEK =	wp_FieldNameToIndex("EK");
    if (idxID < 0 || idxN < 0 || idxWN < 0 || idxFwd < 0 || idxEK < 0) {
	fprintf(stderr, "Cannot find a necessary WP index value.\n");
	exit(2);
    }

    for (;;) {
	RetVal = cwp_Generate(CD, &KeyValue);
	if (RetVal != wperr_NoError) {
	    fprintf(stderr, "wp_Generate() returns %d: %s\n",
		    RetVal, wp_ErrorString(RetVal));
	    exit(1);
	}
	if (KeyValue == NULL) break;
	RetVal = cwp_Read(CD, KeyValue, idxEK, &FldVal);
	if (RetVal != wperr_NoError) {
	    fprintf(stderr, "wp_Read(%s, idxEK): %s\n", KeyValue, wp_ErrorString(RetVal));
	    exit(1);
	}
	EKValue = atoi(FldVal);
	if ((EKValue & 3) == 0) continue;

	RetVal = cwp_Read(CD, KeyValue, idxID, &FldVal);
	if (RetVal == wperr_NoSuchField && ((EKValue & 1) == 0)) continue;
	if (RetVal != wperr_NoError) {
	    fprintf(stderr, "wp_Read(%s, idxID): %s\n", KeyValue, wp_ErrorString(RetVal));
	    exit(1);
	}
	strcpy(IDCopy, FldVal);

	if (AffilFmt == 0) {
	    RetVal = cwp_Read(CD, KeyValue, idxFwd, &FldVal);
	    switch (RetVal) {
		case wperr_NoError:
		    strcpy(FwdCopy, FldVal);
		    for (Src = FwdCopy; *Src != '\0'; ++Src) {
			if (*Src == '\n' || *Src == '\r' || *Src == '\t' || *Src == '\f') *Src = ' ';
		    }
		    for (Src = FwdCopy; *Src != '\0'; ++Src) {
			if (strncmp(Src, "  ", 2) == 0) {
			    for (Dst = Src; *Dst != '\0'; ++Dst) Dst[0] = Dst[1];
			}
		    }
		    Dst = &FwdCopy[strlen(FwdCopy) - 1];
		    while (Dst > FwdCopy && *Dst == ' ') *Dst-- = '\0';
		    if (Dst > FwdCopy) break;	/* else fall through: */
		case wperr_NoSuchField:
		    sprintf(FwdCopy, "%s%s@%s", IDCopy, AccRes > 0 ? "+" : "", CellName);
		    for (FldNext = FwdCopy; *FldNext != '\0'; ++FldNext) {
			if (*FldNext == '\n') *FldNext = ' ';
		    }
		    break;
		default:
		    fprintf(stderr, "wp_Read(%s, idxFwd): %s\n",
			    KeyValue, wp_ErrorString(RetVal));
		    exit(1);
	    }
	}

	RetVal = cwp_Read(CD, KeyValue, idxN, &FldVal);
	if (RetVal == wperr_NoSuchField) continue;
	if (RetVal != wperr_NoError) {
	    fprintf(stderr, "wp_Read(%s, idxN): %s\n", KeyValue, wp_ErrorString(RetVal));
	    exit(1);
	}
	if (AffilFmt == 0) {
	    printf("%s:%s:%s\n", IDCopy, FldVal, FwdCopy);
	} else {
	    FwdCopy[0] = '\0';
	    LCappend(FwdCopy, FldVal);
	    printf("%s:1:%s\n", FwdCopy, IDCopy);
	}

	RetVal = cwp_Read(CD, KeyValue, idxWN, &FldVal);
	if (RetVal != wperr_NoSuchField && RetVal != wperr_NoError) {
	    fprintf(stderr, "wp_Read(%s, idxWN): %s\n", KeyValue, wp_ErrorString(RetVal));
	    exit(1);
	}
	if (RetVal == wperr_NoError) {
	    for (; FldVal != NULL; FldVal = FldNext) {
		FldNext = strchr(FldVal, ';');
		if (FldNext != NULL) *FldNext++ = '\0';
		if (AffilFmt == 0) {
		    printf("%s:%s:%s\n", IDCopy, FldVal, FwdCopy);
		} else {
		    FwdCopy[0] = '\0';
		    LCappend(FwdCopy, FldVal);
                    printf("%s:1:%s\n", FwdCopy, IDCopy);
                }
                if (FldNext != NULL) {
                    while (*FldNext == ' ' || *FldNext == '\t') ++FldNext;
                    if (*FldNext == '\0') FldNext = NULL;
 		}
	    }
	}
    }

    if (cwp_Terminate(CD) != wperr_NoError) exit(6);
    exit(0);
}
