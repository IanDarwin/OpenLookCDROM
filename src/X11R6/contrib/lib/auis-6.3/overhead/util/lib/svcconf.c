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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/svcconf.c,v 2.20 1993/05/04 00:53:32 susan Exp $";
#endif

/* ************************************************************ *\
	svcconf.c
	Definitions for cell-based services.
\* ************************************************************ */


 

#include <andyenv.h>
#include <stdio.h>
#include <sys/param.h>
#include <andrewos.h>		/* sys/file.h */
#include <errno.h>
#include <ctype.h>

#include <util.h>
#include <svcconf.h>

#ifndef _IBMR2
extern char *malloc();
#endif /* _IBMR2 */

extern int errno;

/* The name of a file guaranteed to be in AFS */
char *ViceFile = "*";

/* Name of this domain and its length in characters */
static char DefaultThisDomain[] = "^";

char *ThisDomain = DefaultThisDomain;
int ThisDomainLen = sizeof(DefaultThisDomain)-1;   /* Don't include '\0' */

/* A run-time Boolean to tell us if we're not on Vice */
#ifdef AFS_ENV
int AMS_ViceIsRunning = 1;
#else /* AFS_ENV */
int AMS_ViceIsRunning = 0;
#endif /* AFS_ENV */

/* A run-time Boolean to tell us if we have AMS delivery (as opposed to sendmail) */
#ifdef RUN_AMDS_ENV
int AMS_DeliverySystem = 1;
#else /* RUN_AMDS_ENV */
int AMS_DeliverySystem = 0;
#endif /* RUN_AMDS_ENV */

/* A run-time Boolean to tell us if we should do White Pages lookups */
#ifdef WHITEPAGES_ENV
int AMS_UseWP = 1;
#else /* WHITEPAGES_ENV */
int AMS_UseWP = 0;
#endif /* WHITEPAGES_ENV */

/* Run-time Boolean saying whether we're running on AIX. */
int AMS_OnAIX = 0;

/* Run-time Boolean saying whether this AFS client (``workstation'') has its own mail system, as distinguished from the mail system of the AFS to which it is connected.  Another way of looking at this variable is as whether addresses of the form foo@workstationname are distinct from addresses of the form foo@localcellname.  The variable can be 1 even if the local cell runs AMS delivery, if the workstation retains a mail system to interpret its own names.  If the value is 1, then the mail configuration options apply to the workstation, not to the local cell. */
#ifdef RUN_AMDS_ENV
int AMS_LocalMailSystemExists = 0;
#else /* RUN_AMDS_ENV */
int AMS_LocalMailSystemExists = 1;
#endif /* RUN_AMDS_ENV */

/* A run-time Boolean to tell us whether to default to using the workstation mail system or the AFS-based mail system, if both exist and the user hasn't expressed a preference.  Initialized to -1, but will be set to 0 or 1 if it is set in AndrewSetup. */
#ifdef RUN_AMDS_ENV
int AMS_DefaultToAFSCellMail = -1;
#else /* RUN_AMDS_ENV */
int AMS_DefaultToAFSCellMail = 0;
#endif /* RUN_AMDS_ENV */

/* A run-time Boolean to tell us whether to allow the workstation authentication to serve as ``authentication'' in ThisDomain. */
#ifdef AFS_ENV
int AMS_ThisDomainAuthFromWS = 0;
#else /* AFS_ENV */
int AMS_ThisDomainAuthFromWS = 1;
#endif /* AFS_ENV */

/* Cell features.  The conventional prefix and suffixes for the cell name to arrive at its common directory and that directory's contents. */
/* The ``CellCommon...'' values are established conventionally, and should not be changed. */
char *CellCommonPrefix = "/afs/";	/* ``Andrew File System'' */
char *CellCommonRWSuffix = ".";	/* read-write mount point affix */
char *CellCommonSuffix = "/service/";
char *CellCommonWPDirSuffix = "wp";	/* /afs/CELLNAME/service/wp is the directory for reading; /afs/CELLNAME/rw/service/wp is the directory for writing. */

char *WorkstationCell = "*";	/* Fill these in at run-time. */
char *WorkstationName = "*";

static struct ConfigStrings {
    char *ConfigKey, **ConfigParm;
} ConfigStrings[] = {
    {"ThisDomain", &ThisDomain},
    {"WorkstationCell", &WorkstationCell},
    {"ViceFile", &ViceFile},
    {"CellCommonPrefix", &CellCommonPrefix},
    {"CellCommonRWSuffix", &CellCommonRWSuffix},
    {"CellCommonSuffix", &CellCommonSuffix},
    {"CellCommonWPDirSuffix", &CellCommonWPDirSuffix},
    {NULL, NULL}
};

static struct ConfigBooleans {
    char *ConfigKey;
    int *ConfigParm;
    int ReverseSense; /* Mostly for compatibility; reverses sense of boolean */
} ConfigBooleans[] = {
    {"AMS_NonViceHost", &AMS_ViceIsRunning, 1},
    {"AMS_NonAMSDelivery", &AMS_DeliverySystem, 1},
    {"AMS_UseWP", &AMS_UseWP, 0},
    {"AMS_OnAIX", &AMS_OnAIX, 0},
    {"AMS_LocalMailSystemExists", &AMS_LocalMailSystemExists, 0},
    {"AMS_DefaultToAFSCellMail", &AMS_DefaultToAFSCellMail, 0},
    {"AMS_ThisDomainAuthFromWS", &AMS_ThisDomainAuthFromWS, 0},
    {NULL, NULL, 0}
};


int CheckServiceConfiguration() {
    int i, val, ConfigErrno, LMSEwasSet, ThisDomainwasSet;
    auto char Scratch[MAXPATHLEN], Scr2[MAXPATHLEN], *s;
    static int AlreadyChecked = 0;

    if (AlreadyChecked) return 0;

    ConfigErrno = errno = LMSEwasSet = 0;
    for (i=0; ConfigBooleans[i].ConfigKey; ++i) {
	if ((s = GetConfiguration(ConfigBooleans[i].ConfigKey)) != NULL && *s != '\0') {
	    while (isspace(*s)) ++s;
	    switch(*s) {
		case 'y': case 'Y': case '1': case 't': case 'T':
		    val = 1;
		    break;
		case 'n': case 'N': case '0': case 'f': case 'F':
		    val = 0;
		    break;
		default:
		    fprintf(stderr, "Bad Boolean value '%s' for '%s' in configuration file; ignored\n", s, ConfigBooleans[i].ConfigKey);
		    val = -1;
		    break;
	    }
	    if (val >= 0) {
		if (ConfigBooleans[i].ReverseSense) val = (val ? 0 : 1);
		*ConfigBooleans[i].ConfigParm = val;
		if (ConfigBooleans[i].ConfigParm == &AMS_LocalMailSystemExists)
			LMSEwasSet = 1;
	    }
	}
	else ConfigErrno = errno;
    }
    for (i=0; ConfigStrings[i].ConfigKey; ++i) {
	if ((s = GetConfiguration(ConfigStrings[i].ConfigKey)) != NULL && *s != '\0') {
	    char *t;

	    t = malloc(1+strlen(s));
	    if (t == NULL) return(-1);
	    strcpy(t, s);
	    *ConfigStrings[i].ConfigParm = t;
	}
    }

/* Another special hack to load the ViceFile variable. */
    if (strcmp(ViceFile, "*") == 0) {
	ViceFile = "/afs";		/* a default value, pointing to constant storage here */
#ifdef AFS_ENV
	i = open("/usr/vice/etc/cacheinfo", O_RDONLY, 0);
	if (i >= 0) {
	    val = read(i, Scratch, sizeof(Scratch)-1);
	    if (val > 0) {
		if (val > (sizeof(Scratch)-1)) val = sizeof(Scratch)-1;
		Scratch[val] = '\0';
		s = index(Scratch, ':');
		if (s != NULL) {
		    *s-- = '\0';	/* Truncate at the first colon */
		    while (s >= Scratch && *s == '/') *s-- = '\0';
		    if (strcmp(Scratch, ViceFile) != 0) {
			s = malloc(1+strlen(Scratch));
			if (s == NULL) return(-1);
			strcpy(s, Scratch);
			ViceFile = s;	/* overwrite the pointer to the static string */
		    }
		}
	    }
	}
#endif /* AFS_ENV */
    }

/* Special hack for AMS_ViceIsRunning--call a function from libutil that tries a pioctl (if included at compilation time) and checks if it generates a SYS signal. */
    if (AMS_ViceIsRunning) AMS_ViceIsRunning = ViceIsRunning();

#ifdef AFS_ENV
    if (ConfigErrno != 0 && AMS_ViceIsRunning) {
	if (ConfigErrno == ENOENT && access(ViceFile, F_OK) && errno == ENOENT) {
	    fprintf(stderr, "Warning: Running with no /AndrewSetup file and the AFS file %s does not exist: system is probably badly configured.\n", ViceFile);
	    return(-1);
	}
    }
#endif /* AFS_ENV */

    if (strcmp(WorkstationName, "*") == 0) {	/* Expect that this was left alone, as "*". */
	GetHostDomainName(Scratch, sizeof(Scratch));
	s = malloc(1+strlen(Scratch));
	if (!s) return(-1);
	strcpy(s, Scratch);
	WorkstationName = s;
    }
/* Begin a collection of ``special hacks'' designed to discover pieces of the envrment. */
    Scr2[0] = '\0';	/* First, look for a workstation cell. */
    if (strcmp(WorkstationCell, "*") == 0) {
	AlreadyChecked = 1;	/* Avoid recursion */
	s = ThisDomain;
	ThisDomain = "";	/* will be used for a default value */
	val = GetCurrentWSCell(Scr2, sizeof(Scr2));
	ThisDomain = s;
	AlreadyChecked = 0;
	if (val != 0) Scr2[0] = '\0';
	else if (Scr2[0] != '\0') {
	    s = malloc(1+strlen(Scr2));
	    if (!s) return -1;
	    strcpy(s, Scr2);
	    WorkstationCell = s;
	}
    }
/* "*" for ThisDomain indicates the workstation name, "&" the workstation cell, and "^" (the compile-time initial value) the defaulting algorithm. */
    ThisDomainwasSet = 1;
    if (strcmp(ThisDomain, "*") == 0) ThisDomain = WorkstationName;
    else if (strcmp(ThisDomain, "&") == 0) {
	if (strcmp(WorkstationCell, "*") == 0)	/* only use it if it's not garbage */
	    ThisDomain = WorkstationName;
	else
	    ThisDomain = WorkstationCell;
    } else if (strcmp(ThisDomain, "^") == 0) {
	ThisDomainwasSet = 0;
	if (AMS_ViceIsRunning == 0 || (Scr2[0] == '\0' && strcmp(WorkstationCell, "*") == 0)
	    || (LMSEwasSet == 1 && AMS_LocalMailSystemExists && !AMS_DefaultToAFSCellMail)) { /* dflt to WSname */
		ThisDomain = WorkstationName;
	} else {		/* Default to WScell */
		ThisDomain = WorkstationCell;
	}
    }
    if (strcmp(ThisDomain, "*") == 0) ThisDomain = WorkstationName;
    ThisDomainLen = strlen(ThisDomain);
    if (strcmp(WorkstationCell, "*") == 0) {
	WorkstationCell = ThisDomain;
    }
    if (LMSEwasSet == 0) {		/* if AMS_LocalMailSystemExists wasn't set, */
	if (AMS_ViceIsRunning == 0 ||
	    (ThisDomainwasSet && ULstrcmp(ThisDomain, WorkstationName) == 0 && ULstrcmp(ThisDomain, WorkstationCell) != 0)) {
		AMS_LocalMailSystemExists = 1;	/* guess that there's a local mail sys */
	} else {
		AMS_LocalMailSystemExists = 0;	/* guess that there isn't */
	}
    }
    AlreadyChecked = 1;
    return(0);
}

#ifdef TESTINGONLYTESTING
main()
{
    CheckServiceConfiguration();
    printf("ViceFile is ``%s''.\n", ViceFile);
    printf("ThisDomain is %s (len %d)\n", ThisDomain, ThisDomainLen);
    printf("WorkstationName is %s\n", WorkstationName);
    printf("A local mail system %s.\n", AMS_LocalMailSystemExists ? "exists" : "does not exist");
    printf("WorkstationCell is ``%s''.\n", WorkstationCell);
    printf("Vice is %srunning.\n", AMS_ViceIsRunning ? "" : "not ");
}
#endif /* TESTINGONLYTESTING */
