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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/wputil/RCS/brisk.c,v 1.11 1992/12/15 21:12:21 rr2b R6tape $";
#endif

/*
  brisk.c: massage captured addresses
*/

#include <andrewos.h> /* sys/time.h */
#include <sys/file.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <util.h>

/* This program will not work at all unless AMS_ENV has been defined. */
#ifdef AMS_ENV
#include <mail.h>
#include <parseadd.h>
#include <mailconf.h>
#include <ckndbm.h>
#ifdef tst_NDBM
#include <sys/types.h>
#include <arpa/nameser.h>
#include <netinet/in.h>
#include <resolv.h>
#endif /* tst_NDBM */

extern int errno;

static int Debugging = 0;

/* Globals set on the command line */
static char *ndbmFile = NULL;	/* -h arg */
static char *OutName = NULL;	/* -o arg */
static int AddrOverride = 0;		/* -A */
static int PersOverride = 0;		/* -P */
static int IdentifyFromReply = 0;	/* -I */

static FILE *inFile = NULL, *outFile = NULL;
static char **fNames;

#define ADDRLEN 2500
#define OTHERLEN 1000
static char InDate[OTHERLEN];
static char InFrom[ADDRLEN];
static char InReply[ADDRLEN];
static char InOrgname[ADDRLEN];

static char OutPersName[ADDRLEN];
static char OutAddress[ADDRLEN];
static char OutAffil[ADDRLEN];
static char OutOtherInfo[ADDRLEN];
static struct Str {char *Buf, *Name; int digitsOnly, Needed;} Fields[] = {
    {InDate, "InDate", 1, 1},
    {InFrom, "InFrom", 0, 1},
    {InReply, "InReply", 0, 1},
    {InOrgname, "InOrgname", 0, 1},
    {OutPersName, "OutPersName", 0, 0},
    {OutAddress, "OutAddress", 0, 0},
    {OutAffil, "OutAffil", 0, 0},
    {OutOtherInfo, "OutOtherInfo", 0, 0},
};
#define numFields (sizeof(Fields) / sizeof(Fields[0]))

static void cvDown(Ptr)
char *Ptr;
{/* Lower-case null-terminated string Ptr in place. */
    while (*Ptr != '\0') {
	if (isupper(*Ptr)) {*Ptr = tolower(*Ptr);}
	++Ptr;
    }
}

#define dom_Garbage 0
#define dom_AlmostGarbage 1
#define dom_Unknown 2
#define dom_Minimum 3
#define dom_Good 4
static struct endGoodness {char *endName; int Good, Preempt;} Ends[] = {
    {".uucp", dom_AlmostGarbage, 1}, {".dec", dom_Garbage, 1}, {".bitnet", dom_Good, 1},
    {".edu", dom_Good, 0}, {".com", dom_Good, 0}, {".gov", dom_Good, 0}, {".org", dom_Good, 0},
    {".net", dom_Good, 0}, {".mil", dom_Good, 0}, {".decnet", dom_Garbage, 1}, {".fi", dom_Good, 0},
    {".se", dom_Good, 0}, {".uk", dom_Good, 0}, {".ca", dom_Good, 0}, {".ie", dom_Good, 0},
    {".ch", dom_Good, 0}, {".nz", dom_Good, 0}, {".dk", dom_Good, 0}, {".nl", dom_Good, 0},
    {".it", dom_Good, 0}, {".il", dom_Good, 0}, {".us", dom_Good, 0}, {".au", dom_Good, 0},
    {".no", dom_Good, 0}, {".jp", dom_Good, 0}, {".de", dom_Good, 0}, {".fr", dom_Good, 0},
    {".is", dom_Good, 0}, {".es", dom_Good, 0}, {".sg", dom_Good, 0}, {".pt", dom_Good, 0},
    {".be", dom_Good, 0}, {".span", dom_Garbage, 1}, {".oz", dom_Garbage, 0},
    {".bionet", dom_Garbage, 1}, {".junet", dom_Garbage, 1}, {".uninett", dom_Garbage, 1},
    {".mailnet", dom_Garbage, 1}, {".csnet", dom_Garbage, 1}, {".ccnet", dom_Garbage, 1},
    {".janet", dom_Garbage, 1}, {".mfenet", dom_Garbage, 1}, {".geonet", dom_Garbage, 1},
    {".star", dom_Garbage, 1}, {".hepnet", dom_Garbage, 1}, {".dnet", dom_Garbage, 1},
    {".local", dom_Garbage, 1}, {".cdn", dom_Garbage, 0}, {".cmu", dom_Garbage, 0},
    {".profs", dom_Garbage, 1}, {".sdscnet", dom_Garbage, 1}, {".edutale", dom_Garbage, 0},
    {".dri", dom_Garbage, 0}, {".cern", dom_Garbage, 0}, {".taeva", dom_Garbage, 0},
    {".osbunorth", dom_Garbage, 1}, {".houston", dom_Garbage, 1}, {".steinmetz", dom_Garbage, 1},
    {".gm", dom_Garbage, 0}, {".gwd", dom_Garbage, 0}, {".irb", dom_Garbage, 0},
    {".chunet", dom_Garbage, 1}, {".astronet", dom_Garbage, 1}, {".peacenet", dom_Garbage, 1},
};
#define numEnds (sizeof(Ends) / sizeof(Ends[0]))

static char IsDomain(hst, outhst)
char *hst, *outhst;
{/* Check the goodness of host ``hst''.  Copy it, or what it's an alias for, to ``outhst''. */
    int Ix;
    char *s, *t;

    if (hst == NULL || *hst == '\0') {outhst[0] = '\0'; return dom_Garbage;}
    strcpy(outhst, hst);
    cvDown(outhst);
    t = index(hst, '.');
    if (t == NULL) return dom_Garbage;
    s = rindex(hst, '.');
    if (ULstrcmp(s, ".arpa") == 0) {
	if (s != t) return dom_Garbage;
    } else for (Ix = 0; Ix < numEnds; ++Ix) if (Ends[Ix].Preempt && ULstrcmp(s, Ends[Ix].endName) == 0) return (Ends[Ix].Good);
#ifdef tst_NDBM
    {
	static char badHost[] = "#";
	static char unkHost[] = "&";
	static DBM *db = NULL;
	datum key, val;
	enum MailHostQuality mhq;
#ifdef RESOLVER_ENV
	extern struct state _res;
#endif /* RESOLVER_ENV */

	if (db == NULL && ndbmFile != NULL) {
	    db = dbm_open(ndbmFile, O_RDWR|O_CREAT, 0666);
	    if (db == NULL) ndbmFile = NULL;
#ifdef RESOLVER_ENV
#ifdef RES_DEFNAMES
	    _res.options &= ~RES_DEFNAMES;
#endif /* !RES_DEFNAMES */
#ifdef RES_DNSRCH
	    _res.options &= ~RES_DNSRCH;
#endif /* RES_DNSRCH */
#endif /* RESOLVER_ENV */
	}
	if (db != NULL) {
	    cvDown(hst);
	    key.dptr = hst;
	    key.dsize = strlen(hst);
	    val = dbm_fetch(db, key);
	    if (dbm_error(db)) {val.dptr = NULL; dbm_clearerr(db);}
	    if (val.dptr != NULL) {
		if (strncmp(val.dptr, badHost, val.dsize) == 0) {
		    return dom_Garbage;
		} else if (strncmp(val.dptr, unkHost, val.dsize) != 0) {
		    strncpy(outhst, val.dptr, val.dsize);
		    outhst[val.dsize] = '\0';
		    t = index(outhst, '.');
		    if (t == NULL) return dom_Garbage;
		    else if (ULstrcmp(t, ".arpa") == 0) return dom_Minimum;
		    else return dom_Good;
		}
	    } else {
		outhst[0] = '\0';
		mhq = ValidateMailHostName(hst, outhst, ADDRLEN, 10);
		cvDown(outhst);
		switch (mhq) {
		    case mailhost_bad:
			val.dptr = badHost;
			val.dsize = sizeof(badHost) - 1;
			break;
		    case mailhost_good:
			val.dptr = outhst;
			val.dsize = strlen(outhst);
			break;
		    default:
		    case mailhost_indeterminate:
			val.dptr = NULL;
			val.dsize = 0;
			break;
		}
		if (val.dptr != NULL) {
		    if (osi_ExclusiveLockNoBlock(dbm_dirfno(db)) == 0) {
			(void) dbm_store(db, key, val, DBM_REPLACE);
			if (dbm_error(db)) dbm_clearerr(db);
			else if (Debugging) {
			    fprintf(stderr, "Storing %s -> %s\n", hst, outhst);
			}
			osi_UnLock(dbm_dirfno(db));
		    }
		}
		if (outhst[0] == '\0') {
		    strcpy(outhst, hst);
		    return (mhq == mailhost_bad ? dom_Garbage : dom_Unknown);
		} else {
		    if (mhq == mailhost_good) {
			t = index(outhst, '.');
			if (t == NULL) return dom_Unknown;
			else if (ULstrcmp(t, ".arpa") == 0) return dom_Minimum;
			else return dom_Good;
		    } else if (mhq == mailhost_bad) return dom_Garbage;
		    else return dom_Unknown;
		}
	    }
	}
    }
#endif /* tst_NDBM */
    if (ULstrcmp(s, ".arpa") == 0) return dom_Minimum;
    for (Ix = 0; Ix < numEnds; ++Ix) if (!Ends[Ix].Preempt && ULstrcmp(s, Ends[Ix].endName) == 0) return (Ends[Ix].Good);
    if (Debugging) fprintf(stderr, "IsDomain: unknown goodness for ``%s''\n", hst);
    return dom_Unknown;
}


static void OpenIn()
{/* Open the first file for reading. */
    if (fNames == 0) {
	inFile = stdin;
    } else {
	errno = 0;
	inFile = fopen(*fNames, "r");
	if (inFile == NULL) {fprintf(stderr, "Can't open %s for reading: %s\n", *fNames, UnixError(errno == 0 ? ENOMEM : errno)); exit(1);}
    }
}

static void CloseIn()
{/* Close off any input file. */
    if (inFile != NULL) fclose(inFile);
    inFile = NULL;
}

static int nextC()
{/* Get the next char.  Skip to the next file if needed. */
    int C;

    C = getc(inFile);
    if (fNames != 0) {
	while (C == EOF) {
	    ++fNames;
	    if (*fNames == NULL) break;
	    fclose(inFile);
	    errno = 0;
	    inFile = fopen(*fNames, "r");
	    if (inFile == NULL) {fprintf(stderr, "Can't open %s for reading: %s\n", *fNames, UnixError(errno == 0 ? ENOMEM : errno)); exit(1);}
	    C = getc(inFile);
	}
    }
    return C;
}
    
static int LoadFields()
{/* Return 0 on EOF, <0 for trouble, >0 for success. */
    char *s, *s0;
    static int Char = '\n';
    int FldIx;

    for (;;) {
	for (FldIx = 0; FldIx < numFields; ++FldIx) (Fields[FldIx].Buf)[0] = '\0';
	for (;;) {	/* Scan to the beginning of a line that starts with a vertical bar. */
	    if (Char == EOF) return 0;
	    else if (Char == '|') break;
	    else if (Char != '\n') {
		for (;;) {
		    Char = nextC();
		    if (Char == EOF) return 0;
		    if (Char == '\n') break;
		}
	    }
	    Char = nextC();
	}
	for (FldIx = 0; FldIx < numFields; ++FldIx) {	/* Char is a vertical bar */
	    Char = nextC();		/* skip past it. */
	    if (Char == EOF || Char == '\n') {
		/* Have we read enough of this line? */
		if (Fields[FldIx].Needed == 0) return 1;   /* Need the In fields, not the Out ones. */
		else goto BadLine;  /* Skip this whole line: erase and continue. */
	    }
	    s0 = s = Fields[FldIx].Buf;
	    while (Char != '|') {
		if (Fields[FldIx].digitsOnly && !isdigit(Char)) return -1;
		*s++ = Char;
		Char = nextC();
		if (Char == EOF || Char == '\n') {
		    if (Fields[FldIx].Needed == 0) return 1;
		    else {*s0 = '\0'; goto BadLine;}
		}
	    }
	    *s++ = '\0';
	}
	Char = nextC();	/* Char is again a vertical bar. */
	while (Char != '\n' && Char != EOF) Char = nextC();
	return 1;
	BadLine:;    /* Zip back to the next line */
    }
}

static void DumpFields()
{/* Debugging purposes. */
    int FldIx;
    fprintf(stderr, "\n");
    for (FldIx = 0; FldIx < numFields; ++FldIx) fprintf(stderr, "%s: ``%s''\n", Fields[FldIx].Name, Fields[FldIx].Buf);
}

static int WriteFields()
{/* Write the current set of fields.  Return 0 if OK, non-0 on failure. */
    if (OutAddress[0] != '\0') {
	fprintf(outFile, "|%s|%s|%s|%s|%s|%s|%s|%s|\n",
		InDate, InFrom, InReply, InOrgname,
		OutPersName, OutAddress, OutAffil, OutOtherInfo);
    }
    return 0;
}

static int singAddr(Head, Res)
PARSED_ADDRESS *Head, **Res;
{/* Count the elements under Head, return how many there were, and point to one of them via Res. */
    int RNum;

    RNum = 0;
    FOR_ALL_ADDRESSES(ThisAddr, Head, {
		       switch (ThisAddr->Kind) {
			   case SIMPLE_ADDRESS:
			       ++RNum;
			       *Res = ThisAddr;
			       break;
			   case GROUP_ADDRESS:
			       RNum += singAddr(ThisAddr->Members, Res);
			       break;
			   default:
			       break;
		       }
		       })
      return RNum;
}

static PARSED_ADDRESS *SingleAddress(Head)
PARSED_ADDRESS *Head;
{/* If list Head has a single element, return it, else return NULL. */
    int Count;
    PARSED_ADDRESS *Res;
    Count = singAddr(Head, &Res);
    return (Count == 1 ? Res : NULL);
}

static int AnyNonBlank(txt)
char *txt;
{/* Return TRUE iff there's any non-blank text in the character string txt. */
    if (txt != NULL) for (; *txt != '\0'; ++txt) {
	if (isascii(*txt) && *txt != ' ' && *txt != '(' && *txt != ')' && isprint(*txt)) return 1;
    }
    return 0;
}

static char IsNameChar[128] = {
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 0000 - 0017 */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 0020 - 0037 */
    1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 1, 0, /* 0040 - 0057 */ /* space, apostrophe, comma, hyphen, period */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /* 0060 - 0077 */
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 0100 - 0117 */ /* A-O */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, /* 0120 - 0137 */ /* P-Z, underscore */
    0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /* 0140 - 0157 */ /* a-o */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, /* 0160 - 0177 */ /* p-z */
/* Consider '{' to be name chars for national usage */
};

static int AllNameChars(txt)
char *txt;
{/* Return TRUE iff all characters in txt might reasonably appear in a personal name. */
    if (txt == NULL) return 0;
    for (; *txt != '\0'; ++txt) {
	if (!isascii(*txt) || IsNameChar[*txt] == 0) return 0;
    }
    return 1;
}

static int GetAddress(Addr, Res)
PARSED_ADDRESS *Addr;
char *Res;
{/* Write the address to Res and return an indication of how good an address it is (0 = unusable, and the larger the result the better). */
    char *s, *t, *u, *v, *q;
    int HValue, LValue, isDom, DomCode, AnyDomCode, UnPRes, FinalRes;
    auto char User[ADDRLEN], Host[ADDRLEN];
    auto char addrCopy[ADDRLEN];
    auto char hostCopy[ADDRLEN];
    ADDRESS_COMMENT *CDum;
    ADDRESS_HOST *inHost;
    char *RPDum;

    Res[0] = '\0';  /* Initialize results */
    HValue = LValue = 0;
    if (Debugging) fprintf(stderr, "GetAddress(%s @ %s): ", Addr->LocalPart,
			    (Addr->Hosts->Prev->Name != NULL ? Addr->Hosts->Prev->Name : "NULL"));
    DomCode = AnyDomCode = -1;
    if (Addr->Hosts->Prev == Addr->Hosts) {
	HValue = 5;
    }
    inHost = NULL;
    FOR_ALL_REVERSE_HOSTS(Hst, Addr, {
			   isDom = IsDomain(Hst->Name, addrCopy);
			   if (AnyDomCode < 0) AnyDomCode = isDom;
			   if (isDom >= dom_Minimum) {DomCode = isDom; inHost = Hst; strcpy(hostCopy, addrCopy);}
			   })
    if (inHost != NULL) {
	FOR_ALL_REVERSE_HOSTS(Hst, Addr, {
			      if (Hst == inHost) {
				  cvDown(Hst->Name);
				  if (strcmp(hostCopy, Hst->Name) != 0) {
				      RPDum = NewString(hostCopy);
				      if (RPDum != NULL) {
					  free(Hst->Name);
					  Hst->Name = RPDum;
				      }
				  }
				  break;
			      }
			      RemHost(Hst);
			      })
	  if (DomCode < 0) DomCode = AnyDomCode;
    }
    strncpy(addrCopy, Addr->LocalPart, ADDRLEN);
    s = addrCopy;
    for (;;) {
	UnPRes = strlen(s);
	if (*s == '"' && s[UnPRes-1] == '"') {
	    s[UnPRes-1] = '\0';
	    ++s;
	} else if (*s == '!') ++s;
	else if (*s == '%') ++s;
	else break;
    }
    t = index(s, '%');
    u = index(s, '!');
    Host[0] = User[0] = '\0';
    if (t != NULL && u != NULL) LValue = 1; /* mixed address */
    else if (t == NULL && u != NULL) {	/* Excl's only */
	v = rindex(s, '!');
	for (;;) {
	    *v = '\0';
	    q = rindex(s, '!');
	    if (q == NULL || *q == '\0') q = s;
	    else ++q;
	    isDom = IsDomain(q, hostCopy);
	    if (isDom >= dom_Minimum) {
		DomCode = isDom;
		strncpy(User, v+1, sizeof(User));
		strncpy(Host, hostCopy, sizeof(Host));
		*v = '!';
		break;
	    } else {
		*v = '!';
		if (q == s) break;
		v = q-1;
	    }
	}
	LValue = (Host[0] == '\0' ? 5 : 15);
    } else if (t != NULL && u == NULL) {	/* Percents only */
	v = t;
	for (;;) {
	    q = index(v+1, '%');
	    if (q != NULL) *q = '\0';
	    isDom = IsDomain(v+1, hostCopy);
	    if (isDom >= dom_Minimum) {
		*v = '\0';
		DomCode = isDom;
		strncpy(User, s, sizeof(User));
		strncpy(Host, hostCopy, sizeof(Host));
		*v = '%';
		if (q != NULL) *q = '%';
		break;
	    }
	    if (q != NULL) {*q = '%'; v = q;}
	    else break;
	}
	LValue = (Host[0] == '\0' ? 4 : 14);
    } else LValue = 20;
    if (DomCode >= 0) {
	switch (DomCode) {
	    case dom_Garbage:
		HValue = 3; break;
	    case dom_AlmostGarbage:
		HValue = 5; break;
	    case dom_Unknown: default:
		HValue = 8; break;
	    case dom_Minimum:
		HValue = 15; break;
	    case dom_Good:
		HValue = 20; break;
	}
    }
    FinalRes = (HValue <= LValue ? HValue : LValue);
    if (User[0] != '\0') {
	for (s = User; *s != '\0'; ++s) {
	    if (*s != '.' && !IsOK822Atom(*s)) {User[0] = '\0'; break;}
	}
    }
    if (User[0] != '\0' && Host[0] != '\0') {
	cvDown(Host);
	sprintf(Res, "%s@%s", User, Host);
	if (Debugging) fprintf(stderr, " shortcut ``%s'' (%d)\n", Res, FinalRes);
	return FinalRes;
    } else {
	CDum = Addr->Comments;
	Addr->Comments = NULL;
	RPDum = Addr->RoutePhrase;
	Addr->RoutePhrase = NULL;
	UnPRes = UnparseOneAddress(Addr, UP_SPACES_TO_DOTS, Res, ADDRLEN, "", ADDRLEN);
	Addr->Comments = CDum;
	Addr->RoutePhrase = RPDum;
	if (UnPRes != PA_OK) {
	    Res[0] = '\0';
	    if (Debugging) fprintf(stderr, "no unparse: %d (0)\n", UnPRes);
	    return 0;
	} else {
	    if (Debugging) fprintf(stderr, "``%s'' (%d)\n", Res, FinalRes);
	    return FinalRes;
	}
    }
}

static void FixGecos(fld, pwnam)
char *fld, *pwnam;
{/* Heuristically guess whether the argument points to an encoded pw_gecos field, and if so, replace it with a simple person name. */
    char *s, *d, *t;
    auto char outFld[ADDRLEN], outNam[ADDRLEN];

    if (strncmp(fld, "pri=", 4) == 0) {
	s = fld+4;
	while (*s != '\0' && (isdigit(*s) || *s == '-')) ++s;
	if (*s != '\0' && isspace(*s)) {
	    while (*s != '\0' && isspace(*s)) ++s;
	    if (*s != '\0') {
		d = fld;
		while (*s != '\0') *d++ = *s++;
		*d = '\0';
	    }
	}
    }
    if (isdigit(*fld)) {
	for (s = fld; *s != '\0' && isdigit(*s); ++s);
	if (*s == '-') {
	    ++s;
	    if (isupper(*s)) {  /* System-V pw_gecos field */
		if (Debugging) fprintf(stderr, "FixGecos(``%s'') produces ", fld);
		d = fld;
		while (*s != '\0') *d++ = *s++;
		*d = '\0';
		if (Debugging) fprintf(stderr, "``%s''.\n", fld);
		return;
	    }
	}
    }
    s = index(fld, ',');    /* Look for two field-separating commas. */
    if (s != NULL && !isspace(s[1])) {
	d = index(s+1, ',');
	if (d != NULL && !isspace(d[1])) { /* BSD finger-able pw_gecos value */
	    if (Debugging) fprintf(stderr, "FixGecos(``%s'') produces ", fld);
	    *s = '\0';	/* Truncate it */
	    s = fld;
	    d = outFld;
	    for (;*s != '\0'; ++s) {
		if (*s == '&') {
		    t = pwnam;
		    if (t != (char)0 && t[0] != (char)0) {
			strncpy(outNam, t, ADDRLEN);
			cvDown(outNam);
			if (islower(outNam[0])) outNam[0] = toupper(outNam[0]);
			for (t = outNam; *t != '\0'; ++t) *d++ = *t;
		    } else {
			*d++ = *s;
		    }
		} else *d++ = *s;
	    }
	    strncpy(fld, outFld, ADDRLEN);
	    if (Debugging) fprintf(stderr, "``%s''.\n", fld);
	    return;
	}
    }
    if (isupper(*fld)) {
	for (s = fld; *s != '\0' && (isupper(*s) || isdigit(*s)); ++s) ;
	if (*s == '-') {
	    ++s;
	    if (isupper(s[0]) && (islower(s[1]) || s[1] == ' ' || s[1] == '.')) {	/* another System-V pw_gecos */
		if (Debugging) fprintf(stderr, "FixGecos(``%s'') produces ", fld);
		d = fld;
		while (*s != '\0') *d++ = *s++;
		*d = '\0';
		if (Debugging) fprintf(stderr, "``%s''.\n", fld);
		return;
	    }
	}
    }
}

static int GetPersonal(comm, nameRes, affilRes, otherRes)
char *comm; char *nameRes, *affilRes, *otherRes;
{/* Get personal information from the given text string and return an indication of how good that information is (0 for unusable; the larger the better). */
    char *nComm, *s, *t, *u, *v;
    int Ctr, oldC, RDum;
    auto char Copy[ADDRLEN];
    static char EndGarbage[] = "\",-=<>~/;:_!|";

    nameRes[0] = affilRes[0] = otherRes[0] = '\0';
    if (Debugging) fprintf(stderr, "GetPersonal(``%s''): ", comm);
    strncpy(Copy, comm, ADDRLEN);
    nComm = Copy;
    while (*nComm != '\0' && (isspace(*nComm) || index(EndGarbage, *nComm) != NULL)) ++nComm;
    if (*nComm != '\0') {
	t = &nComm[strlen(nComm)-1];
	while (t >= nComm && (isspace(*t) || index(EndGarbage, *t) != NULL)) --t;
	++t; *t = '\0';
    }
    for (s = t = nComm; *s != '\0'; ++s) {
	if (*s != '"') *t++ = *s;
    }
    *t = '\0';
    if (*nComm == '(') {
	Ctr = strlen(nComm);
	for (;;) {
	    if (nComm[Ctr-1] != ')') break;
	    nComm[Ctr-1] = '\0';
	    ++nComm;
	    if (*nComm != '(') break;
	    Ctr -= 2;
	}
    }
    s = index(nComm, '@');
    if (s == NULL) s = index(nComm, ';');
    if (s == NULL) s = index(nComm, '/');
    if (s != NULL && s > nComm) {
	t = s+1;
	RDum = 12;
	if (!isspace(*t)) --RDum;
	while (isspace(*t)) ++t;
	--s;
	if (!isspace(*s)) --RDum;
	while (isspace(*s) && s > nComm) --s;
	++s;
	oldC = *s;
	*s = '\0';
	if (AllNameChars(nComm)) {
	    strncpy(nameRes, nComm, ADDRLEN);
	    *s = oldC;
	    CanonicalizePersonName(nameRes);
	    if (index(nameRes, ' ') == NULL) --RDum;
	    strncpy(otherRes, t, ADDRLEN);
	    if (Debugging) fprintf(stderr, "name ``%s'' other ``%s'' (%d)\n", nameRes, otherRes, RDum);
	    return RDum;
	}
	*s = oldC;
    }
    s = index(nComm, ',');
    if (s != NULL && s > nComm && isspace(s[1])) {
	t = s+2;
	while (isspace(*t)) ++t;
	oldC = *t;
	*t = '\0';
	if (oldC != '\0' && AllNameChars(nComm)) {
	    *t = oldC;
	    if (ULstrcmp(t, "jr") == 0 || ULstrcmp(t, "jr.") == 0 || ULstrcmp(t, "iii") == 0 || ULstrcmp(t, "iv") == 0 || ULstrcmp(t, "ii") == 0 || ULstrcmp(t, "v") == 0) {
		strncpy(nameRes, nComm, ADDRLEN);
		CanonicalizePersonName(nameRes);
		if (Debugging) fprintf(stderr, "name ``%s'' (20)\n", nameRes);
		return 20;
	    }
	    --s;
	    while (s > nComm && isspace(*s)) --s;
	    ++s;
	    *s = '\0';
	    strncpy(nameRes, nComm, ADDRLEN);
	    CanonicalizePersonName(nameRes);
	    strncpy(otherRes, t, ADDRLEN);
	    RDum = 8;
	    if (index(nameRes, ' ') == NULL) --RDum;
	    if (Debugging) fprintf(stderr, "name ``%s'' other ``%s'' (%d)\n", nameRes, otherRes, RDum);
	    return RDum;
	}
	*t = oldC;
    }
    for (s = index(nComm, '-'); s != NULL; s = index(s+1, '-')) {
	if (s > nComm) {
	    if (isspace(s[-1]) && (s[1] == '-' || isspace(s[1]))) {
		t = s-2;
		while (t > nComm && isspace(*t)) --t;
		++t; *t = '\0';
		++s;
		while (*s != '\0' && (*s == '-' || isspace(*s))) ++s;
		strncpy(nameRes, nComm, ADDRLEN);
		CanonicalizePersonName(nameRes);
		strncpy(otherRes, s, ADDRLEN);
		RDum = 12;
		if (index(nameRes, ' ') == NULL) --RDum;
		if (Debugging) fprintf(stderr, "name ``%s'' other ``%s'' (%d)\n", nameRes, otherRes, RDum);
		return RDum;
	    }
	}
    }
/* Look for parenthesized comments */
    s = &nComm[strlen(nComm)-1];
    if (*s == ')') {
	Ctr = 0; u = NULL;
	for (t = nComm; *t != '\0'; ++t) {
	    if (*t == '(') {++Ctr; if (u == NULL) u = t;}
	    else if (*t == ')') --Ctr;
	    else if (*t == '\\') {++t; if (*t == '\0') break;}
	}
	if (Ctr == 0 && u != NULL) {
	    v = u-1;
	    while (v > nComm && isspace(*v)) --v;
	    if (v > nComm) {
		++v;
		oldC = *v;
		*v = '\0';
		if (AllNameChars(nComm)) {
		    strncpy(nameRes, nComm, ADDRLEN);
		    CanonicalizePersonName(nameRes);
		    *v = oldC;
		    *s = '\0';
		    strncpy(otherRes, u+1, ADDRLEN);
		    RDum = 14;
		    if (index(nameRes, ' ') == NULL) --RDum;
		    if (Debugging) fprintf(stderr, "name ``%s'' other ``%s'' (%d)\n", nameRes, otherRes, RDum);
		    return RDum;
		}
		*v = oldC;
	    }
	}
    }
/* Look for comments in braces */
    s = &nComm[strlen(nComm)-1];
    u = index(nComm, '{');
    if (*s == '}' && u != NULL) {
	t = rindex(nComm, '{');
	if (t == u) {
	    v = t-1;
	    while (v > nComm && isspace(*v)) --v;
	    if (v > nComm) {
		++v;
		oldC = *v;
		*v = '\0';
		if (AllNameChars(nComm)) {
		    strncpy(nameRes, nComm, ADDRLEN);
		    CanonicalizePersonName(nameRes);
		    *v = oldC;
		    *s = '\0';
		    strncpy(otherRes, t+1, ADDRLEN);
		    RDum = 12;
		    if (index(nameRes, ' ') == NULL) --RDum;
		    if (Debugging) fprintf(stderr, "name ``%s'' other ``%s'' (%d)\n", nameRes, otherRes, RDum);
		    return RDum;
		}
		*v = oldC;
	    }
	}
    }
/* Look for trailing phone numbers */
    s = &nComm[strlen(nComm)-1];
    if (s > nComm && isdigit(*s)) {
	t = NULL;
	while (s > nComm) {
	    if (index("0123456789+(){}", *s) != NULL) t = s;
	    else if (index("xX. -", *s) == NULL) break;
	    --s;
	}
	if (s > nComm && t != NULL && t > nComm) {
	    u = t-1;
	    Ctr = 0;
	    if (isspace(*u) || *u == ',') Ctr = 1;
	    else if (u > nComm && isspace(u[-1])) {--u; Ctr = 2;}
	    if (Ctr != 0) {
		while (u > nComm && isspace(*u)) --u;
		if (u > nComm) {
		    ++u;
		    oldC = *u;
		    *u = '\0';
		    if (AllNameChars(nComm)) {
			strncpy(nameRes, nComm, ADDRLEN);
			CanonicalizePersonName(nameRes);
			*u = oldC;
			if (Ctr == 1) {
			    if (*t == '(' && t[strlen(t)-1] == ')') {
				strncpy(otherRes, t+1, ADDRLEN);
				otherRes[strlen(otherRes)-1] = '\0';
			    } else strncpy(otherRes, t, ADDRLEN);
			}
			RDum = 16;
			if (index(nameRes, ' ') == NULL) --RDum;
			if (Debugging) {
			    fprintf(stderr, "name %s", nameRes);
			    if (otherRes[0] != '\0') fprintf(stderr, " other/phone %s", otherRes);
			    fprintf(stderr, " (%d)\n", RDum);
			}
			return RDum;
		    }
		    *u = oldC;
		}
	    }
	}
    }
    if (AllNameChars(nComm)) {
	strncpy(nameRes, nComm, ADDRLEN);
	CanonicalizePersonName(nameRes);
	RDum = 8;
	if (index(nameRes, ' ') == 0) --RDum;
	if (Debugging) fprintf(stderr, "name ``%s'' (%d)\n", nameRes, RDum);
	return RDum;
    }
    if (Debugging) fprintf(stderr, "nothing (0)\n");
    return 0;
}

static int GetPersEffects(Addr, nameRes, affilRes, otherRes)
PARSED_ADDRESS *Addr;
char *nameRes, *affilRes, *otherRes;
{/* Get personal information from the address (personal name, text affiliation, anything else) and return an indication of how good the information is (0 = unusable; the larger the result the better). */
    auto char wk1[ADDRLEN], wk2[ADDRLEN], wk3[ADDRLEN], wk4[ADDRLEN], wk5[ADDRLEN], wk6[ADDRLEN];
    auto char Comm[ADDRLEN];
    char *thisComm;
    char *s, *t, *u, *v;
    int UseRoutePhrase, UseComments;
    int qual1, qual2, Ctr, RDum;

    nameRes[0] = affilRes[0] = otherRes[0] = '\0';
    thisComm = Comm;
    UseRoutePhrase = AnyNonBlank(Addr->RoutePhrase);
    UseComments = (Addr->Comments != NULL && AnyNonBlank(Addr->Comments->Text));
    if (UseComments) {
	strncpy(Comm, Addr->Comments->Text, sizeof(Comm));
	thisComm = Comm;
	if (*thisComm == '(') {
	    Ctr = strlen(thisComm);
	    for (;;) {
		if (thisComm[Ctr-1] != ')') break;
		thisComm[Ctr-1] = '\0';
		++thisComm;
		if (*thisComm != '(') break;
		Ctr -= 2;
	    }
	}
	FixGecos(thisComm, Addr->LocalPart);
	if (thisComm[0] == '\0') UseComments = 0;
    }
    if (UseRoutePhrase && UseComments) {    /* could be bogus--better heuristic blend? */
	qual1 = GetPersonal(Addr->RoutePhrase, wk1, wk3, wk5);
	qual2 = GetPersonal(thisComm, wk2, wk4, wk6);
	if (qual1 <= 0) wk1[0] = wk3[0] = wk5[0] = '\0';
	if (qual2 <= 0) wk2[0] = wk4[0] = wk6[0] = '\0';
	if (wk1[0] != '\0' && wk2[0] != '\0') {
	    strncpy(nameRes, (qual1 >= qual2 ? wk1 : wk2), ADDRLEN);
	} else if (wk1[0] != '\0' || wk2[0] != '\0') {
	    strncpy(nameRes, (wk1[0] != '\0' ? wk1 : wk2), ADDRLEN);
	}
	if (wk3[0] != '\0' && wk4[0] != '\0') {
	    strncpy(affilRes, (qual1 >= qual2 ? wk3 : wk4), ADDRLEN);
	} else if (wk3[0] != '\0' || wk4[0] != '\0') {
	    strncpy(affilRes, (wk3[0] != '\0' ? wk3 : wk4), ADDRLEN);
	}
	if (wk5[0] != '\0' && wk6[0] != '\0') {
	    strncpy(otherRes, (qual1 >= qual2 ? wk5 : wk6), ADDRLEN);
	} else if (wk5[0] != '\0' || wk6[0] != '\0') {
	    strncpy(otherRes, (wk5[0] != '\0' ? wk5 : wk6), ADDRLEN);
	}
	return (qual1 >= qual2 ? qual1 : qual2);
    } else if (UseRoutePhrase) {
	return GetPersonal(Addr->RoutePhrase, nameRes, affilRes, otherRes);
    } else if (UseComments) {
	return GetPersonal(thisComm, nameRes, affilRes, otherRes);
    } else if (Addr->LocalPart[0] != '\0') {
	if (Debugging) fprintf(stderr, "GetPersEffects(``%s''): ", Addr->LocalPart);
	RDum = 9;
	strncpy(wk1, Addr->LocalPart, sizeof(wk1));
	qual1 = strlen(wk1);
	if (wk1[0] == '"' && wk1[qual1-1] == '"') {
	    wk1[qual1-1] = '\0';
	    s = wk1+1;
	} else s = wk1;
	u = index(s, '%');
	v = index(s, '!');
	if (u != NULL) --RDum;
	if (v != NULL) --RDum;
	if (u != NULL && v == NULL) {	/* percent-path */
	    for (t = u; *t != '\0'; ++t) {
		if (!isalnum(*t) && *t != '%' && *t != '.' && *t != '-' && *t != '#') break;
	    }
	    if (*t == '\0' && u != s) *u = '\0';
	} else if (u == NULL && v != NULL) {	/* bang-path */
	    v = rindex(s, '!');
	    for (t = s; t < v; ++t) {
		if (!isalnum(*t) && *t != '!' && *t != '.' && *t != '-' && *t != '#') break;
	    }
	    if (t == v && v[1] != '\0') s = v+1;
	}
	wk5[0] = '\0';
	u = index(s, '.'); if (u != NULL) u = rindex(s, '.');
	v = index(s, '_');
	t = (u == NULL ? NULL : index(u, '_'));
	if (u != NULL && t == NULL && v != NULL) {	/* foo_b._baz.XeroxDom */
	    *u++ = '\0';
	    strncpy(wk5, u, ADDRLEN);
	    RDum = 10;
	}
	if (AllNameChars(s)) {
	    strncpy(nameRes, s, ADDRLEN);
	    CanonicalizePersonName(nameRes);
	    strncpy(otherRes, wk5, ADDRLEN);
	    if (index(nameRes, ' ') == NULL) --RDum;
	    if (Debugging) {
		fprintf(stderr, "name %s", nameRes);
		if (otherRes[0] != '\0') fprintf(stderr, " other %s", otherRes);
		fprintf(stderr, " (%d)\n", RDum);
	    }
	    return RDum;
	}
	if (Debugging) fprintf(stderr, "nothing (0)\n");
    }
    return 0;
}

static int CanonAndWrite()
{/* Canonicalize the InFrom and InOrgname into the OutXXX fields.  Return 0 if OK, non-0 on failure. */
    PARSED_ADDRESS *FromHead, *ReplyHead;
    PARSED_ADDRESS *From, *Reply;
    auto char Work1[ADDRLEN], Work2[ADDRLEN], Work3[ADDRLEN], Work4[ADDRLEN], Work5[ADDRLEN], Work6[ADDRLEN];
    int Qual1, Qual2;
    char fc, rc;
    
    FromHead = ReplyHead = NULL;
    From = Reply = NULL;
    if (InFrom[0] != '\0') {
	if (ParseAddressList(InFrom, &FromHead) != PA_OK) FromHead = NULL;
    }
    if (InReply[0] != '\0' && (InFrom[0] == '\0' || strcmp(InFrom, InReply) != 0)) {
	if (ParseAddressList(InReply, &ReplyHead) != PA_OK) ReplyHead = NULL;
    }
    if (FromHead != NULL) {
	From = SingleAddress(FromHead);
	if (From == NULL) {
	    FreeAddressList(FromHead);
	    FromHead = NULL;
	} else {
	    if (From->Hosts != NULL && From->Hosts->Prev != NULL && From->Hosts->Prev->Name != NULL) cvDown(From->Hosts->Prev->Name);
	}
    }
    if (ReplyHead != NULL) {
	Reply = SingleAddress(ReplyHead);
	if (Reply == NULL) {
	    FreeAddressList(ReplyHead);
	    ReplyHead = NULL;
	} else {
	    if (Reply->Hosts != NULL && Reply->Hosts->Prev != NULL && Reply->Hosts->Prev->Name != NULL) cvDown(Reply->Hosts->Prev->Name);
	}
    }
    if (FromHead != NULL && ReplyHead != NULL) {
	if (IdentifyFromReply || ULstrcmp(From->LocalPart, Reply->LocalPart) == 0) {
	    if (AddrOverride || OutAddress[0] == '\0') {
		Qual1 = GetAddress(From, Work1);
		Qual2 = GetAddress(Reply, Work2);
		if (Qual1 > 0 && Qual1 >= Qual2) {
		    strncpy(OutAddress, Work1, sizeof(OutAddress));
		} else if (Qual2 > 0 && Qual2 >= Qual1) {
		    strncpy(OutAddress, Work2, sizeof(OutAddress));
		}
	    }
	    if (PersOverride || OutPersName[0] == '\0') {
		Work1[0] = Work2[0] = Work3[0] = Work4[0] = Work5[0] = Work6[0] = '\0';
		Qual1 = GetPersEffects(From, Work1, Work3, Work5);
		Qual2 = GetPersEffects(Reply, Work2, Work4, Work6);
		if (Qual1 > 0 && Qual1 >= Qual2) {
		    strncpy(OutPersName, Work1, sizeof(OutPersName));
		} else if (Qual2 > 0 && Qual2 >= Qual1) {
		    strncpy(OutPersName, Work2, sizeof(OutPersName));
		}
		if (Work3[0] != '\0') {
		    strncpy(OutAffil, Work3, sizeof(OutAffil));
		} else if (Work4[0] != '\0') {
		    strncpy(OutAffil, Work4, sizeof(OutAffil));
		} else strncpy(OutAffil, InOrgname, sizeof(OutAffil));
		if (Work5[0] != '\0') {
		    strncpy(OutOtherInfo, Work5, sizeof(OutOtherInfo));
		} else if (Work6[0] != '\0') {
		    strncpy(OutOtherInfo, Work6, sizeof(OutOtherInfo));
		}
	    }
	    WriteFields();
	} else {
	    OutPersName[0] = OutAddress[0] = OutAffil[0] = OutOtherInfo[0] = '\0';
	    if (GetAddress(From, Work1) > 0 && Work1[0] != '\0') strncpy(OutAddress, Work1, sizeof(OutAddress));
	    if (GetPersEffects(From, Work1, Work3, Work5) > 0) {
		if (Work1[0] != '\0') strncpy(OutPersName, Work1, sizeof(OutPersName));
		if (Work3[0] != '\0') strncpy(OutAffil, Work3, sizeof(OutAffil));
		if (Work5[0] != '\0') strncpy(OutOtherInfo, Work5, sizeof(OutOtherInfo));
	    }
	    if (OutAffil[0] == '\0') strncpy(OutAffil, InOrgname, sizeof(OutAffil));
	    rc = InReply[0];
	    InReply[0] = '\0';
	    WriteFields();
	    InReply[0] = rc;

	    OutPersName[0] = OutAddress[0] = OutAffil[0] = OutOtherInfo[0] = '\0';
	    if (GetAddress(Reply, Work1) > 0 && Work1[0] != '\0') strncpy(OutAddress, Work1, sizeof(OutAddress));
	    if (GetPersEffects(Reply, Work1, Work3, Work5) > 0) {
		if (Work1[0] != '\0') strncpy(OutPersName, Work1, sizeof(OutPersName));
		if (Work3[0] != '\0') strncpy(OutAffil, Work3, sizeof(OutAffil));
		if (Work5[0] != '\0') strncpy(OutOtherInfo, Work5, sizeof(OutOtherInfo));
	    }
	    if (OutAffil[0] == '\0') strncpy(OutAffil, InOrgname, sizeof(OutAffil));
	    fc = InFrom[0];
	    InFrom[0] = '\0';
	    WriteFields();
	    InFrom[0] = fc;
	}
    } else if (FromHead != NULL) {
	if ((AddrOverride || OutAddress[0] == '\0') && GetAddress(From, Work1) > 0)
	    strncpy(OutAddress, Work1, sizeof(OutAddress));
	if ((PersOverride || OutPersName[0] == '\0') && GetPersEffects(From, Work1, Work3, Work5) > 0) {
	    strncpy(OutPersName, Work1, sizeof(OutPersName));
	    if (OutAffil[0] == '\0') strncpy(OutAffil, Work3, sizeof(OutAffil));
	    if (OutOtherInfo[0] == '\0') strncpy(OutOtherInfo, Work5, sizeof(OutOtherInfo));
	}
	if (OutAffil[0] == '\0') strncpy(OutAffil, InOrgname, sizeof(OutAffil));
	rc = InReply[0];
	if (!IdentifyFromReply) InReply[0] = '\0';
	WriteFields();
	InReply[0] = rc;
    } else if (ReplyHead != NULL) {
	if ((AddrOverride || OutAddress[0] == '\0') && GetAddress(Reply, Work1) > 0)
	    strncpy(OutAddress, Work1, sizeof(OutAddress));
	if ((PersOverride || OutPersName[0] == '\0') && GetPersEffects(Reply, Work1, Work3, Work5) > 0) {
	    strncpy(OutPersName, Work1, sizeof(OutPersName));
	    if (OutAffil[0] == '\0') strncpy(OutAffil, Work3, sizeof(OutAffil));
	    if (OutOtherInfo[0] == '\0') strncpy(OutOtherInfo, Work5, sizeof(OutOtherInfo));
	}
	if (OutAffil[0] == '\0') strncpy(OutAffil, InOrgname, sizeof(OutAffil));
	rc = InFrom[0];
	if (!IdentifyFromReply) InFrom[0] = '\0';
	WriteFields();
	InFrom[0] = rc;
    }
	
    if (FromHead != NULL) {
	FreeAddressList(FromHead);
    }
    if (ReplyHead != NULL) {
	FreeAddressList(ReplyHead);
    }
    return 0;
}

static void OpenOut()
{/* Open the output file as needed. */
    if (OutName == NULL) {
	outFile = stdout;
    } else {
	errno = 0;
	outFile = fopen(OutName, "w");
	if (outFile == NULL) {fprintf(stderr, "Can't open %s for writing: %s\n", OutName, UnixError(errno == 0 ? ENOMEM : errno)); exit(1);}
    }
}

static void CloseOut()
{/* Close the output file, if there is one. */
    int RC;
    if (OutName != NULL && outFile != NULL) {
	errno = 0;
	RC = fclose(outFile);
	outFile = NULL;
	if (RC != 0) {
	    fprintf(stderr, "Can't close output file %s: %s\n", OutName, UnixError(errno));
	    unlink(OutName);
	    exit(1);
	}
    }
}

static void Usage(arg0, prob)
char *arg0, *prob;
{/* Complain about a Usage error. */
    fputs("brisk: ", stderr);
    fputs(prob, stderr);
    fprintf(stderr, "\nusage: %s [-d][-h ndbmFile][-o outFile][-A(ddrOverride)][-P(ersOverride)][-I(dentity)] file file...\n", arg0);
    exit(2);
}

main(argc, argv)
int argc;
char *argv[];
{
    int RC;

    RC = 1;
    while (RC < argc && argv[RC][0] == '-') {
	    if (strcmp(argv[RC], "-d") == 0) {
		Debugging = 1;
	    } else if (strcmp(argv[RC], "-h") == 0) {
		++RC;
		if (RC < argc) ndbmFile = argv[RC];
		else Usage(argv[0], "Missing ndbmFile after -h");
	    } else if (strcmp(argv[RC], "-o") == 0) {
		++RC;
		if (RC < argc) OutName = argv[RC];
		else Usage(argv[0], "Missing name after -o");
	    } else if (strcmp(argv[RC], "-A") == 0) {
		AddrOverride = 1;
	    } else if (strcmp(argv[RC], "-P") == 0) {
		PersOverride = 1;
	    } else if (strcmp(argv[RC], "-I") == 0) {
		IdentifyFromReply = 1;
	    } else Usage(argv[0], "Bad option given");
	++RC;
    }
    if (RC < argc) {
	fNames = &argv[RC];
    } else {
	fNames = 0;
    }
    OpenIn(); OpenOut();
    for (;;) {
	RC = LoadFields();
	if (RC == 0) break;
	if (RC < 0) exit(1);
	if (Debugging) DumpFields();
	if (CanonAndWrite() != 0) exit(1);
    }
    CloseIn(); CloseOut();
    exit(0);
}
#else /* AMS_ENV */
main() {
    fprintf(stderr, "'brisk' fails because AMS_ENV was not defined when it was built.\n");
    exit(1);
}
#endif /* AMS_ENV */
