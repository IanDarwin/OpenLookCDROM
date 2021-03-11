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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/tokunpak.c,v 2.27 1993/05/04 00:53:32 susan Exp $";
#endif

/*
		tokunpak.c -- Subroutines for unpacking Venus tokens
			from datagrams and setting them
			as current with Venus.
*/

 

#include <andrewos.h>		/* sys/types.h sys/time.h */
#include <stdio.h>
#include <netinet/in.h>
#include <svcconf.h>
#include <pwd.h>
#include <util.h>

#ifdef AFS_ENV
#include <afs/param.h>
#include <rx/xdr.h>
#include <afs/afsint.h>
#include <afs/auth.h>
#include <tokens.h>
#include <afs/cellconfig.h>
#define KEYSIZE	(sizeof(auth_EncryptionKey))
#include <errno.h>
#include <ctype.h>
#define MAXPackedTicket_Len (11*sizeof(unsigned long) + sizeof(struct ktc_encryptionKey) + MAXKTCTICKETLEN + (2 * sizeof(struct ktc_principal)) + 8*sizeof(unsigned long))
#endif /* AFS_ENV */

#define NIL 0
extern int errno;
#ifndef _IBMR2
extern char *malloc(), *realloc();
#endif /* _IBMR2 */

#ifdef AFS_ENV
static int IsKTC(where)
char *where;
{/* Return TRUE if this is a packed KTC ticket or FALSE otherwise. */
    long int Dum0, Dum9, Dum10;

    Dum0 = ntohl(* (long *) where);
    where += 9 * sizeof(long int);
    Dum9 = ntohl(* (long *) where);
    where += sizeof(long int);
    Dum10 = ntohl(* (long *) where);
    return (Dum0 == 0 && Dum9 == -1 && Dum10 == -1);
}

static int UnpackKTC(tokens, atok, aserv, acli, debug, pPrimFlag)
char *tokens;
struct ktc_token *atok;
struct ktc_principal *aserv, *acli;
int debug, *pPrimFlag;
{
    register char *p;
    long int Dum;

    p = tokens;
    Dum = * (long *) p; /* 0/0 zero */
    p += sizeof(long int);
    if (Dum != 0) return -1;
    atok->startTime = ntohl(* (long *) p); /* 1/4 start time */
    p += sizeof(long int);
    atok->endTime = ntohl(* (long *) p); /* 2/8 end time */
    p += sizeof(long int);
    Dum = ntohl(* (long *) p); /* 3/12 KVNo */
    p += sizeof(long int);
    atok->kvno = (short) Dum;
    if (atok->kvno != Dum) return -1;
    atok->ticketLen = ntohl(* (long *) p); /* 4/16 tkt len */
    p += sizeof(long int);
    Dum = ntohl(* (long *) p); /* 5/20 zero */
    p += sizeof(long int);
    if (Dum != 0) return -1;
    Dum = ntohl(* (long *) p); /* 6/24 zero */
    p += sizeof(long int);
    if (Dum != 0) return -1;
    Dum = ntohl(* (long *) p); /* 7/28 prim/amshome flag */
    p += sizeof(long int);
    if (pPrimFlag != NULL) *pPrimFlag = Dum;
    Dum = ntohl(* (long *) p); /* 8/32 session key size */
    if (Dum != sizeof(atok->sessionKey.data)) return -1;
    p += sizeof(long int);
    Dum = ntohl(* (long *) p); /* 9/36 flag -1 */
    p += sizeof(long int);
    if (Dum != -1) return -1;
    Dum = ntohl(* (long *) p); /* 10/40 flag -1 */
    p += sizeof(long int);
    if (Dum != -1) return -1;
    bcopy(p, atok->sessionKey.data, sizeof(atok->sessionKey.data));
    p += sizeof(atok->sessionKey.data);
    if (atok->ticketLen > 0) bcopy(p, atok->ticket, atok->ticketLen);
    p += atok->ticketLen;
    Dum = strlen(p) + 1;
    strncpy(aserv->name, p, sizeof(aserv->name));
    p += Dum;
    Dum = strlen(p) + 1;
    strncpy(aserv->instance, p, sizeof(aserv->instance));
    p += Dum;
    Dum = strlen(p) + 1;
    strncpy(aserv->cell, p, sizeof(aserv->cell));
    p += Dum;
    Dum = strlen(p) + 1;
    strncpy(acli->name, p, sizeof(acli->name));
    p += Dum;
    Dum = strlen(p) + 1;
    strncpy(acli->instance, p, sizeof(acli->instance));
    p += Dum;
    Dum = strlen(p) + 1;
    strncpy(acli->cell, p, sizeof(acli->cell));
    p += Dum;

#ifdef DEBUG
    if (debug) {
	int i;
	char *T;

	fprintf(stderr, "Packed tokens (%d long): 0x", (p - tokens));
	for (T = tokens; T < p; ++T) fprintf(stderr, "%02x", (unsigned char) *T);
	fprintf(stderr, "\nUnpacked into:\n---Server: ``%s''/``%s''/``%s''\n", aserv->name, aserv->instance, aserv->cell);
	fprintf(stderr, "---Client: ``%s''/``%s''/``%s''\n", acli->name, acli->instance, acli->cell);
	fprintf(stderr, "---Token (kvno %d): 0x", atok->kvno);
	T = (char *) atok;
	for (i=0; i<(sizeof(struct ktc_token) - (MAXKTCTICKETLEN - atok->ticketLen)); ++i)
	    fprintf(stderr, "%02x", (unsigned char) *T++);
	fputc('\n', stderr);
    }
#endif /* DEBUG */
    while ( ((p - tokens) % sizeof(unsigned long)) != 0) ++p;
    return (p - tokens);
}
#endif /* AFS_ENV */

int unpacktokens(tokens, ctoken, stoken, debug, set)
{    return 0; }

#if defined(AMS_DELIVERY_ENV) || defined(AFS_ENV)
int tok_GetStr(pRead, EndP, outStr, sizeOutStr)
char **pRead, *EndP, *outStr; int sizeOutStr;
{/* Get a null-terminated string from *pRead, putting it into outStr. */
    int CharsLeft;
    char *Out, *In;

    CharsLeft = sizeOutStr - 1;
    Out = outStr;
    for (In = *pRead; In < EndP && *In != '\0' && CharsLeft > 0; ++In, --CharsLeft) {
	*Out++ = *In;
    }
    if (CharsLeft > 0) *Out++ = '\0';
    if (CharsLeft == 0 || In == EndP) return 0;	/* If counts ran out, there was an error. */
    else {*pRead = In; return 1;}
}
#endif /* defined(AMS_DELIVERY_ENV) || defined(AFS_ENV) */

static int GenAuths(pWhere, pWhereLen, srvP, cliP, tokP, begdP, expdP, vidP, cell, vname, primP, locP, debug)
#ifdef AFS_ENV
char **pWhere;
int *pWhereLen;
struct ktc_principal *srvP, *cliP; struct ktc_token *tokP;
unsigned long int *begdP, *expdP; int *vidP;
char *cell, *vname;
int *primP, *locP;
int debug;
{/* Read the next authentication from the sequence of them that starts at *pWhere, and return it.  Return >0 if successful, =0 if all done, and <0 on any error.
    After a successful return, srvP, cliP, and tokP will be loaded with ktc info or ctokP and stokP will be loaded with tokens, begdP and expdP will have the beginning and ending times, vidP (if >= 0, else vname) and cell the authentication (where cell is of size at least MAXCELLCHARS), primP and locP the indication of whether the auth is ``primary'' and/or workstation-local.
*/
    char *Read, *EndP, *Ck;
    char StrCellName[1+MAXCELLCHARS+1];
    int IsPrimary, IsLocal, CharFlag, Len;
    int PrimFlag;

    Read = *pWhere;
    EndP = &Read[*pWhereLen];
    if ((Read + (11*sizeof(unsigned long int))) > EndP) return 0;	/* Finished */
    if (IsKTC(Read)) {
	Len = UnpackKTC(Read, tokP, srvP, cliP, debug, &PrimFlag);
	if (Len <= 0) return -1;
	Read += Len;
	if (strcmp(srvP->cell, cliP->cell) != 0) return -1;
	*begdP = tokP->startTime;
	*expdP = tokP->endTime;
	*vidP = -1;
	strcpy(vname, cliP->name);
	strncpy(cell, srvP->cell, MAXCELLCHARS);
    } else {
	return -1;
    }
    *pWhereLen -= (Read - *pWhere);	/* Advance the generator. */
    *pWhere = Read;
    IsPrimary = IsLocal = 0;
    switch (PrimFlag) {
	case TokLocalPrimary:
	    IsLocal = IsPrimary = 1; break;
	case TokLocalNotPrimary:
	    IsLocal = 1; break;
	case TokIsPrimary:
	    IsPrimary = 1; break;
	case TokNotPrimary:
	    break;
	default:
	    return -1;
    }
    if (cell[0] == '\0') return -1;
    CharFlag = 0;
    for (Ck = cell; *Ck != '\0'; ++Ck)
	if (!isascii(*Ck)) CharFlag = 1;
	else if (!isalnum(*Ck) && *Ck != '.' && *Ck != '-') CharFlag = 1;
    if (CharFlag != 0) return -1;
    *primP = IsPrimary;
    *locP = IsLocal;
    return 1;
}
#else /* AFS_ENV */
{    return 0; }
#endif /* AFS_ENV */

int tok_GenAuths(pWhere, pWhereLen, begdP, expdP, vidP, cell, vname, primP, locP, debug)
#ifdef AFS_ENV
char **pWhere;
int *pWhereLen;
unsigned long int *begdP, *expdP; int *vidP;
char *cell, *vname;
int *primP, *locP;
int debug;
{/* exported: generate the information for clients, but not the ticket bits themselves. */
    struct ktc_principal Srv, xCli; struct ktc_token Token;

    return(GenAuths(pWhere, pWhereLen, &Srv, &xCli, &Token, begdP, expdP, vidP, cell, vname, primP, locP, debug));
}
#else /* AFS_ENV */
{    return 0; }
#endif /* AFS_ENV */

int GenTokens(pWhere, pWhereLen, expdP, vidP, cell, primP, locP, debug)
#ifdef AFS_ENV
char **pWhere;
int *pWhereLen;
unsigned long int *expdP; int *vidP;
char *cell;
int *primP, *locP;
int debug;
{/* Read the next authentication from the sequence of them that starts at *pWhere, and return it.  Return >0 if successful, =0 if all done, and <0 on any error.
    After a successful return, ctokP and stokP will be loaded with tokens, expdP will have the expiration date, vidP and cell the authentication (where cell is of size at least MAXCELLCHARS), primP and locP the indication of whether the auth is ``primary'' and/or workstation-local.
*/
    unsigned long int Beg;
    struct ktc_principal Serv, xCli; struct ktc_token KTok;
    char vname[MAXCELLCHARS+1];

    return GenAuths(pWhere, pWhereLen, &Serv, &xCli, &KTok, &Beg, expdP, vidP, cell, vname, primP, locP, debug);
}
#else /* AFS_ENV */
{    return 0; }
#endif /* AFS_ENV */

int UnpackAndSetTokens(Where, WhereLen, debug, setPag)
#ifdef AFS_ENV
char *Where;
int WhereLen, debug, setPag;
{/* Unpack the result of the given Where string, up to its length, and set those tokens in those cells. */
    struct ktc_principal Server, Client; struct ktc_token KToken;
    char *Read;
    char StrCellName[1+MAXCELLCHARS+1];
    char VName[MAXCELLCHARS+1];
    int IsPrimary, TokIx, IsLocal, wantSetPag, Remaining, VID;
    unsigned long int Now, Begd, Expd;

    wantSetPag = setPag;
    Now = osi_GetSecs() + 15;
    TokIx = 0;
    Read = Where; Remaining = WhereLen;
    while (GenAuths(&Read, &Remaining, &Server, &Client, &KToken, &Begd, &Expd, &VID, StrCellName, VName, &IsPrimary, &IsLocal, debug) > 0) {
#ifdef DEBUG
	if (debug) {
	    char *p;
	    int i;

	    fprintf(stderr, "About to set %s %s tokens, exp [%d..%d], for %d/``%s'' in cell ``%s''.\n",
		    (IsPrimary ? "primary" : "non-primary"),
		    (IsLocal ? "local" : "non-local"),
		    Begd, Expd, VID, VName, StrCellName);
	    if (VID >= 0) {
		p = (char *) &ctoken;
		fputs("---Clear token: 0x", stderr);
		for (i=0; i<sizeof(ClearToken); ++i)
		    fprintf(stderr, "%02x", (unsigned char) *p++);
		fputs("\n---Secret token: 0x", stderr);
		p = (char *) &stoken;
		for (i=0; i<sizeof(SecretToken); ++i)
		    fprintf(stderr, "%02x", (unsigned char) *p++);
		fputc('\n', stderr);
	    } else {
		fprintf(stderr, "---Server: %s/%s/%s\n", Server.name, Server.instance, Server.cell);
		fprintf(stderr, "---Client: %s/%s/%s\n", Client.name, Client.instance, Client.cell);
		p = (char *) &KToken;
		fputs("---Token: 0x", stderr);
		for (i=0; i<KToken.ticketLen; ++i)
		    fprintf(stderr, "%02x", (unsigned char) *p++);
		fprintf(stderr, "\n---kvno: %d\n", KToken.kvno);
	    }
	}
#endif /* DEBUG */
	/* OK: let's try this one. */
	if (IsLocal) {
	    if (ULstrcmp(WorkstationName, StrCellName) == 0) {
		if (Now < Begd || Expd < Now) return -1;
	    }
	} else {
	    if (VID >= 0) {
		return -1;
	    } else {
		if (wantSetPag) {
		    setpag();
		    wantSetPag = 0;
		}
		if (ktc_SetToken(&Server, &KToken, &Client) != 0) {
		    if (Begd < Now && Now < Expd)
			return -1;	/* Times ok: must be some real problem. */
		}
	    }
	    ++TokIx;
	}
    }
    return TokIx;	/* number of (afs) tokens successfully set */
}
#else /* AFS_ENV */
{    return 0; }
#endif /* AFS_ENV */

int ExtractCellID(Where, WhereLen, cellName, debug)
#ifdef AFS_ENV
char *Where, *cellName;
int WhereLen, debug;
{/* Unpack the tokens at Where (for WhereLen), looking for a token for cell cellName.  If one is found, return the ViceId for that cell. */
    char *Read, *SP;
    char StrCellName[1+MAXCELLCHARS+1];
    struct ktc_principal Server, Client; struct ktc_token KToken;
    char VName[MAXCELLCHARS+1];
    int wantLocal, haveLocal, IsPrimary, Remaining, VID;
    unsigned long int Begd, Expd;
    struct passwd *PW;

    wantLocal = 0;
    if (ULstrcmp(cellName, "localhost") == 0) wantLocal = 1;
    else if (cellName[0] == '&' && ULstrcmp(&cellName[1], WorkstationName) == 0) wantLocal = 1;
    Read = Where;
    Remaining = WhereLen;
    while (GenAuths(&Read, &Remaining, &Server, &Client, &KToken, &Begd, &Expd, &VID, StrCellName, VName, &IsPrimary, &haveLocal, debug) > 0) {
#ifdef DEBUG
	if (debug) {
	    fprintf(stderr, "Token in cell %s: VID %d, vname %s.\n", StrCellName, VID, VName);
	}
#endif /* DEBUG */
	/* Now check the value. */
	if (wantLocal) {
	    if (haveLocal
		&& (cellName[0] != '&' || ULstrcmp(StrCellName, WorkstationName) == 0)) {
		if (VID >= 0) return VID;
		SP = NULL;
		if (strncmp(VName, "Unix UID ", 9) == 0) SP = &VName[9];
		if (SP != NULL) return(atoi(SP));
		PW = getpwnam(VName);
		if (PW != NULL) return PW->pw_uid;
		return -1;
	    }
	} else {
	    if (ULstrcmp(cellName, StrCellName) == 0) {
		if (VID >= 0) return VID;
		SP = NULL;
		if (strncmp(VName, "AFS ID ", 7) == 0) SP = &VName[7];
		else if (strncmp(VName, "ViceID=", 7) == 0) SP = &VName[7];
		else if (strncmp(VName, "Unix UID ", 9) == 0) SP = &VName[9];
		else if (strncmp(VName, "(for ", 5) == 0) SP = &VName[5];
		if (SP != NULL) return(atoi(SP));
		PW = getcpwnam(VName, cellName);
		if (PW != NULL) return PW->pw_uid;
		return -1;
	    }
	}
    }
    return -1;	/* Didn't find a match. */
}
#else /* AFS_ENV */
{    return -1; }
#endif /* AFS_ENV */

#ifdef TESTINGONLYTESTING
main()
{
    char *BigPacket = NULL;
    int BigLen, BigMax, RC, ix;
    static char *cellNames[] = {
	"cs.cmu.edu",
	"andrew.cmu.edu",
	"ri.osf.org",
	"psy.cmu.edu",
	"athena.mit.edu",
    };

    RC = GetAndPackAllTokens(&BigPacket, &BigLen, &BigMax, 1);
    fprintf(stderr, "GetAndPackAllTokens returns %d.\n", RC);
    if (RC >= 0) {
	RC = UnpackAndSetTokens(BigPacket, BigLen, 1, 0);
	fprintf(stderr, "UnpackAndSetTokens returns %d.\n", RC);
	CheckServiceConfiguration();
	RC = ExtractCellID(BigPacket, BigLen, WorkstationName, 1);
	fprintf(stderr, "ExtractCellID(\"%s\") returns %d.\n", WorkstationName, RC);
	RC = ExtractCellID(BigPacket, BigLen, ThisDomain, 1);
	fprintf(stderr, "ExtractCellID(\"%s\") returns %d.\n", ThisDomain, RC);
	for (ix = 0; ix < (sizeof(cellNames)/sizeof(cellNames[0])); ++ix) {
	    RC = ExtractCellID(BigPacket, BigLen, cellNames[ix], 0);
	    if (RC >= 0) fprintf(stderr, "ExtractCellID(\"%s\") returns %d.\n",
				 cellNames[ix], RC);
	}
    }
    exit(0);
}
#endif /* TESTINGONLYTESTING */
