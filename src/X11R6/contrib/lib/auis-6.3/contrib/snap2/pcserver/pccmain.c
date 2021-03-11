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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pcserver/RCS/pccmain.c,v 2.6 1992/12/15 21:06:36 rr2b R6tape $";
#endif

/*
 *	PC Server - Test Client
 *	Access to the VICE File System for IBM PC/XT/ATs
 *
 *	(C)Copyright IBM Corporation, 1984, 1985
 *	Program Property of IBM
 *
 *	Version 2.0
 *	Based on original code written by Bob Sidebotham
 *	Developed for the Information Technology Center at
 *	Carnegie-Mellon University
 *
 */

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <snap.h>
#include <pcserver.h>

int Cid;

char *PCdate(), *PCtime();
int Open(), Close(), Read(), Write(), DirSearch(), RemoveFiles(), RenameFiles(), MkDir(), RmDir(), ChMod(), SpaceQuery(), Error(), Quit();
char *(Arg[20]); int nArgs;
struct command {
    char *name;
    int (*func)();
    int nargs;
} commands[] = {
    {"open", Open, 3},
    {"close", Close, 1},
    {"read", Read, 3},
    {"write", Write, 4},
    {"dirsearch", DirSearch, 3},
    {"removefiles", RemoveFiles, 1},
    {"renamefiles", RenameFiles, 2},
    {"mkdir", MkDir, 1},
    {"rmdir", RmDir, 1},
    {"chmod", ChMod, 2},
    {"spacequery", SpaceQuery, 0},
    {"quit", Quit, 0},
    0
};

char *ReturnCode(rc)
long rc;
{
    static char *codes[] = {"PCS_SUCCESS", "PCS_NOFILE", "PCS_NOPATH",
    "PCS_NOSPACE", "PCS_NOACCESS", "PCS_BADFILT",
    "PCS_BADINTEN", "PCS_NOHANDLE", "PCS_EOF",
    "PCS_NOMATCH", "PCS_NOTEMPTY", "PCS_BADMODE",
    "PCS_BADARG"};
#define NCODES ((sizeof codes) / (sizeof (char *)))
    static char *snapcodes[] = {"SNAP_NOMORECONVS",
    "SNAP_NOMEMORY", "SNAP_BADPARMS",
    "SNAP_NOMORERETRIES", "SNAP_XMITFAIL",
    "SNAP_INVALIDOP", "SNAP_INVALIDCID",
    "SNAP_BUFFERLIMIT", "SNAP_SOCKETERROR",
    "SNAP_NOINIT", "SNAP_NOAUTHENTICATION",
    "SNAP_NOREPLYDUE", "SNAP_SERVERREJECT",
    "SNAP_TIMEOUT", "SNAP_INVALIDACK",
    "SNAP_WRONGVERSION", "SNAP_SELECTFAILED",
    "SNAP_RESEND", "SNAP_SERVERDIED"};
#define NSCODES ((sizeof snapcodes) / (sizeof (char *)))
    if ((rc >= NCODES) || (rc<0 && (-rc+SNAP_NOMORECONVS >= NSCODES))) {
	static char line[50];
	sprintf(line, "Unknown error (%ld)", rc);
	return line;
    }
    if (rc >= 0)
	return codes[rc];
    else
	return snapcodes[-rc+SNAP_NOMORECONVS];
}

/* Moved from within Mode function to bypass compiler bug! LKR 5/15/86 */
static char *modes[] = {"PCS_NORMAL", "PCS_DIRECTORY", "PCS_HIDDEN",
"PCS_READONLY", 0};

char *Mode(mode)
long mode;
{
    static char result[100];
    register char **mp;
    int needplus = 0, m;

    *result = '\0';
    for (mp=modes, m=1; *mp; mp++, m<<=1)
	if (mode&m) {
	    if (needplus)
		strcat(result, "+");
	    strcat(result, *mp);
	    needplus = 1;
	}
    return result;
}

long Intent(intent)
char *intent;
{
    static char *intents[] = {"input", "output", "unspecified", 0};
    register char **p = intents;
    while (*p && strcmp(*p,intent) != 0)
	p++;
    if (!*p)
	printf("[CLIENT] Sending bogus intent value to server\n");
    return p-intents;
}

long Filter(filter)
char *filter;
{
    static struct {
	char *name;
	int value;
    } filters[] = {
	{"normal",      PCS_NORMAL},
	{"directory",   PCS_DIRECTORY},
	{"hidden",      PCS_HIDDEN},
	{"readonly",    PCS_READONLY},
	{0},
    };
    char *index();
    char *next;
    int returnvalue = 0;

    while(filter) {
	register char *i;
	register int f;
	if ((i = index(filter,'+')) != NULL) {
	    *i = '\0';
	    next = i+1;
	}
	else
	    next = 0;
	for (f = 0; ;f++) {
	    if (filters[f].name == 0) {
		printf("[CLIENT] Filter \"%s\" unrecognized - ignored.\n",
		       filter);
		break;
	    }
	    if (strcmp(filters[f].name,filter) == 0) {
		returnvalue += filters[f].value;
		break;
	    }
	}
	filter=next;
    }
    return returnvalue;
}

main(argc, argv)
int argc;
char **argv;
{
    SNAP_CPARMS bcparms, cparms;
    int i, rc;
    char *hostname, *user, *pw;

    if (argc < 2 || argc > 4) {
	puts("[CLIENT] Bad args");
	exit(-1);
    }
    hostname = argv[1];
    user = (argc >= 3 ? argv[2] : "larry");
    pw	 = (argc >= 4 ? argv[3] : "");

    bcparms.maxtime = 20;
    bcparms.timeout = 10;
    bcparms.encryptlevel = SNAP_NOENCRYPT;

    cparms.maxtime = 20;
    cparms.timeout = 1;
    cparms.encryptlevel = SNAP_NOENCRYPT;

    if ((rc = SNAP_ClientInit()) != SNAP_SUCCESS) {
	printf("[CLIENT] RC from ClientInit: %d\n", rc);
	exit(-1);
    }

    printf ("[CLIENT] Back from ClientInit\n");
    printf ("[CLIENT] Host: \"%s\", User: \"%s\", Pw: \"%s\"\n",
	     hostname,user,pw);

    if ((Cid = SNAP_BeginConv ("snap.guardian", hostname, "pcserver",
				user, pw, strlen(pw), 0, &bcparms)) < 0) {
	printf("[CLIENT] BeginConv failed, rc: %d\n", Cid);
	exit(-1);
    }

    printf ("[CLIENT] Back from BeginConv\n");
    SNAP_SetConvParms (Cid, &cparms);

    for (;;) {
	register struct command *cp = &commands[0];
	getArgs();
	if (nArgs-- == 0)
	    continue;
	while (cp->name) {
	    if (strcmp(Arg[0],cp->name) == 0)
		break;
	    cp++;
	}
	if (!cp->name) {
	    printf("[CLIENT] Unrecognized command\n");
	    continue;
	}
	if (cp->nargs != nArgs) {
	    printf("[CLIENT] %s command requires %d arguments\n",
		   cp->name, cp->nargs);
	    continue;
	}
	if ((*cp->func)() == -1)
	    break;
    }
    if ((rc = SNAP_EndConv (Cid,"", 0, NULL)) != SNAP_SUCCESS) {
	printf("[CLIENT] EndConv failed, - rc: %d\n", rc);
	exit(1);
    }
    exit(0);
}


Open()
{
    long rc, handle, mode, date, time, size;
    rc = PCS_Open (Cid, Arg[1], Intent(Arg[2]), Filter(Arg[3]),
		    &handle, &mode, &date, &time, &size);
    printf
      ("[CLIENT] RC=%s, handle=%ld, mode=%s, date=%s, time=%s size=%ld\n",
	ReturnCode(rc), handle, Mode(mode), PCdate(date), PCtime(time), size);
    return 0;
}

Close()
{
    long rc;
    rc = PCS_Close(Cid, (long) atoi(Arg[1]));
    printf("[CLIENT] RC=%s\n", ReturnCode(rc));
    return 0;
}

Read()
{
    long rc, return_length;
    char *data;
    char *FormatData();
    rc = PCS_Read (Cid, (long) atoi(Arg[1]), (long) atoi(Arg[2]),
		    (long) atoi(Arg[3]), &return_length, &data);
    printf
      ("[CLIENT] RC=%s, return length=%ld, returned data=\"%s\"\n",
	ReturnCode(rc), return_length, FormatData(data,(int) return_length));
    return 0;
}

Write()
{
    static char *WriteSamples[] = {"ABCDEFGHIJKLMNOPQRSTUVWXYZ",
    "012345678901234567890123456780",
    "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};
    int nWriteSamples = sizeof (WriteSamples) / sizeof (char *), sample_number;
    long templen, rc, return_length;
    char buffer[1000];
    char *data = buffer;
    char *sample;
    sample_number = atoi(Arg[4])-1;
    if (sample_number >= nWriteSamples) {
	printf ("[CLIENT] %d is the biggest sample number available.\n",
		nWriteSamples);
	return 0;
    }
    sample = (sample_number == -1 ? Arg[4] : WriteSamples[sample_number]);
    templen = (long) atoi(Arg[3]);
    if (templen > strlen(sample)) {
	templen = strlen (sample);
	printf ("[CLIENT] Length truncated to %ld\n", templen);
    }
    strncpy(data, sample, templen);
    rc = PCS_Write (Cid, (long) atoi(Arg[1]), (long) atoi(Arg[2]), templen,
		     data, &return_length);
    printf ("[CLIENT] RC=%s, return length=%ld\n",
	     ReturnCode(rc), return_length);
    return 0;
}

DirSearch()
{
    long rc, mode, date, time, size;
    char *match;
    rc = PCS_DirSearch (Cid, Arg[1], Filter(Arg[2]), Arg[3],
			 &match, &mode, &date, &time, &size);
    printf("[CLIENT] RC=%s", ReturnCode(rc));
    if (rc == PCS_SUCCESS)
	printf(", first match=%s, mode=%s, date=%s, time=%s, size=%ld\n",
	       match, Mode(mode), PCdate(date), PCtime(time), size);
    printf("\n");
    return 0;
}

RemoveFiles()
{
    long rc;
    rc = PCS_RemoveFiles(Cid, Arg[1]);
    printf("[CLIENT] RC=%s\n", ReturnCode(rc));
    return 0;
}

RenameFiles()
{
    long rc;
    rc = PCS_RenameFiles(Cid, Arg[1], Arg[2]);
    printf("[CLIENT] RC=%s\n", ReturnCode(rc));
    return 0;
}

MkDir()
{
    long rc;
    rc = PCS_MkDir(Cid, Arg[1]);
    printf("[CLIENT] RC=%s\n", ReturnCode(rc));
    return 0;
}

RmDir()
{
    long rc;
    rc = PCS_RmDir(Cid, Arg[1]);
    printf("[CLIENT] RC=%s\n", ReturnCode(rc));
    return 0;
}

ChMod()
{
    long rc;
    rc = PCS_ChMod(Cid, Arg[1], Filter(Arg[2]));
    printf("[CLIENT] RC=%s\n", ReturnCode(rc));
    return 0;
}

SpaceQuery()
{
    long rc;
    long Total, Remaining, SectorsPerAU, BytesPerSector;
    PCS_SpaceQuery(Cid, &Total, &Remaining, &SectorsPerAU, &BytesPerSector);
    printf
      ("[CLIENT] Total=%ld, Remaining=%ld, SectorsPerAU=%ld, BytesPerSector=%ld\n",
	Total, Remaining, SectorsPerAU, BytesPerSector);
    return 0;
}

Quit()
{
    return -1;
}

getArgs()
{
    char   *l, *b;
    static char line[500];
    char eol;
    printf ("[CLIENT] * ");
    nArgs = 0;
    if (gets (line) == NULL)
	strcpy(line,"quit");
    for (l = b = line, eol = 0; !eol;) {
	if ((eol = (*l == 0)) || isspace (*l)) {
	    *l++ = '\0';
	    Arg[nArgs] = b;
	    if (strcmp(Arg[nArgs], "\"\"") == 0)
		Arg[nArgs] = "";
	    nArgs++;
	    if (eol)
		break;
	    while (isspace (*l))
		l++;
	    b = l;
	}
	else
	    l++;
    }
}

char *FormatData(data,n)
char *data;
int n;
{
    static char hex[] = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
    'a', 'b', 'c', 'd', 'e', 'f'};
    static char result[4096];
    register char *r = result;
    register char *p = data;
    while (n--) {
	if (isprint(*p))
	    *r++ = *p;
	else {
	    *r++ = '[';
	    *r++ = hex[(*p>>4) & 0xf];
	    *r++ = hex[*p & 0xf];
	    *r++ = ']';
	}
	p++;
    }
    *r++ = 0;
    return result;
}

char *PCdate(date)
long date;
{
    static char datestring[10];
    sprintf (datestring, "%02ld/%02ld/%02ld",
	      (date>>9)+1980-1900, (date>>5)&0xf, date&0x1f);
    return datestring;
}

char *PCtime(time)
long time;
{
    static char timestring[10];
    sprintf (timestring, "%02ld:%02ld:%02ld",
	      time>>11, (time>>5)&0x3f, (time&0x1f)<<1);
    return timestring;
}
