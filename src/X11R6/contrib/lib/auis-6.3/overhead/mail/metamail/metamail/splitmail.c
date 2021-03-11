/*
Copyright (c) 1991 Bell Communications Research, Inc. (Bellcore)

Permission to use, copy, modify, and distribute this material 
for any purpose and without fee is hereby granted, provided 
that the above copyright notice and this permission notice 
appear in all copies, and that the name of Bellcore not be 
used in advertising or publicity pertaining to this 
material without the specific, prior written permission 
of an authorized representative of Bellcore.  BELLCORE 
MAKES NO REPRESENTATIONS ABOUT THE ACCURACY OR SUITABILITY 
OF THIS MATERIAL FOR ANY PURPOSE.  IT IS PROVIDED "AS IS", 
WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES.
*/
/****************************************************** 
    Metamail -- A tool to help diverse mail readers 
                cope with diverse multimedia mail formats.

    Author:  Nathaniel S. Borenstein, Bellcore

 ******************************************************* */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <config.h>
#include <ctype.h>
#ifndef vax
#include <time.h>
#endif
#include <andrewos.h>

#define MINCHUNKSIZE 20000 /* Better be enough to hold the headers, or we die! */
extern char *getmyname();

#ifdef AMIGA
#define Prototype   extern

#include <getfiles.h>
#include <time.h>
#include <lib_protos.h>

#define NORMALDELIVERYCMD NormalDeliveryCmd
#define VERBOSEDELIVERYCMD VerboseDeliveryCmd
#else
extern char *getenv();
#define NORMALDELIVERYCMD "/usr/lib/sendmail -t -oi"
#define VERBOSEDELIVERYCMD "/usr/lib/sendmail -t -v -oi"
#endif

usageexit() {
    fprintf(stderr, "Usage:  splitmail [-d] [-v] [-s splitsize] [-i id-suffix] [-p prefix] [file-name]\n");
    exit(-1);
}

char *MonthNames[] = {"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"};
char *DayNames[] = {"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"};

char *
endofheader(s)
char *s;
{
    char *orgs = s, c;
    while (1) {
	s = index(s, '\n');
	if (!s) return(orgs+strlen(orgs));
	c = *(s+1);
        if (c != ' ' && c != '\t') return(s);
        ++s;
    }
}

main(argc, argv)
char **argv;
{
    int i, DoDeliver=0, SplitSize=DEFAULT_SPLIT_SIZE, dum, InNewline=1, bytesread, whichpart=1, Verbose=0, numparts = -1, c;
    char *fname = NULL, *bigbuf, *s, *SharedHeaders, *headend, *from, id[100], *deliverycmd, *prefix, SubjectBuf[250];
    char *MessageID = 0;
    FILE *fp;
#ifdef AMIGA
    char *NormalDeliveryCmd;
    char VerboseDeliveryCmd[100];
#endif    

    s = getenv("METAMAIL_TMPDIR");
    if (s) {
        prefix = malloc(10+strlen(s));
        if (!prefix) {
            fprintf(stderr, "splitmail: Not enough memory\n");
            exit(-1);
        }
        sprintf(prefix, "%s/split.", s);
    } else {
#ifdef MSDOS
        prefix = "./split.";
#else
        prefix = "/tmp/split.";
#endif /* MSDOS */
    }
    s = getenv("SPLITSIZE");
    if (s) {
        dum = atoi(s);
        if (dum < MINCHUNKSIZE) {
            fprintf(stderr, "Ignoring SPLITSIZE environment variable of %d -- the minimum value is %d\n", dum, MINCHUNKSIZE);
        } else {
            SplitSize = dum;
        }
    }
#ifdef AMIGA
    NormalDeliveryCmd = GetConfigProgram("Sendmail");
    strcpy(VerboseDeliveryCmd, NormalDeliveryCmd);
    strcat(VerboseDeliveryCmd, "-v");
#endif
    for (i=1; i<argc; ++i) {
	if (argv[i][0] == '-') {
	    switch (argv[i][1]) {
		case 's':
		    if (++i >= argc) usageexit();
		    dum = atoi(argv[i]);
		    if (dum < MINCHUNKSIZE && dum >= 0) {
			fprintf(stderr, "splitmail: Using minimum splitsize of %d\n", MINCHUNKSIZE);
			dum = MINCHUNKSIZE;
			
		    }
		    SplitSize = dum;
		    break;
		case 'd':
		    DoDeliver = 1;
                    break;
                case 'p':
		    if (++i >= argc) usageexit();
                    prefix = argv[i];
                    break;
 		case 'i':
 		    if( ++i >= argc) usageexit();
 		    MessageID = argv[i];
 		    break;
		case 'v':
		    Verbose = 1;
		    break;
		default:
		    usageexit();
	    }
	} else {
	    if (fname) usageexit();
	    fname = argv[i];
	}
    }
    bigbuf = malloc(100+SplitSize);
    if (!bigbuf) {
	fprintf(stderr, "splitmail:  Not enough memory for %d-byte chunks\n", SplitSize);
	exit(-1);
    }
    SplitSize -= 1000; /* gives fudge factor for headers, line endings */
    if (fname) {
        struct stat stbuf;
        if (!stat(fname, &stbuf)) {
            /* Note:  this will sometimes be 1 too high when it is a very close call,
              because of the desire to have complete lines.  In such cases, we send
              a null final part */
            numparts = 1 + (stbuf.st_size / SplitSize);
        }
	fp = fopen(fname, "r");
	if (!fp) {
	    fprintf(stderr, "splitmail: Cannot read file %s\n", fname);
	    exit(-1);
	}
    } else fp = stdin;
    headend = bigbuf;
    while((c=getc(fp)) != EOF) {
        if (headend >= bigbuf + SplitSize) {
            fprintf(stderr, "splitmail: Could not find the end of the headers!\n");
            exit(-1);
        }
	*headend++ = c;
	if (c == '\n') {
	    if (InNewline) break; /* end of headers */
	    InNewline = 1;
	} else {
	    InNewline = 0;
	}
    }
    if (c == EOF) {
	fprintf(stderr, "splitmail: Could not find the end of the headers!\n");
	exit(-1);
    }
    *headend = '\0';
    SharedHeaders = malloc(1+strlen(bigbuf)); /* maximum size needed */
    if (!SharedHeaders) {
	fprintf(stderr, "splitmail: Not enough memory\n");
	exit(-1);
    }
    from = bigbuf;
    *SharedHeaders = '\0';
    strcpy(SubjectBuf, "Partial Message");
    while (from < headend) {
	s = endofheader(from);  /* would be index(from, '\n'),
				 but need to check for continuation lines */
        *s = '\0';
	if (ShareThisHeader(from, SubjectBuf)) {
	    strcat(SharedHeaders, from);
	    strcat(SharedHeaders, "\n");
	}
	*s = '\n';
	from = ++s;
    }
#ifdef AMIGA
    sprintf(id, "%d.%s@%s%s", time(0), SeqToName(GetSequence(4)), FindConfig("NodeName"),
            FindConfig("DomainName"));
#else
#ifdef MSDOS
    sprintf(id, "%ld.UNKNOWN.SITE.NAME", time(NULL)); // BCR
#else
    sprintf(id, "%d.%d.%d.%s", (int) getuid(), (int) getpid(), (int) time(0), getmyname());
#endif /* MSDOS */
#endif /* AMIGA */
    bytesread = headend - bigbuf;
    deliverycmd = Verbose ? VERBOSEDELIVERYCMD : NORMALDELIVERYCMD;
    while (!feof(fp)) {
	while (SplitSize > bytesread && !feof(fp)) {
	    /* Need to loop because fread is weird */
	    bytesread += fread(bigbuf + bytesread, sizeof(char), SplitSize - bytesread, fp);
	}
	/* Now complete the line */
	while((c=getc(fp)) != EOF) {
	    bigbuf[bytesread++] = c;
	    if (c=='\n') break;
	}
        bigbuf[bytesread] = '\0';
        if (feof(fp) && numparts <= 0) numparts = whichpart;
        HandleOnePart(DoDeliver, deliverycmd, prefix, numparts, whichpart, SharedHeaders, SubjectBuf, id, MessageID, bigbuf);
	bytesread = 0;
	++whichpart;
    }
    while (whichpart <= numparts) {
        /* Our guess as to how many parts was OFF, hopefully only by one */
        *bigbuf = '\0'; /* have to deliver an empty part, ugh! */
        HandleOnePart(DoDeliver, deliverycmd, prefix, numparts, whichpart, SharedHeaders, SubjectBuf, id, MessageID, bigbuf);
        ++whichpart;
    }
    return(0);
}

HandleOnePart(DoDeliver, deliverycmd, prefix, numparts, whichpart, SharedHeaders, SubjectBuf, id, MessageID, bigbuf)
int DoDeliver, numparts, whichpart;
char *deliverycmd, *prefix, *SharedHeaders, *SubjectBuf, *id, *MessageID, *bigbuf;
{
    FILE *fpout;
    char OutputFile[1000];
    int code;

    if (DoDeliver) {
        fpout = popen(deliverycmd, "w");
    } else {
        sprintf(OutputFile, "%s%d", prefix, whichpart);
        fpout = fopen(OutputFile, "w");
    }
    if (!fpout) {
        fprintf(stderr, "splitmail: Can't open %s for writing\n", deliverycmd);
        exit(-1);
    }
    if (numparts != 1) { /* one-parters end up not changed at all! */
        struct tm *gt;
        time_t clock;
        int secsoff, hrsoff, minsoff;
        char signoff='+';

        fputs(SharedHeaders, fpout);
#if defined(SYSV) || defined(AIX)
        clock=time(0);
        gt = gmtime(&clock);
        hrsoff=minsoff=0;
        fprintf(fpout, "Date: %s, %02d %s %d %02d:%02d:%02d %c%02d%02d (GMT)\n", DayNames[gt->tm_wday],
                 gt->tm_mday, MonthNames[gt->tm_mon], 1900+gt->tm_year, gt->tm_hour,
                 gt->tm_min, gt->tm_sec, signoff, hrsoff, minsoff);
#else
        clock=time(0);
        gt = localtime(&clock);
#ifdef MSDOS 
        /* BCR -- THE tzname USED BELOW MAY NOT BE USED IN MICROSOFT C! */
        fprintf(fpout, "Date: %s, %02d %s %d %02d:%02d:%02d %s\n", DayNames[gt->tm_wday],
                 gt->tm_mday, MonthNames[gt->tm_mon], 1900+gt->tm_year, gt->tm_hour,
                 gt->tm_min, gt->tm_sec, tzname[(gt->tm_isdst) ? 1 : 0]);
#else
        secsoff= gt->tm_gmtoff;
        if (secsoff < 0) {
            signoff = '-';
            secsoff *= -1;
        }
        hrsoff = secsoff / 3600;
        minsoff = (secsoff - (3600*hrsoff)) / 60;
        fprintf(fpout, "Date: %s, %02d %s %d %02d:%02d:%02d %c%02d%02d (%s)\n", DayNames[gt->tm_wday],
                 gt->tm_mday, MonthNames[gt->tm_mon], 1900+gt->tm_year, gt->tm_hour,
                 gt->tm_min, gt->tm_sec, signoff, hrsoff, minsoff, gt->tm_zone);
#endif
#endif
        if( MessageID) {
            fprintf(fpout, "Message-Id: <%d.%s.%s>\n",whichpart,id,MessageID);
        }
        fprintf(fpout, "Subject: %s (part %d of ", SubjectBuf, whichpart);
        if (numparts > 0) {
            fprintf(fpout, "%d)\n", numparts);
        } else {
            fprintf(fpout, "several)\n");
        }
        fprintf(fpout, "Content-type: message/partial; id=\"%s\"; number=%d", id, whichpart);
        if (numparts > 0) fprintf(fpout, "; total=%d", numparts);
        fprintf(fpout, "\nMIME-Version: 1.0\n\n");
    }
    fputs(bigbuf, fpout);
    code = DoDeliver ? pclose(fpout) : fclose(fpout);
    if (code) {
        fprintf(stderr, "splitmail: %s of part %d failed\n", DoDeliver ? "Delivery" : "Writing", whichpart);
        if (whichpart > 1) fprintf(stderr, "  (previous %d parts may have succeeded)\n", whichpart -1);
        exit(-1);
    }
}

static char *SharedHeads[] = {
    "from",
    "to",
    "cc",
    "bcc",
    "newsgroup",
    "newsgroups",
    NULL
};

ShareThisHeader(s, SubjectBuf)
char *s;
char *SubjectBuf;
{
    int i;
    char *colon = index(s, ':');
    if (!colon) return(0); /* don't share it in all parts */
    *colon = '\0';
    if (!ULstrcmp(s, "subject")) {
        *colon = ':';
        strcpy(SubjectBuf, ++colon);
        return(0);
    }
    if (!ULstrcmp(s, "content-type")) {
        if (IllegalContentType(colon+1)) {
            fprintf(stderr, "splitmail: message has illegal content-type header, delivery cancelled.\n");
            exit(-1);
        }
    }
    for (i=0; SharedHeads[i]; ++i) {
	if (!ULstrcmp(s, SharedHeads[i])) break;
    }
    *colon = ':';
    return(SharedHeads[i] ? 1 : 0);
}

static char *tspecials = "()<>@,;:\\\"/[]?.=";

IllegalContentType(ctype)
char *ctype;
{
    char *ct, *semicolon, *st, *s, *param, *eq, *matcheq;
    ct = malloc(1+strlen(ctype));
    if (!ct) {
        fprintf(stderr, "splitmail: out of memory!\n");
        exit(-1);
    }
    strcpy(ct, ctype);
    semicolon = index(ct, ';');
    if (semicolon) *semicolon = 0;
    st = index(ct, '/');
    if (!st) {
        fprintf(stderr, "Illegal content-type specification: %s\nAll MIME content-type headers must contain a type/subtype specification.\n", ctype);
        return(-1);
    }
    if (st) *st++ = 0;
    for (s = ct; *s; ++s) {
        if (!isprint(*s) || index(tspecials, *s)) {
            fprintf(stderr, "Illegal content-type specification: %s\nThe character '%c' (ASCII %d) is illegal in a MIME content-type.\n", ctype, *s, *s);
            return(-1);
        }
    }
    for (s = st; s && *s; ++s) {
        if (!isprint(*s) || index(tspecials, *s)) {
            fprintf(stderr, "Illegal content-type specification: %s\nThe character '%c' (ASCII %d) is illegal in a MIME content-subtype.\n", ctype, *s, *s);
            return(-1);
        }
    }
    while (semicolon) {
        param = ++semicolon;
        while (*param && isspace(*param)) ++param;
        semicolon = index(param, ';');
        if (semicolon) *semicolon = 0;
        eq = index(param, '=');
        if (eq) *eq++ = 0;
        for (s = param; *s; ++s) {
            if (!isprint(*s) || index(tspecials, *s)) {
                fprintf(stderr, "Illegal content-type specification: %s\nThe character '%c' (ASCII %d) is illegal in a MIME content-type parameter name.\n", ctype, *s, *s);
                return(-1);
            }
        }
        while (*eq && isspace(*eq)) ++eq;
        if (*eq == '\"') {
            matcheq = eq;
            do {
                matcheq = index(matcheq+1, '\"');
                if (!matcheq) {
                    fprintf(stderr, "Illegal content-type specification: %s\nA quoted MIME parameter value must have matching quotation marks.\n", ctype);
                    return(-1);
                }
            } while (*(matcheq-1) == '\\');
            while (*++matcheq) {
                if (!isspace(*matcheq)) {
                    fprintf(stderr, "Illegal content-type specification: %s\nA quoted MIME parameter value must stop after the closing quotation mark.\n", ctype);
                    return(-1);
                }
            }
        } else {
            for (s = eq; s && *s; ++s) {
                if (!isprint(*s) || index(tspecials, *s)) {
                    fprintf(stderr, "Illegal content-type specification: %s\nThe character '%c' (ASCII %d) is illegal in an unquoted MIME parameter value.\n", ctype, *s, *s);
                    return(-1);
                }
            }
        }
    }
    free(ct);
    return(0);
}

int ULstrcmp(s1, s2)
register char *s1, *s2;
{
    char c1,c2;

    for(;;) {
	c1 = *s1++; if (c1 <= 'Z') if (c1 >= 'A') c1 += 040;
	c2 = *s2++; if (c2 <= 'Z') if (c2 >= 'A') c2 += 040;
	if (c1 != c2) break;
	if (c1 == '\0') return(0);
    }
    return(c1 - c2);
}

/* STILL TO DO:  
  Get number of parts right when possible 
*/
