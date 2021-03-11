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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/utils/purge/RCS/purge.c,v 1.17 1993/12/08 22:55:04 gk5g Exp $";
#endif

/* ************************************************************ *\
	purge.c
	Purger for the database of already-sent messages on
	post office machines.
\* ************************************************************ */

#include <andrewos.h>
#include <stdio.h>
#include <truth.h>
#include <errno.h>
#include <sys/stat.h>
#include <signal.h>
#ifdef HAS_SYSEXITS
#include <sysexits.h>
#endif
#include <ctype.h>
#include <sys/param.h>
#include <ckndbm.h>

#include <util.h>
#include <mailconf.h>
#include <mail.h>

extern int errno;

static Debugging = 0;
static Deleting = 1;

static char dbmfile[MAXPATHLEN+1] = "/usr/spool/log/SENT";
static char dbmdir[MAXPATHLEN+1], dbmpag[MAXPATHLEN+1],
		newdbm[MAXPATHLEN+1],
		newdbmdir[MAXPATHLEN+1], newdbmpag[MAXPATHLEN+1];

#ifdef tst_NDBM
static DBM *readDB = NULL;
static DBM *writeDB = NULL;
#endif /* tst_NDBM */

int KeysPresent, ValuesExtracted, KeysFresh, KeysWritten, KeysExpired, KeysDeleted;

#ifdef tst_NDBM
static void WritePair(Key, Value)
datum Key, Value;
{/* Write the pair Key, Value to the writeDB.  Only return if it all worked; print a message and exit with status 2 if it failed. */
	int StoreVal;

	if (dbm_error(writeDB)) {
		fprintf(stderr, "Clearing error in writing-DB\n");
		dbm_clearerr(writeDB);
	}
	StoreVal = dbm_store(writeDB, Key, Value, DBM_REPLACE);
	if (StoreVal < 0 || dbm_error(writeDB)) {
		fprintf(stderr, "Can't store a key: value %d, errno %d (%s)\n",
				StoreVal, errno, UnixError(errno));
		if (dbm_error(writeDB)) {
			fprintf(stderr, "Clearing error in writing-DB\n");
			dbm_clearerr(writeDB);
		}
		exit(2);
	}
	++KeysWritten;
}
#endif /* tst_NDBM */

#ifdef tst_NDBM
static int PurgeOlder(MinimumTime)
long int MinimumTime;
{/* Remove entries older than time Time from the database.
  */
	char *Src;
	int Ret;
	datum dbKey, dbValue;
	int Date, RetLen, ForLen, Count;
	static char KeyBuf[PBLKSIZ+1]; int KeyLen;
	static char RetPth[PBLKSIZ+1], ForStr[PBLKSIZ+1];
	register int longval;
	int HardErrs = 1000, IsForStr = 0;

	readDB = dbm_open(dbmfile, osi_O_READLOCK, 0644);
	if (readDB == NULL) return -1;
	if (Debugging) fprintf(stdout, "Reading old database\n");
	KeysPresent = ValuesExtracted = KeysFresh = KeysWritten =
			KeysExpired = KeysDeleted = 0;
	Count = osi_ExclusiveLockNoBlock(dbm_dirfno(readDB));
	if (Count < 0) {
		dbm_close(readDB);
		readDB = NULL;
		return -2;
	}
	if (Debugging) fprintf(stdout, "Old database locked.\n");
	if (Deleting) {
		writeDB = dbm_open(newdbm, O_RDWR | O_CREAT, 0666);
		if (writeDB == NULL) {
			fprintf(stderr, "Can't open new null database %s: %s\n",
					newdbm, UnixError(errno));
			(void) unlink(newdbmdir);
			(void) unlink(newdbmpag);
			exit(2);
		}
		if (osi_ExclusiveLockNoBlock(dbm_dirfno(writeDB)) < 0) {
			fprintf(stderr, "Can't lock %s: %s\n", newdbmdir, UnixError(errno));
			unlink(newdbmpag);
			unlink(newdbmdir);
			dbm_close(writeDB);
			exit(2);
		}
		if (Debugging) fprintf(stdout, "Writing new, locked database\n");
	}
	for (dbKey = dbm_firstkey(readDB); dbKey.dptr != NULL; dbKey = dbm_nextkey(readDB)) {
		++KeysPresent;
		KeyLen = dbKey.dsize;
		if (KeyLen > PBLKSIZ) KeyLen = PBLKSIZ;
		strncpy(KeyBuf, dbKey.dptr, KeyLen);
		KeyBuf[KeyLen] = '\0';
		for (Count = 0; Count < 2; ++Count) {
			dbValue = dbm_fetch(readDB, dbKey);
			if (dbValue.dptr != NULL) break;
			if (dbm_error(readDB)) {
				fprintf(stderr, "Clearing error in reading-DB\n");
				dbm_clearerr(readDB);
			}
			sleep(1);
		}
		if (dbValue.dptr == NULL) dbValue = dbm_fetch(readDB, dbKey);
		if (dbValue.dptr == NULL) {
			if (dbm_error(readDB)) {
				fprintf(stderr, "Clearing error in reading-DB\n");
				dbm_clearerr(readDB);
			}
			fprintf(stdout, "**No value for key ``%s''\n", KeyBuf);
			if (Deleting && Debugging) exit(2); else {
				if (--HardErrs > 0) continue; else break;
			}
		}
		if (dbValue.dsize < 4) {
			if (dbm_error(readDB)) {
				fprintf(stderr, "Clearing error in reading-DB\n");
				dbm_clearerr(readDB);
			}
			fprintf(stdout, "**Value for key ``%s'' has dsize of %d\n",
				KeyBuf, dbValue.dsize);
			if (Deleting && Debugging) exit(2); else continue;
		}
		if (dbValue.dsize < 12)
		    longval = 0;
		else
		    longval = 1;
/*
   The first four bytes of the value are the date at which the message was sent.
   If there are more bytes, then:
   Following that value are eight digits, four plus four, giving the length of
   the Return-Path and the current For: clause.  If there is no For: clause,
   the second four digits will be zero.
*/
		Src = dbValue.dptr;
		Date = 0;
		for (Ret = 0; Ret < 4; Ret++) Date = (Date << 8) | *Src++;
		if (longval) {
		    RetLen = 0;
		    for (Ret = 0; Ret < 4; Ret++)
			RetLen = (RetLen * 10) + (*Src++ - '0');
		    if (dbValue.dsize < (12 + RetLen)) {
			if (dbm_error(readDB)) {
				fprintf(stderr, "Clearing error in reading-DB\n");
				dbm_clearerr(readDB);
			}
			fprintf(stdout,
			"**Value for key ``%s'' has dsize of %d, not 12+%d\n",
				KeyBuf, dbValue.dsize, RetLen);
			if (Deleting && Debugging) exit(2); else continue;
		    }
		    if (RetLen > PBLKSIZ) RetLen = PBLKSIZ;
		    strncpy(RetPth, &(dbValue.dptr[12]), RetLen);
		    RetPth[RetLen] = '\0';
		    ForLen = 0;
		    IsForStr = 0;
		    for (Ret = 0; Ret < 4; Ret++)
			ForLen = (ForLen * 10) + (*Src++ - '0');
		    if (ForLen > PBLKSIZ) ForLen = PBLKSIZ;
		    if (ForLen > 0) {
			if (dbm_error(readDB)) {
				fprintf(stderr, "Clearing error in reading-DB\n");
				dbm_clearerr(readDB);
			}
			if (dbValue.dsize < (12 + RetLen + ForLen)) {
			    fprintf(stdout,
			"**Value for key ``%s'' has dsize of %d, not 12+%d+%d\n",
				    KeyBuf, dbValue.dsize, RetLen, ForLen);
			    if (Deleting && Debugging) exit(2); else continue;
			}
			strncpy(ForStr, &(dbValue.dptr[12 + RetLen]), ForLen);
			ForStr[ForLen] = '\0';
			IsForStr = 1;
		    }
		}
		++ValuesExtracted;
		if (Debugging) {
			fprintf(stdout, "Key ``%s'', value is:\n", KeyBuf);
			fprintf(stdout, "    Date=%d (%s)",
				Date, NiceTime(Date));
			if (longval) {
			    fprintf(stdout, ", RetPth=``%s''", RetPth);
			    if (IsForStr)
				fprintf(stdout, ", For=``%s''", ForStr);
			}
			fprintf(stdout, ".\n");
		}
		if (Date >= MinimumTime) {	/* need to save it */
			++KeysFresh;
			WritePair(dbKey, dbValue);
		}
	}

/* All key-value pairs examined. */
	if (Deleting) {
/* Now we're about to rename the new copy of the database to the old slot.
    There's a window between the two renames, where a reader could see the new copy of the first renamed file and the old copy of the second.  In any case, the .dir file should always be flocked against concurrent modifications, if not reads.
*/
		if (rename(newdbmpag, dbmpag) != 0) {
			fprintf(stderr, "Can't rename NEW.pag to .pag: %s\n",
					UnixError(errno));
			unlink(newdbmdir);
			unlink(newdbmpag);
			exit(2);
		}
		if (rename(newdbmdir, dbmdir) != 0) {
			fprintf(stderr, "**** Can't rename NEW.dir to .dir: %s\n",
					UnixError(errno));
			unlink(newdbmdir);
			unlink(newdbmpag);
			exit(2);
		}
		osi_UnLock(dbm_dirfno(writeDB));
		dbm_close(writeDB); writeDB = NULL;
	}

	(void) osi_UnLock(dbm_dirfno(readDB));
	dbm_close(readDB);
	readDB = NULL;
	printf("%d keys in database, %d values extracted, %d in-range pairs, %d rewritten.\n",
		KeysPresent, ValuesExtracted, KeysFresh, KeysWritten);
	return 0;
}
#endif /* tst_NDBM */

static void Usage(pgm, fmt, p1, p2, p3, p4, p5)
char *pgm, *fmt, *p1, *p2, *p3, *p4, *p5;
{/* usage error: report and quit */
	fprintf(stderr, "Usage error: ");
	fprintf(stderr, fmt, p1, p2, p3, p4, p5);
	fprintf(stderr,
		"\nusage: %s [-dDT] [-f<dbm-file>] <number>[smhdw]\n", pgm);
	exit(3);
}

/* Main program (driver). */
main(argc, argv)
int argc; char *argv[];
{	/* Exits with 0 for no deletions, 1 for deletions, 2 for data error, 3 for usage error */

	struct osi_Times tval;
	int Number, Multiplier, EarlyTime, SecondsAge;
	char *Arg, *MName;
	int ThisArg;

#ifndef tst_NDBM
	fprintf(stderr, "This system type (%s) does not support the ndbm(3) package.\n", SYS_NAME);
	fprintf(stderr, "This program will not run without it.\n");
	exit(3);
#else /* tst_NDBM */
	Debugging = 0;
	Deleting = 1;
	Number = 0; Multiplier = 0; MName = "NONE";	/* no default */
	osi_GetTimes(&tval);
	for (ThisArg = 1; ThisArg < argc; ThisArg++) {
		Arg = argv[ThisArg];
		if (*Arg == '-') {
		    switch (*(Arg+1)) {
			case 'd':   Debugging = 1;
				    break;
			case 'D':   Debugging = 2;
				    break;
			case 'T':   Deleting = 0;
				    break;
			case 'f':	if (*(Arg+2) == '/') dbmfile[0] = '\0';
					else strcpy(dbmfile, DuplicateDB);
					strcat(dbmfile, Arg+2);
					break;
			default:    Usage(argv[0], "illegal option ``%s''", Arg);
		    }
		} else {
			if (! isdigit(*Arg))
			    Usage(argv[0], "need digits, not ``%s''", Arg);
			else {
				Number = atoi(Arg);
				while (isdigit(*Arg)) Arg++;
				if (*Arg == 's') {Multiplier = 1; MName = "second";}
				else if (*Arg == 'm') {Multiplier = 60; MName = "minute";}
				else if (*Arg == 'h') {Multiplier = 60*60; MName = "hour";}
				else if (*Arg == 'd') {Multiplier = 24*60*60; MName= "day";}
				else if (*Arg == 'w')
					{Multiplier = 7*24*60*60; MName = "week";}
				else if (*Arg == '\0') /* nothing */;
				else Usage(argv[0], "need multiplier, not ``%c''", *Arg);
			}
		}
	}
	if (Multiplier == 0) Usage(argv[0], "You need to give a purge interval.");

	SecondsAge = Number * Multiplier;
	if (SecondsAge < 24*60*60 && Debugging == 0) {
		fprintf(stderr,
			"Too recent a purge time: %d %s%s\n",
			Number, MName, (Number == 1 ? "." : "s."));
		exit(3);
	}
	EarlyTime = tval.Secs - SecondsAge;

	/* Construct file names */
	sprintf(dbmdir, "%s.dir", dbmfile);
	sprintf(dbmpag, "%s.pag", dbmfile);
	sprintf(newdbm, "%sNEW", dbmfile);
	sprintf(newdbmdir, "%s.dir", newdbm);
	sprintf(newdbmpag, "%s.pag", newdbm);

	printf("It is now %s.\n", NiceTime(tval.Secs));
	printf("Dbm base file name is \"%s\".\n", dbmfile);
	printf("About to purge entries older than %d %s%s\n",
	       Number, MName, (Number == 1 ? "--" : "s--"));
	printf("that is, older than %s.\n", NiceTime(EarlyTime));
	fflush(stdout);

	PurgeOlder(EarlyTime);

	if (KeysDeleted > 0)
		exit(1);
	else
		exit(0);
#endif /* tst_NDBM */
}
