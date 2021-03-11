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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/msprint.c,v 2.35 1993/09/21 21:58:07 gk5g Exp $";
#endif

#include <stdio.h>
#include <andrewos.h> /* sys/file.h */
#include <sys/stat.h>
#include <ms.h>
#include <mailconf.h>
#include <errprntf.h>
#include <hdrparse.h>

#define DefaultDocumentFormat "2"
#define PRINTPREFIX ".MS.ToPrint"

extern char home[], MeInFull[];

extern char *StripWhiteEnds(), *getenv();
extern double getla();
extern int DelayPrinting, AlwaysPrintImmediately;

MS_PrintMessage(DirName, id, flags, printer)
char *DirName, *id, *printer;
int flags;
{
    char RawFileName[1+MAXPATHLEN], PrintQueueFileName[1+MAXPATHLEN], LineBuf[2000];
    int errsave, myid;
    FILE *rfp, *wfp;
    static int ctr=0;

    debug(1,("MS_PrintMessage %s %s flags %d printer %s\n", DirName, id, flags, printer ? printer : "<NULL>"));
    if (CheckPrinterValidity(printer)) AMS_RETURN_ERRCODE(EMSBADPRINTER, EIN_PARAMCHECK, EVIA_PRINTMESSAGE);
    if (!DirName || !id) AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_PRINTMESSAGE);
    QuickGetBodyFileName(DirName, id, RawFileName);
    /* This next loop puts each print request in a separate file.  With this step, the whole raison d'etre of the text822 object is nearly gone, and we are wasting LOTS of paper on cover pages, but it was necessary to make MIME-format mail print properly, sigh... We could save a little paper by putting all plain text & Andrew print requests in a single file, but I'm not feeling up to that right now...  */
    
    while (TRUE) {
	sprintf(PrintQueueFileName, "%s/%s.%d.%d", home, PRINTPREFIX, getpid(), ++ctr);
	if (printer) {
	    strcat(PrintQueueFileName, "_");
	    strcat(PrintQueueFileName, printer);
	}
	if (access(PrintQueueFileName, F_OK)) break;
    }
    rfp = fopen(RawFileName, "r");
    if (!rfp && (RetryBodyFileName(RawFileName) < 0
		 || !(rfp = fopen(RawFileName, "r")))) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_PRINTMESSAGE);
    }
    wfp = fopen(PrintQueueFileName, "a");
    if (wfp == NULL) {
	errsave = errno;
	fclose(rfp);
	AMS_RETURN_ERRCODE(errsave, EIN_FOPEN, EVIA_PRINTMESSAGE);
    }
    if (osi_ExclusiveLockNoBlock(fileno(wfp))) {
	errsave = errno;
	fclose(wfp);
	fclose(rfp);
	AMS_RETURN_ERRCODE(errsave, EIN_FLOCK, EVIA_PRINTMESSAGE);
    }
    myid = time(0); /* Not ideal, but... */
    fprintf(wfp, "\\begindata{text822, %d}\nPrinted-For: %s\nPrinted-From: %s\nPrinted-Date: %s", myid, MeInFull, RawFileName, arpadate());
    if (flags) fprintf(wfp, "X-Andrew-Text822Mode: %d\n", flags);
    while (fgets(LineBuf, sizeof(LineBuf), rfp) != NULL) {
	fputs(LineBuf, wfp);
    }
    fprintf(wfp, "\\enddata{text822, %d}\n", myid);
    fclose(rfp);
    if (ferror(wfp) || feof(wfp)) {
	fclose(wfp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_PRINTMESSAGE);
    }
    if (vfclose(wfp)) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_PRINTMESSAGE);
    }
    if (AlwaysPrintImmediately) {
	return(PrintPendingRequests(TRUE));
    }
    return(0);
}

static char SEPARATOR[] = "\n____________________________________________________________\n";

/* The following routine used to be needed for printing messages; it is still needed for generating reply templates, in reply.c */

PrintSpecialStuff(fp, prcode, FormatVersion)
FILE *fp;
int prcode;
char *FormatVersion;
{
    Boolean OldStyle = FALSE;

    FormatVersion = StripWhiteEnds(FormatVersion);
    if (!strcmp(FormatVersion, "2")
    || !lc2strncmp("yes", FormatVersion, 5)) {
	OldStyle = TRUE;
    }
    switch(prcode) {
	case PR_STARTBOLD:
	    fprintf(fp, OldStyle ? "@begin(bold)" : "\\bold{");
	    break;
	case PR_ENDBOLD:
	    fprintf(fp, OldStyle ? "@end(bold)" : "}");
	    break;
	case PR_STARTBIGGER:
	    fprintf(fp, OldStyle ? "@begin(bigger)" : "\\bigger{");
	    break;
	case PR_ENDBIGGER:
	    fprintf(fp, OldStyle ? "@end(bigger)" : "}");
	    break;
	case PR_SEPARATORLINE:
	    fprintf(fp, OldStyle ? "\n\n" : "\n");
	    PrintQuotingFormatting(fp, SEPARATOR, FormatVersion, sizeof(SEPARATOR)-1);
	    fprintf(fp, OldStyle ? "\n\n" : "\n");
	    break;
	case PR_STARTSHRINK:
	    fprintf(fp, OldStyle ? "@begin(smaller)" : "\\smaller{");
	    break;
	case PR_ENDSHRINK:
	    fprintf(fp, OldStyle ? "@end(smaller)" : "}");
	    break;
	case PR_STARTSUPERSHRINK:
	    fprintf(fp, OldStyle ? "@FormatNote(\n\n.vs 6\n\n)@begin(smaller)@begin(smaller)@begin(smaller)@begin(smaller)" : "\\formatnote{\n.vs 6\n}\\smaller{\\smaller{\\smaller{\\smaller{");
	    break;
	case PR_ENDSUPERSHRINK:
	    fprintf(fp, OldStyle ? "@end(smaller)@end(smaller)@end(smaller)@end(smaller)@FormatNote(\n\n.vs 14\n)\n" : "}}}}\\formatnote{\n.vs 14}\n");
	    break;
	case PR_BODYSEPARATOR:
	    fprintf(fp, OldStyle ? "@FormatNote(\n.bp)\n\n" : "\\formatnote{\n\n.bp\n}\n");
	    break;
	case PR_STARTTYPING:
	    fprintf(fp, OldStyle ? "@begin(typewriter)" : "\\typewriter{");
	    break;
	case PR_ENDTYPING:
	    fprintf(fp, OldStyle ? "@end(typewriter)" : "}");
	    break;
    }
}


/* If we try to print twice in a row, we'll get lock collisions; this is
    more-or-less inhibited by the PRINTCOLLISIONTIME hack */
#define PRINTCOLLISIONTIME 30
#define PRINTVECMAX 100
PrintPendingRequests(MustPrint) 
Boolean MustPrint;
{
    DIR *dirp;
    DIRENT_TYPE *dirent;
    char *PrintVector[PRINTVECMAX+4], *s, PrintProg[1+MAXPATHLEN], *thisprinter = NULL;
    int VecIndex = 3, homelen, prefixsize;
    static int lastgoodprint = 0;
    extern char *DefaultPrinter;

    if (!MustPrint && (DelayPrinting || getla(0) > 0.8) || ((time(0) - lastgoodprint) < PRINTCOLLISIONTIME)) {
	return(0);
    }
    if ((dirp = opendir(home)) == NULL) {
	AMS_RETURN_ERRCODE(errno, EIN_OPENDIR, EVIA_PRINTMESSAGE);
    }
    homelen = strlen(home)+2;
    prefixsize = sizeof(PRINTPREFIX) -1;
    sprintf(PrintProg, AndrewDir("/bin/ezprint"));
    PrintVector[0] = PrintProg;
    PrintVector[1] = "-o";
    PrintVector[2] = "Messages_You_Wanted_To_Print";
    while ((dirent = readdir(dirp)) != NULL) {
	if (!strncmp(dirent->d_name, PRINTPREFIX, prefixsize)) {
	    char bashable_name[MAXPATHLEN];
	    strcpy(bashable_name,dirent->d_name);
	    /* Found one! */
	    PrintVector[VecIndex++] = "-z";
	    thisprinter = strchr(bashable_name, '_');
	    if (thisprinter) {
		*thisprinter++ = '\0';
		s = strchr(thisprinter, '_'); /* future compatibility */
		if (s) *s++ = '\0';
	    }
	    if (thisprinter) {
		if (CheckPrinterValidity(thisprinter)) {
		    char ErrorText[250];

		    sprintf(ErrorText, "Printer %s is unrecognized; using default of %s instead.", thisprinter, DefaultPrinter);
		    NonfatalBizarreError(ErrorText);
		    thisprinter = NULL;
		}
	    }
	    if (!thisprinter) {
		debug(1, ("Using default printer: %s\n", DefaultPrinter));
		thisprinter = DefaultPrinter;
	    }
	    debug(1, ("Printing %s on printer %s\n",dirent->d_name, thisprinter ? thisprinter : "<default>"));
	    if (thisprinter) {
		PrintVector[VecIndex] = malloc(3+strlen(thisprinter));
		if (!PrintVector[VecIndex]) {
		    FreePrintVec(PrintVector, --VecIndex);
		    closedir(dirp);
		    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PRINTMESSAGE);
		}
		sprintf(PrintVector[VecIndex++], "-S%s", thisprinter);
	    }
	    PrintVector[VecIndex] = malloc(homelen+strlen(dirent->d_name));
	    if (!PrintVector[VecIndex]) {
		FreePrintVec(PrintVector, --VecIndex);
		closedir(dirp);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_PRINTMESSAGE);
	    }
	    sprintf(PrintVector[VecIndex++], "%s/%s", home,dirent->d_name);
	}
	if (VecIndex >= PRINTVECMAX) {
	    NonfatalBizarreError("Too many prints to do all at once; saving some for later.");
	    break;
	}
    }
    closedir(dirp);
    PrintVector[VecIndex] = NULL;    
    if (VecIndex > 3) {
	int fd;
	char ErrTxt[500+MAXPATHLEN];

	errprintf("ms", ERR_WARNING, 0, 0, "Now printing messages as requested");
	if (! osi_vfork()) {
	    /* I am a child */
	    for (fd = getdtablesize(); fd > 2; --fd) (void) close(fd);
	    execv(PrintVector[0], PrintVector);
	    strcpy(ErrTxt, "Could not exec ezprint!  (");
	    strcat(ErrTxt, UnixError(errno));
	    strcat(ErrTxt, ", ");
	    strcat(ErrTxt, ap_Shorten(PrintVector[0]));
	    strcat(ErrTxt, ")");
	    NonfatalBizarreError(ErrTxt);
	    _exit(1);
	}
	FreePrintVec(PrintVector, --VecIndex);
    }
    lastgoodprint = time(0);
    return(0);
}

FreePrintVec(PrintVector, VecIndex)
char *PrintVector[];
int VecIndex;
{
    while (VecIndex > 3) {
	if (PrintVector[VecIndex] && strcmp(PrintVector[VecIndex], "-z") ) {
	    free(PrintVector[VecIndex]);
	}
	--VecIndex;
    }
}
/*
static RotBuf13(buf, ct)
char *buf;
int ct;
{
    register char *s = buf;
    while (*s) {
	if ((*s >= 0x41 && *s <= 0x5a) || (*s >= 0x61 && *s <= 0x7a)) {
	    *s = (((((*s -1 ) & 0X1F) + 13) % 26) + 1) | (*s & 0XE0);
	}
	++s;
    }
}
*/
CheckPrinterValidity(printer)
char *printer;
{
    int i, Checked;
    char FName[1+MAXPATHLEN];
    struct stat stbuf;

    if (!printer) return(0); /* Defaults are always fine */
    Checked = FALSE;
    debug(1, ("Validating printer name %s\n", printer));
    if (AMS_PrinterNamesInSpoolDirectories) {
	debug(1, ("Checking in spool directories\n"));
	for(i=0; i<numAMS_PrintSpoolDirectories; ++i) {
	    strcpy(FName, AMS_PrintSpoolDirectories[i]);
	    strcat(FName, "/");
	    strcat(FName, printer);
	    debug(1, ("Checking for existence of directory %s\n", FName));
	    if (!stat(FName, &stbuf) && ((stbuf.st_mode & S_IFMT) == S_IFDIR)) {
		debug(1, ("Bingo!  It is fine.\n"));
		return(0);
	    }
	    Checked = TRUE;
	}
    }
    if (AMS_PrinterNamesInPrintcap) {
	FILE *fp;
	char LineBuf[1000];

	debug(1, ("Looking in /etc/printcap for printer %s\n", printer));
	fp = fopen("/etc/printcap", "r");
	if (fp) {
	    debug(1, ("Opened /etc/printcap\n"));
	    while (fgets(LineBuf, sizeof(LineBuf), fp)) {
		if (PrinterInPrintcapLine(printer, LineBuf)) {
		    debug(1, ("Found it!\n"));
		    return(0);
		}
	    }
	    fclose(fp);
	    Checked = TRUE;
	}
	debug(1, ("Didn't find anything useful in /etc/printcap\n"));
    }
    debug(1, ("Checking for NamedValidPrinters\n"));
    for(i=0; i<numAMS_NamedValidPrinters; ++i) {
	if (!strcmp(AMS_NamedValidPrinters[i], printer)) {
	    return(0);
	}
	debug(1, ("It isn't %s\n", AMS_NamedValidPrinters[i]));
	Checked = TRUE;
    }
    if (Checked) {
	debug(1, ("%s is NOT a valid printer name.\n", printer));
	return(-1);
    } else {
	debug(1, ("Treating %s as OK, since there was no way to validate it.\n", printer));
	return(0);
    }
}

PrinterInPrintcapLine(printer, line)
char *printer, *line;
{
    char *start, *s;

    for (s=start=line; *s; ++s) {
	if (*s == ':' || *s == '|') {
	    *s = '\0';
	    if (!strcmp(start, printer)) return(1);
	    start = s+1;
	}
    }
    return(0);
}

