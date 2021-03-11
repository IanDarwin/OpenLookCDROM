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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ezprint/RCS/ezprinta.c,v 2.23 1993/01/08 16:31:54 rr2b R6tape $";
#endif


 

#include <sys/param.h> /* For MAXPATHLEN. */

/*
*	ezprint - A program for printing ez documents
*
*	Modified from bx and be1 code.
*
*
*
*/
#include <andrewos.h> /* sys/file.h */
#include <class.h>
#include <ezprinta.eh>
#include <im.ih>
#include <text.ih>
#include <textv.ih>
#include <readscr.ih>
#include <txttroff.ih>
#include <view.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <filetype.ih>

/* doc_Read(dd,file) */
#include <signal.h>
#include <ctype.h>
#include <print.ih>
#define print_INDEXTROFF 42424
#include <keymap.ih>
#ifdef hpux
#include <unistd.h>
#endif /* hpux */
/* output options */
#define PREVIEW 0
#define PRINT 1
#define TROFF 2

extern int errno;
static boolean quiet;
#ifdef PageOffsetOpt

static char *topts[] = {
	"1.25",
	"6",
	".fi",
	};
#endif /* PageOffsetOpt */

char *progname;

void ezprintapp__ReadInitFile(self)
struct ezprintapp *self;
{
}

static void usage(self)
struct ezprintapp *self;
{
    if(!quiet){
	ezprintapp_PrintVersionNumber(self);
    }
    fprintf(stderr,"usage: %s <-P PrinterName> <-troff> <-preview> <-stdin> < -o OutputFileName> <-v ScribeVersion> <-contents> <-zapafterprinting> <-Enumerate> <-IndexOnly> <-quiet> file\n",progname);
    exit(2);
}

boolean ezprintapp__ParseArgs (self,argc, argv)
struct ezprintapp *self;
    int argc;
    char **argv;
/* Since we are non-interactive, everything is done in this function so we
can handle the args as they come */
    {register int i;
    char *DocumentName,*c,*printargs ;
    FILE *ofile,*f,*fopen();
    boolean indexflag;
    struct dataobject *d;
    struct view *v;
    char *objectName,*viewName;
    long objectID;
    int ffl = 0;
    int popt = PRINT;
    int status;
    int LockAndDelete = 0; 
    struct view *ff;
    char *ScribeVersion = NULL;
    char *printer = NULL;
    char *currentfile;
    indexflag = FALSE;
    /* initialize as if ez */
    ((struct application * )self)->name = "ez";
    super_ReadInitFile(self);
    ((struct application * )self)->name = "ezprint";
    d = NULL;
    printargs = "";
    DocumentName = NULL;
    quiet = FALSE;
    ofile = stdout; f = NULL;
    progname = argv[0];
    ff = (struct view *) class_NewObject("frame");
    for (i=1;i<argc;i++){
	if (argv[i][0] == '-'){
	    char *env,*val;
	    	switch(argv[i][1]){
		case 'q':
		    quiet = TRUE;
		    break;
		case 'i':
		case 'I':
		    environ_Put("IndexOnly",NULL);
		    indexflag = TRUE;
		    break;
		case 'e':
		    if(argv[i][2] != '\0')
			env = &argv[i][2];
		    else if(++i  < argc){
			env = argv[i];
		    }
		    else {
			usage(self);
			break;
		    }
		    if((val = index(env,'=')) != NULL){
			*val++ = '\0';
			if(*val == '\0'){
			    environ_Delete(env);
			    break;
			}
		    }
		    environ_Put(env,val);
		    break;
		case 'E': /* enumerate */
		    environ_Put("AutoEnumerate",NULL);
		    break;
		case 'F':
		    val = "";
		    if(argv[i][2] != '\0')
			val = &argv[i][2];
		    else if(++i  < argc){
			val = argv[i];
		    }
		    switch(*val){
			case 'E':
			case 'e':
			    environ_Put("Endnotes",val);
			    break;
			default:
			    environ_Delete("Endnotes");
		    }
		case 'c':
		    val = "";
		    if(argv[i][2] != '\0')
			val = &argv[i][2];
		    environ_Put("PrintContents",val);
		    break;
		case 'C':
		    if(argv[i][2] != '\0')
			val = &argv[i][2];
		    else if(++i  < argc){
			val = argv[i];
		    }
		    else {
			usage(self);
			break;
		    }
		    environ_Put("ContentsList",val);
		    break;
		case 'N':
		    if(argv[i][2] != '\0')
			val = &argv[i][2];
		    else if(++i  < argc){
			val = argv[i];
		    }
		    else {
			usage(self);
			break;
		    }
		    environ_Put("InitialChapNumber",val);
		    break;
		case 'T':
		    if(argv[i][2] != '\0')
			val = &argv[i][2];
		    else if(++i  < argc){
			val = argv[i];
		    }
		    else {
			usage(self);
			break;
		    }
		    environ_Put("TroffArgs",val);
		    break;
		case 'n':
		    if(argv[i][2] != '\0')
			val = &argv[i][2];
		    else if(++i  < argc){
			val = argv[i];
		    }
		    else {
			usage(self);
			break;
		    }
		    {
		    char buff[128];
		    sprintf(buff,"-n%s",val);
		    environ_Put("TroffArgs",buff);
		    }
		    break;
		case 'p':
			popt = PREVIEW;
			break;
		case 't':
			popt  = TROFF;
			break;
		case 'h':	/* hard copy (default) */
			popt = PRINT;
			break;
		case 'O':
		case 'o':
    		        if(argv[i][2] != '\0')
				DocumentName = &argv[i][2];
			else if(++i  < argc){
				DocumentName = argv[i];
				}
			else {
				usage(self);
				}
			break;
		case 's':
			/* Read document from stdin */
			f = stdin;
			break;
		case 'a':
		        if(argv[i][2] != '\0')
				printargs = &argv[i][2];
			else if(++i  < argc){
				printargs = argv[i];
				}
			else {
				usage(self);
				}
			break;
#ifdef PageOffsetOpt
		case 'm':	/* Page offset - left margin */
		        if(argv[i][2] != '\0')
				topts[PageOffsetOpt] = &argv[i][2];
			else if(++i  < argc){
				topts[PageOffsetOpt] =argv[i];
				}
			else {
				usage(self);
				}
			 break;
		case 'l':	/* Line length */
		        if(argv[i][2] != '\0')
				topts[LineLengthOpt] = &argv[i][2];
			else if(++i  < argc){
				topts[LineLengthOpt] =argv[i];
				}
			else {
				usage(self);
				}
			 break;
		case 'n': /* no fill lines */
			topts[FillLineOpt] = "";
			break;
#endif /* PageOffsetOpt */
		case 'v':
			if (argv[i][2]) {
			    ScribeVersion = 2+argv[i];
			} else if (++i < argc) {
			    ScribeVersion = argv[i];
			} else {
				usage(self);
			}
			break;
		case 'V':
			ScribeVersion = NULL;
			break;
		case 'z':
			LockAndDelete = 1;
			break;
		case 'S':
		case 'P':
		        if(argv[i][2] != '\0')
				printer = &argv[i][2];
			else if(++i  < argc){
				printer =argv[i];
				}
			else {
				usage(self);
				}
			 break;
		default:
			 printf("bad flag %s\n", argv[i]);
			break;
		}
	    }
	else {
		if((f = fopen(argv[i],(LockAndDelete ? osi_F_READLOCK : "r")) ) == NULL){
			fprintf(stderr,"Can't open %s\n",argv[i]);
			continue;
		}
		currentfile = argv[i];
printit:	ffl++;
                if(!quiet){
		    ezprintapp_PrintVersionNumber(self);
		    quiet = TRUE;
		}
		if (LockAndDelete) {
		    if (osi_ExclusiveLockNoBlock(fileno(f))){
			fprintf(stderr, "Cannot lock %s (%d)\n", currentfile, errno);
			fclose(f);
			continue;
		    }
		}
		if (ScribeVersion) {
		    objectName = "text";
		    objectID = 0;
		} else {
		    if(f == stdin)
			objectName = (char *) filetype_Lookup(f,NULL, &objectID, NULL);
		    else
			objectName = (char *) filetype_Lookup(f,currentfile, &objectID, NULL);
		}
		if(printer)  {   /* insure that both of these are set, catch sys5 + ucb
spoolers */
                    environ_Put("LPDEST",printer);
		    environ_Put("PRINTER",printer);
                }
		if(objectName == NULL) d = (struct dataobject *) text_New();
		else d = (struct dataobject *) class_NewObject(objectName);
		dataobject_Read(d,f,objectID);
		viewName = dataobject_ViewName(d);
		if(viewName == NULL) v = (struct view *) textview_New();
		else v =(struct view *) class_NewObject(viewName);
		view_SetDataObject(v,d);
		view_LinkTree(v,ff);
		if (ScribeVersion) {
		    struct text *mytext;

		    mytext = (struct text *) d;
		    if (readscr_Begin(mytext, 0, text_GetLength(mytext), 1, ScribeVersion, 1) != mytext) {
			fprintf(stderr, "Can't read scribe format in file %s\n", currentfile);
			exit(1);
		    }
		}
	   	if(popt == TROFF){
			/* write out troff file */
			if(DocumentName){
				if((ofile = fopen(DocumentName,"w")) == NULL){
					fprintf(stderr,"can't open %s for output\n",DocumentName);
					exit(1);
					}
				}
			if(indexflag){
			    status = print_ProcessView(v, print_INDEXTROFF, 0 ,currentfile,printargs);
			}
			else {
			    view_Print(v,ofile,"troff","PostScript",1);
			    status = 0; /* Bogus -- no error code returned! */
			}
			fflush(ofile);
			if(ofile != stdout){
				 fclose(ofile);
				ofile = stdout;
				}
		} else {
			if(DocumentName){
			    for(c = DocumentName; *c != '\0'; c++){
				if(*c == '/'  ) *c = '-';
				else if((!isprint(*c)) || isspace(*c)) *c = '_';
				}
			    status = print_ProcessView(v, popt, 0 ,DocumentName,printargs);
			    }
			else {
			    status = print_ProcessView(v, popt, 0 ,currentfile,printargs);
			}
		}
		DocumentName = NULL;
		if (status) {
		    fprintf(stderr, "Print request for %s apparently failed.\n", currentfile);
		    if (LockAndDelete) {
			char Fname[1+MAXPATHLEN], DirName[1+MAXPATHLEN], *s;
			int ctr = 0;

			strcpy(DirName, currentfile);
			s = rindex(DirName, '/');
			if (s) *++s = NULL;
			while (++ctr < 1000) {
			    sprintf(Fname, "%s/PrintErr.%d", DirName, ctr);
			    if (access(Fname, F_OK) != 0) break;
			}
			if (ctr >= 1000) {
			    fprintf(stderr, "Cannot rename failed print file %s!", currentfile);
			} else {
			    if (rename(currentfile, Fname)) {
				fprintf(stderr, "Cannot rename %s to %s!", currentfile, Fname);
			    }
			}
		    }
		} else if (LockAndDelete && unlink(currentfile)) {
		    fprintf(stderr, "Cannot delete file %s\n", currentfile);
		}
		/* Only close AFTER delete when we did the locking */
		fclose(f);
		view_Destroy(v);
/* 		dataobject_Destroy(d);
 */	    }
	}
	if(!ffl) {
		if(f == stdin) {
		    LockAndDelete = FALSE;
		    currentfile = "Stdin";
		    goto printit;
		}
		usage(self);
		}
	return TRUE;
}
ezprintapp__Run(self)
struct ezprintapp *self;
{   /* we are already done */
    exit(0);
}
boolean ezprintapp__InitializeObject(classID,self)
struct classheader *classID;
struct ezprintapp *self;
{
    ezprintapp_SetPrintVersionFlag(self,FALSE);
    ezprintapp_SetMajorVersion(self, 7);
    ezprintapp_SetMinorVersion(self, 0);
return TRUE;

}

