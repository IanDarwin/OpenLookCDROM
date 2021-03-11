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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/datacat/RCS/datacata.c,v 2.25 1993/05/04 01:14:48 susan Exp $";
#endif


 
#include <sys/param.h> /* For MAXPATHLEN. */

/*
*	datacat - A program for concatening ez documents
*
*
*
*
*/
#include <class.h>
#include <andrewos.h>
#include <datacata.eh>
#include <text.ih>
#include <dataobj.ih>
#include <environ.ih>
#include <keymap.ih>
#include <filetype.ih>
#include <im.ih> /* for im_GetDir and im_ChangeDir */
/* output options */


extern int errno;

char *progname;
#define checkandprint(A) if(A) {datacatapp_PrintVersionNumber(self);A = FALSE;};
boolean verbose;
boolean insertedtemplate = FALSE;
boolean cleanmode = FALSE;

#define MAXIMUM_DEPTH (20)

void datacatapp__ReadInitFile(self)
struct datacatapp *self;
{
}

/* assumes that we want to insert before the last character */
clean_insert(tx, fp, ipros, followlinks)
struct text *tx;
FILE *fp;
boolean ipros, followlinks;
{
#define BUFSIZE (85)

    static char buf[BUFSIZE];
    static char namebuf[256];
    static char obuf[512];
    long tid;
    long ix, len, tpos;
    long pos;
    boolean done = FALSE;
    struct dataobject *o;
    long startpos;

    pos = text_GetLength(tx)-1;
    startpos = pos;

    if (fgets(buf, BUFSIZE, fp) == NULL)
	done = TRUE; /* end of file */

    while (!done) {

	ix = sscanf(buf, "\\begindata{%[^,],%ld}", namebuf, &tid);
	if (ix!=2) {
	    /* not an object inset */
	    text_InsertCharacters(tx, pos, buf, strlen(buf));
	    if (ipros) doinsert(tx, pos, 0, followlinks, 0);
	}
	else {

	    o = (struct dataobject *)class_NewObject(namebuf);
	    if (!o) {
		/* error message, read to enddata */
		sprintf(obuf, "[Unable to create '%s']\n", namebuf);
		text_InsertCharacters(tx, pos, obuf, strlen(obuf));
	    }
	    else {
		ix = dataobject_Read(o, fp, tid);
		switch (ix) {
		    case dataobject_NOREADERROR:
			if (class_IsType(o, tx)) {
			    if (!insertedtemplate) {
				insertedtemplate = TRUE;
				text_ReadTemplate(tx, "default", FALSE);
			    }
			    len = text_GetLength((struct text *)o);
			    if (verbose)
				fprintf(stderr, "Copying text object\n", len);
			    text_AlwaysCopyText(tx, pos, (struct text *)o, 0, len);
			    if (ipros) doinsert(tx, pos, 0, followlinks, 0);
			    tpos = text_GetLength(tx);
			    text_InsertCharacters(tx, tpos, "\n", 1);
			}
			else {
			    if (verbose)
				fprintf(stderr, "Inserting object '%s', view '%s'\n", namebuf, dataobject_ViewName(o));
			    text_AddView(tx, pos, dataobject_ViewName(o), o);
			    tpos = text_GetLength(tx);
			    text_InsertCharacters(tx, tpos, "\n", 1);
			}
			break;
		    default:
			sprintf(obuf, "[Unable to read '%s']\n", namebuf);
			text_InsertCharacters(tx, pos, obuf, strlen(obuf));
			break;
		}
	    }
	}

	pos = text_GetLength(tx)-1;
	if (fgets(buf, BUFSIZE, fp) == NULL)
	    done = TRUE; /* end of file */
    }

}

doinsert(text, size, endskip, followlinks, depth)
struct text * text;
long size, endskip;
boolean followlinks;
long depth;
{
    long i,j,len;
    char name[MAXPATHLEN+1],cname[MAXPATHLEN+1],wdname[MAXPATHLEN+1];
    char *c,*cn;
    FILE *f;
    boolean foundopen ;
    int charflag ;

    if (depth >= MAXIMUM_DEPTH) {
	fprintf(stderr, "halting -- there appears to be a recursion of @include lines (depth %d reached)\n", depth);
	return;
    }

    /*getwd(wdname);*/
    im_GetDirectory(wdname);

    while((i = text_Index(text,size,'@',(len = text_GetLength(text)) - (size+endskip))) >= 0){
	if((i == size || (text_GetChar(text,i - 1) == '\n')) &&  text_Strncmp(text,i,"@include",8) == 0 && i + 10 < len){
	    foundopen = FALSE;
	    charflag = 0;
	    cn = cname;
	    j = i + 8;
	    if(len > j + 1024) len = j + 1024;
	    for(c = name; j < len; j++){
		*c = text_GetChar(text,j);
		*cn++ = *c;
		if(*c == ')' || *c == '\n') break;
		if(*c == '(' ){
		    foundopen = TRUE;
		    continue;
		}
		if(*c == ' ' || *c == '\t'){
		    if(charflag == 1 ) charflag = 2;
		    continue;
		}
		if(charflag > 1){
		    /* found space in name */
		    charflag = 3;
		    break;
		}
		else charflag = 1;
		c++;
	    }
	    if(charflag > 0 && charflag < 3 && foundopen && *c == ')'){
		char *slash;

		*c = '\0';
		slash = (char*) strchr(name, '/');
		if(slash && *name != '/') { /* relative filename */
		    char tmp_name[MAXPATHLEN+1];
		    *slash = (char)0;
		    sprintf(tmp_name, "%s/%s", wdname, name);
		    /*chdir(tmp_name);*/
		    im_ChangeDirectory(tmp_name);
		    filetype_CanonicalizeFilename(cname, slash+1, 1024);
		}
		else
		    filetype_CanonicalizeFilename(cname, name, 1024);
		if(verbose) {
		    fprintf(stderr,"Including  %s\n",cname);
		    fflush(stderr);
		}
		if((f = fopen(cname,"r") ) == NULL){
		    fprintf(stderr,"Can't open include file %s\n",cname);
		}
		else {
		    text_DeleteCharacters(text,i,j + 1 - i);
		    len = text_InsertFile(text,f,name, i);
		    fclose(f);
		    /*if(followlinks) size = i;
		    else size = i + len;*/
		    if (followlinks) {
			doinsert(text, i, text_GetLength(text)-(i+len), followlinks, depth+1);
		    };
		    size = i + len;
		    continue;
		}
	    }
	    else {
		*cn-- = '\0';
		if(*cn == '\n'){
		    *cn = '\0';
		    fprintf(stderr,"badly formed include line '@include%s'\n",cname);
		}
		else 		    
		    fprintf(stderr,"badly formed include line '@include%s...'\n",cname);
	    }

	}
	size = i + 1;
    }
    im_ChangeDirectory(wdname);
}

boolean datacatapp__ParseArgs (self,argc, argv)
struct datacatapp *self;
int argc;
char **argv;
    /* Since we are non-interactive, everything is done in this function so we
      can handle the args as they come */
{
    register int i;
    char *DocumentName ;
    long clen;
    FILE *ofile,*f,*fopen();
    struct text *tx;
    boolean pv = TRUE;
    boolean ipros = FALSE;
    long oldlength;
    boolean followlinks = FALSE;
#ifdef LOCKANDDELETE
    int LockAndDelete = 0;
#endif /* LOCKANDDELETE */

    /* initialize as if ez */
    ((struct application * )self)->name = "ez";
    verbose = FALSE;
    super_ReadInitFile(self);
    ((struct application * )self)->name = "datacat";

    DocumentName = NULL;
    ofile = stdout; f = NULL;
    progname = argv[0];
    tx = text_New();
    for (i=1;i<argc;i++){
	if (argv[i][0] == '-'){
	    switch(argv[i][1]){
		case '\0':
		    /* argument was just - */
		    if (!cleanmode) {
			fprintf(stderr, "You must include the -c switch to read stdin.\n");
		    }
		    else {
			checkandprint(pv);
			if(verbose){
			    fprintf(stderr,"concatenating stdin\n");
			    fflush(stderr);
			}
			oldlength = text_GetLength(tx);
			text_InsertCharacters(tx,oldlength,"\n",1);
			clean_insert(tx, stdin, ipros, followlinks);
			clen = text_GetLength(tx);
			if(text_GetChar(tx,clen - 2) == '\n')
			    text_DeleteCharacters(tx,clen - 2,1);
			if(ipros) doinsert(tx,oldlength,0,followlinks,0);
		    }
		    break;
		case 'i':
		    ipros = TRUE ; /*process includes */
		    break;
		case 'I':
		    ipros = TRUE ; /*process includes */
		    followlinks = TRUE;
		    break;
		case 'q': 
		    pv = FALSE;
		    break;
		case 'c': 
		    cleanmode = TRUE;
		    break;
		case 'v':
		    pv = TRUE;
		    verbose = TRUE;
		    break;
		case 'O':
		case 'o':
		    if(argv[i][2] != '\0')
			DocumentName = &argv[i][2];
		    else if(++i  < argc){
			DocumentName = argv[i];
		    }
		    else {
			checkandprint(pv);
			usage();
		    }
		    if((ofile = (fopen(DocumentName,"w"))) == NULL){
			checkandprint(pv);
			fprintf(stderr,"Can't open %s for output\n",DocumentName);
			exit(1);
		    }
		    break;

#ifdef LOCKANDDELETE
		case 'z':
		    LockAndDelete = 1;
		    break;
#endif /* LOCKANDDELETE */

		default:
		    checkandprint(pv);
		    if(text_GetLength(tx) == 0){
			fprintf(stderr,"bad flag %s\n", argv[i]);
			usage();
		    }
		    else 
			fprintf(stderr,"bad flag %s - ignoring\n", argv[i]);
		    break;
	    }
	}
	else {
#ifndef LOCKANDDELETE
	    if((f = fopen(argv[i],"r") ) == NULL){
		checkandprint(pv);
		fprintf(stderr,"Can't open %s\n",argv[i]);
		continue;
	    }
#else /* LOCKANDDELETE */
	    if((f = fopen(argv[i],(LockAndDelete ? osi_F_READLOCK : "r")) ) == NULL){
		checkandprint(pv);
		fprintf(stderr,"Can't open %s\n",argv[i]);
		continue;
	    }
	    if (LockAndDelete) {
		if (osi_ExclusiveLockNoBlock(fileno(f))){
		    checkandprint(pv);
		    fprintf(stderr, "Cannot lock %s (%d)\n", argv[i], errno);
		    fclose(f);
		    continue;
		}
	    }
#endif /* LOCKANDDELETE */
	    checkandprint(pv);
	    if(verbose){
		fprintf(stderr,"concatenating  %s\n",argv[i]);
		fflush(stderr);
	    }
	    oldlength = text_GetLength(tx);
	    text_InsertCharacters(tx,oldlength,"\n",1);
	    if (cleanmode) {
		clean_insert(tx, f, ipros, followlinks);
	    }
	    else {
		text_InsertFile(tx,f,argv[i], oldlength);
	    }
	    fclose(f);
	    clen = text_GetLength(tx);
	    if (text_GetChar(tx,clen - 2) == '\n')
		text_DeleteCharacters(tx,clen - 2,1);
	    if (ipros && !cleanmode) /* if cleanmode is on, doinsert has been called already */
		doinsert(tx,oldlength,0,followlinks,0);
#ifdef LOCKANDDELETE
	    if (LockAndDelete && unlink(argv[i])) {
		fprintf(stderr, "Cannot delete file %s\n", argv[i]);
	    }
#endif /* LOCKANDDELETE */
	}
    }
    text_Write(tx,ofile,1,0);
    return TRUE;
    }
datacatapp__Run(self)
struct datacatapp *self;
{   /* we are already done */
    exit(0);
}
void datacatapp__FinalizeObject(classID,self)
struct classheader *classID;
struct datacatapp *self;
{
}
boolean datacatapp__InitializeObject(classID,self)
struct classheader *classID;
struct datacatapp *self;
{
    datacatapp_SetMajorVersion(self,1);
    datacatapp_SetMinorVersion(self,1);
    datacatapp_SetPrintVersionFlag(self,FALSE);
    return TRUE;

}
usage(){
	fprintf(stderr,"usage: %s  <-quiet> <-verbose> <-cleanup> < -o OutputFileName> [ - ] [files] \n",progname);
	fflush(stderr);
	exit(2);
	}
