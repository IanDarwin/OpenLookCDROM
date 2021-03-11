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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/extensions/RCS/tags.c,v 2.22 1994/05/16 02:56:01 rr2b Exp $";
#endif


 

/* * tags.c -- A tags package for the ATK editor
   *
   * Uses the output from the 'ctags' command to find
   * function definitions.
   * Currently cannot set default for tag file name.
   * Does not deal with multiple windows.
   * Now does recursive edit -- give it an argument (^U) to AVOID recursive edit
*/

#include <andrewos.h>
#include <class.h>
#include <sys/wait.h>
#include <textv.ih>
#include <message.ih>
#include <text.ih>
#include <search.ih>
#include <proctbl.ih>
#include <buffer.ih>
#include <frame.ih>
#include <dataobj.ih>
#include <im.ih>
#include <environ.ih>
#include <filetype.ih>
#include <complete.ih>
#include <view.ih>
#include <ctype.h>
#include <sys/param.h>
#include <tags.eh>

struct SearchPattern {
    short size, used;
    unsigned char *body;
};


#define TBUFNAM "Tags-Buffer"
static char *TagsFile = NULL;


struct finderInfo {
    struct frame *myFrame, *otherFrame, *bestFrame;
    struct buffer *myBuffer;
};

static ViewEqual(frame, view)
    struct frame *frame;
    struct view *view;
{

#if 1
    return (frame_GetView(frame) == view);
#else /* 1 */
    return (view_GetIM((struct view *) frame) == view_GetIM(view))
#endif /* 1 */
}

static struct frame *FindByView(view)
    struct view *view;
{

    return frame_Enumerate(ViewEqual, (long) view);
}


static void checkFileName()
{
    if (TagsFile == NULL)
        if ((TagsFile = environ_GetProfile("tagfilename")) == NULL)
            TagsFile = "tags";

}


static struct buffer *tags_OpenBuffer(filename)
char *filename;

{
    struct buffer *rbuffer;
    long version;

    rbuffer = buffer_GetBufferOnFile(filename,buffer_MustExist);
    if (rbuffer) {
        version = dataobject_GetModified(buffer_GetData(rbuffer));
        buffer_SetCkpClock(rbuffer,0);
        buffer_SetCkpVersion(rbuffer,version);
        buffer_SetWriteVersion(rbuffer,version);
    }
    return rbuffer;
}

/* open a buffer on the tag file*/

static struct buffer *tags_OpenTagsBuffer(name)
char *name;

{
    struct buffer *tagsbuffer;
    long version;
    
    checkFileName();

    if ((tagsbuffer = buffer_FindBufferByName(TBUFNAM)) == NULL) {
        tagsbuffer = buffer_GetBufferOnFile(TagsFile, TRUE);
        if (tagsbuffer==NULL) return NULL;
        buffer_SetName(tagsbuffer,TBUFNAM);
        buffer_SetScratch(tagsbuffer,TRUE);
        version = dataobject_GetModified(buffer_GetData(tagsbuffer));
        buffer_SetCkpClock(tagsbuffer,0);
        buffer_SetCkpVersion(tagsbuffer,version);
        buffer_SetWriteVersion(tagsbuffer,version);
    }
    return tagsbuffer;
}



/*
 * Zombie handler for completion of the 'tags' command.
 */
void tags_finished(pid, view, status)
int pid;
struct view *view;
long *status;
{
    int exit_status;
    struct buffer *buffer;

    /* Extract exit status */
    exit_status = (status == NULL || *status & 0xff) ? -1 : (*status >> 8) & 0xff;
    if (exit_status != 0)
	message_DisplayString(view,0,"Rebuild failed");
    else {
	message_DisplayString(view,0,"Tag file rebuilt.");
	if (buffer=buffer_FindBufferByName(TBUFNAM)) {
	    buffer_ReadFile(buffer,TagsFile);
	}
    }
}

void tags_RebuildTagsFile(view, key)
struct view *view;
long key;

{
    int pid;
#ifdef linux
    static char command[256]="ctags *.c *.h",
                command2[256];
#else
    static char command[256]="ctags -w *.c *.h",
                command2[256];
#endif
    checkFileName();

    if (im_ArgProvided(view_GetIM(view))) {
	if (message_AskForString(view,0,"Rebuild tags with command: ",command, command2, 255))
	    return;
	strcpy(command, command2);
    }

    message_DisplayString(view,0,"Rebuilding tag file; please wait...");
    im_ForceUpdate();

    /*    sprintf(command,"ctags -w -f %s *.c", TagsFile);*/ /* should be able to use -f file */

    if ((pid = osi_vfork()) == 0) { /* in child process */
        execlp("/bin/csh","csh","-f", "-c",command,">>& /dev/console",0);
	exit(0);
    }
    im_AddZombieHandler(pid, tags_finished, view);
}


/* skips over to the next field separated by blanks & tabs */

long nextField(doc, pos)
struct text *doc;
long pos;

{
    char c;

    c = text_GetChar(doc,pos);
    while ((c!='\t') && (c!=' '))
        c = text_GetChar(doc,++pos);
    while ((c=='\t') || (c==' '))
        c = text_GetChar(doc,++pos);
    return pos;
}


/* returns true if pos is in the first field
   fields are separated by blanks & tabs */

int firstField(doc,pos)
struct text *doc;
long pos;

{
    char c = text_GetChar(doc,pos);
    if ((c == ' ') || (c == '\t') || (c == '\n'))
        return 0;
    while ((c!='\n') && (pos>0)) {
        c = text_GetChar(doc,--pos);
        if ((c == ' ') || (c == '\t'))
            return 0;
    }
    return 1;
}


/* gets the function name at the dot */

char *getFunction(doc,pos)
struct text *doc;
long pos;
{
    char c;
    static char name[256];
    char *nptr=name;
    
    c=text_GetChar(doc,pos);
    while (isalnum(c) || c=='_') 
        c=text_GetChar(doc,--pos);
    c=text_GetChar(doc,++pos);
    while (isalnum(c) || c=='_') 
        *nptr++ = c = text_GetChar(doc,pos++);
    *--nptr='\0';
    return name;
}

struct view *TagsCreateWindow(buffer)
struct buffer *buffer; {
    struct frame *frame;
    struct im *newIM;
    struct view *view;
    newIM = im_Create(NULL);
    frame = frame_Create(buffer);
    im_SetView(newIM, frame);
    frame_PostDefaultHandler(frame, "message", frame_WantHandler(frame, "message"));

/* This is here because the frame_Create call can't set the input focus
 * because the frame didn't have a parent when it called view_WantInputFocus.
 * This is bogus but hard to fix...
 */
    view = frame_GetView(frame);
    view_WantInputFocus(view, view);
    return view;
}

/* Most of the work is done here. */

void tags_FindTag (view, tag, RecursiveEdit)
struct view *view;
char *tag;
int RecursiveEdit;

{
    char find[255];

    struct text *doc; /* = (struct text *) view->dataobject;*/
    long position=-1, fuzzypos=-1, curpos, length;
    boolean foundTag=FALSE;
    int same;

    static struct SearchPattern *result = NULL;
    struct frame *ourFrame;
    struct view *ourView;
    struct buffer *filebuffer, *curbuf, *TagsBuffer;
    static char c, filename[256], searchstring[256];
    char *fname=filename, *sstring=searchstring;
    int match_bol = 0, match_eol = 0;
    boolean justAnumber;

    /* open or create tags buffer */
    if ((TagsBuffer = tags_OpenTagsBuffer("Tags")) == NULL) {
        message_DisplayString(view,0,"Could not open tags file");
        return;
    }
    doc = (struct text *) buffer_GetData(TagsBuffer);

    search_CompilePattern(tag, &result);
    while (!foundTag) {
	/* keep looking for the EXACT tag name, and only use a fuzzy match IF no exact one was found  -RSK*/
	position = search_MatchPattern(doc, position+1, result);
	if (position<0) {
	    /* we're all done looking, no exact match, so use fuzzy one */
	    foundTag= TRUE;
	    if (fuzzypos>=0)
		position= fuzzypos;
	    else {
		/* no matches found, not even a fuzzy one */
		sprintf(find,"Tag '%s' not found",tag);
		message_DisplayString(view, 0, find);
		return;
	    }
	} else if (firstField(doc,position)) {
	    /* the string we found IS a tag, remember this position for posterity */
	    if ((position==0 || text_GetChar(doc,position-1)=='\n') &&
		((c=text_GetChar(doc,position+search_GetMatchLength()))==' ' || c=='\t')) {
		/* we found that EXACT tag */
		foundTag= TRUE;
	    } else if (fuzzypos<0) {
		/* it's the first fuzzy match we've found; remember it just in case an EXACT one isn't found */
		fuzzypos= position;
	    }
	} /* else what we found is in the middle of something else, just keep searching */
    }

    /* position is pointing to either the exact tag, or the first fuzzy one if no exact match was found  -RSK*/
    position = nextField(doc, position);

    /* copy filename to string */
    c = text_GetChar(doc,position);
    while((c!='\t') && (c!=' ')) {
	*fname++ = c;
	c = text_GetChar(doc,++position);
    }
    *fname = '\0';

    /* copy search string */
    /* skip over '/^' */
    curpos= position= nextField(doc, position);
    while ((c=text_GetChar(doc,position))=='/' || c=='?')
	++position;
    if (position==curpos)
	justAnumber= TRUE; /* might be proven wrong later, but at least it's POSSIBLE */
    else
	justAnumber= FALSE; /* impossible; it's definitely a search string */

    while ((c!='/') && (c!='?')) {
	switch(c) {
	    case '\n':
		if (justAnumber && sstring>searchstring) {
		    /* quit grabbing stuff, we've got our typedef line number */
		    c= '/';
		    continue;
		}
		break;
	    case '^':
		*sstring++ = '\n';
		match_bol = 1;
		break;
	    case '$':
		*sstring++ = '\n';
		match_eol = 1;
		break;
	    case '*':
		*sstring++ = '\\';
		/* fall through */
	    default:
		*sstring++ = c;
		if (justAnumber && !isdigit(c))
		    justAnumber= FALSE;
	}
	c = text_GetChar(doc,++position);
    }
    *sstring = '\0';

    if ((filebuffer = tags_OpenBuffer(filename)) == NULL) {
	sprintf(find,"Bad tag - could not open source file %s",filename);
	message_DisplayString(view,0,find);
	return;
    }

    doc = (struct text *) buffer_GetData(filebuffer);

    if (justAnumber && strlen(searchstring)>=1) {
	/* it's just a line number, the result of using "ctags -t" to find typedefs  -RSK*/
	long linenum;
	if (sscanf(searchstring, "%ld", &linenum)==1) {
	    position= text_GetPosForLine(doc, linenum);
	    length= text_GetPosForLine(doc, linenum+1) - position - 1;
	} else {
	    sprintf(find, "Invalid numerical data '%s' in tag file", searchstring);
	    message_DisplayString(view,0,find);
	    return;
	}
    } else {
	/* search for tag in source file */
	search_CompilePattern(searchstring, &result);
	position = search_MatchPattern(doc,0,result);
	if (position<0) {
	    message_DisplayString(view,0,"Tag not found in source file -- rebuild tag file!");
	    return;
	}
	length = search_GetMatchLength();
	if (match_bol) {
	    position++;
	    length--;
	}
	if (match_eol)
	    length--;
    }

    if (view == NULL) {
	/* being called by ezapp to open a window */
	view = TagsCreateWindow(filebuffer);
	if (view == NULL) {
	    tag = "Window could not be created.";
	    return;
	}
	*tag = '\0';	/* let caller know it succeeded */
    }

    /* set the filebuffer to be current buffer */ 

    ourFrame=FindByView(view);

    curbuf = frame_GetBuffer(ourFrame);
    curpos = textview_GetDotPosition((struct textview *) view);
    same = (filebuffer == curbuf);

    if (!same)
	frame_SetBuffer(ourFrame, filebuffer, TRUE);

    ourView = frame_GetView(ourFrame);

    textview_SetDotPosition((struct textview *) ourView, position);
    textview_SetDotLength((struct textview *) ourView, length);
    textview_FrameDot((struct textview *) ourView, position);


    if (RecursiveEdit) {
	char bufferName[100];
	message_DisplayString(ourView, 0, "Recursive editing - ^C exits");
	if (!same && curbuf!=NULL)
	    strncpy(bufferName, buffer_GetName(curbuf), sizeof(bufferName));
	else
	    *bufferName= '\0';
	im_KeyboardProcessor();
	if (same) {
	    textview_SetDotPosition((struct textview *) view, curpos);
	    textview_SetDotLength((struct textview *) view, 0);
	    textview_FrameDot((struct textview *) view, curpos);
	} else if (*bufferName && (curbuf= buffer_FindBufferByName(bufferName))!=NULL)
	    frame_SetBuffer(ourFrame,curbuf,TRUE);
	message_DisplayString(view,0,"");
    }
    else
	message_DisplayString(ourView, 0, searchstring);
    /*im_ForceUpdate();*/  /*isn't this bogus? */
}

void tags_GotoTagCmd(view,key)
struct view *view;
long key;
{
    static char name[256];
    int RecursiveEdit= !im_ArgProvided(view_GetIM(view)) && environ_GetProfileSwitch("TagRecursiveEdit", TRUE);

    /* ask for the tag to search for */
    if (message_AskForString(view,0,"Goto tag: ",NULL,name,255))
	return;
    tags_FindTag(view,name,RecursiveEdit);
}

void tags_FindTagCmd(view,key)
struct view *view;
long key;
{
    char *name;
    int RecursiveEdit= !im_ArgProvided(view_GetIM(view)) && environ_GetProfileSwitch("TagRecursiveEdit", TRUE);
    struct text *doc=(struct text *)view->dataobject;

    name = getFunction(doc,textview_GetDotPosition((struct textview *)view));
    tags_FindTag(view,name,RecursiveEdit);
}

void tags_OpenCmd(view,key)
struct view *view;
long key;
{
    char *name;
    int RecursiveEdit;

    if (key != NULL) {
	tags_FindTag(view,(char *)key, FALSE);
    }

}

void tags_LoadTagFileCmd(view,key)
struct view *view;
long key;
{
    struct buffer *buffer, *tbuf;
    static char name[MAXPATHLEN+1];

    checkFileName();
    filetype_CanonicalizeFilename(name,TagsFile,sizeof(name));
    if(completion_GetFilename(view, "Load tag file: ", name, name, sizeof(name), FALSE, FALSE) == -1)
        return;
    else
        TagsFile = name;
    
    tbuf = buffer_FindBufferByName(TBUFNAM);
    buffer = tags_OpenBuffer(TagsFile);

    if (buffer == NULL) {  /* couldn't open, save old buffer */
        message_DisplayString(view,0,"Could not open tag file!");
        return;
    }
    else if (buffer == tbuf) { /* same file as old buffer */
            buffer_ReadFile(buffer,TagsFile);
    }
    else {  /* new file, new buffer */
        if (tbuf) buffer_Destroy(tbuf);
        buffer_SetName(buffer,TBUFNAM);
    }

    message_DisplayString(view,0,"Done.");
}




boolean tags__InitializeClass(classID)
    struct classheader *classID;
{
    struct classinfo *textviewType = class_Load("textview");

    proctable_DefineProc("tags-goto-tag", (procedure) tags_GotoTagCmd, textviewType, NULL, "Goto a given tag.");
    proctable_DefineProc("tags-find-tag", (procedure) tags_FindTagCmd, textviewType, NULL, "Goto the tag whose name is at the dot.");
    proctable_DefineProc("tags-open", (procedure) tags_OpenCmd, textviewType, NULL, "Goto the tag whose name is passed on the command line.");
    proctable_DefineProc("tags-rebuild-tags", (procedure) tags_RebuildTagsFile, textviewType, NULL, "Regenerate the tag file with ctags");
    proctable_DefineProc("tags-load-tag-file", (procedure) tags_LoadTagFileCmd, textviewType, NULL, "Load a tag file from the current directory");
    return TRUE;
}
