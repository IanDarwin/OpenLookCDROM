/*LIBS: -lutil
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/buffer.c,v 2.36 1992/12/15 21:41:42 rr2b R6tape $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 

/*  Fixed problem in buffer_WriteToFile 3/2/90 cch@mtgzx.att.com.

  The problem was that it is creating a temporary filename by appending 
  the string .NEW (and even .<number> if necessary).  If the file is on a 
  Unix V file system, filenames are truncated to 14 characters, and there is 
  no guarantee that the temp filename will be unique.  This sends the routine 
  into an infinite loop. This fix uncovers a bug elsewhere, in which the 
  file can be deleted if the checkpoint filename (when truncated to 14 
  characters ) is the same as the file being saved. In this case the file is 
  deleted as soon as it is stored, with no notice or warning.

  To use short temporary filenames, define USESHORTFILENAMES in your 
  site.h file.
*/

/* I hate AIX.  I hate POSIX. */
#define _ALL_SOURCE

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <andyenv.h>
#include <sys/errno.h>

#include <sys/stat.h>

#include <class.h>
#include <observe.ih>
#include <dataobj.ih>
#include <view.ih>
#include <im.ih>
#include <path.ih>
#include <filetype.ih>
#include <attribs.h>
#include <environ.ih>
#include <message.ih>
#include <bufferlist.ih>

#include <buffer.eh>

#ifndef MAXPATHLEN 
#include <sys/param.h>
#endif

extern int errno;

static struct bufferlist *allBuffers;
static char *backupExtension = NULL;
static char *checkpointExtension = ".CKP";
static char *checkpointDirectory = NULL;
static boolean overwriteFiles = TRUE;
static boolean checkpointGawdyNames = FALSE;

void buffer__SetCheckpointFilename(self)
struct buffer *self;
{
    /* For this routine to work properly buffer names must be unique. */
    /* If they are not then the user might lose some information */

    if (self->ckpFilename != NULL)  {
	free(self->ckpFilename);
    }

    if (checkpointDirectory != NULL) {
#ifdef USESHORTFILENAMES
      if (FALSE)
#else
      if (checkpointGawdyNames)
#endif /* USESHORTFILENAMES */
      {
          int i;
          char tname[MAXPATHLEN];
	  sprintf (tname, "#%d%s#", getuid(), (self->filename ? self->filename : self->bufferName) );
          for (i = 0; i < MAXPATHLEN && tname[i] != '\0'; i++)
              if (tname[i] == '/') tname [i] = '@';
          self->ckpFilename = malloc(strlen(checkpointDirectory) + strlen (tname) + strlen(checkpointExtension) + 2);
          sprintf(self->ckpFilename, "%s/%s%s", checkpointDirectory, tname, checkpointExtension);
      } else {
	char *basename;

	basename = strrchr(self->bufferName, '/');
	if (basename == NULL)  {
	    basename = self->bufferName;
	}
	else  {
	    basename++;
	}
	self->ckpFilename = malloc(strlen(checkpointDirectory) + strlen(basename) + strlen(checkpointExtension) + 2);
	sprintf(self->ckpFilename, "%s/%s%s", checkpointDirectory, basename, checkpointExtension);
      }
    }
    else {
	char *filename = (self->filename != NULL) ? self->filename : self->bufferName;

	self->ckpFilename = malloc(strlen(filename) + strlen(checkpointExtension) + 1);
	strcpy(self->ckpFilename, filename);
	strcat(self->ckpFilename, checkpointExtension);
    }
}


boolean buffer__InitializeObject(classID, self)
    struct classheader *classID;
    struct buffer *self;
{
    self->bufferData = NULL;
    self->bufferName = NULL;
    self->filename = NULL;
    self->ckpFilename = NULL;
    self->viewList = (struct bufferContents *) malloc(sizeof(struct bufferContents) * 3);
    self->viewsAllocated = 3;
    self->viewsUsed = 0;
    self->ckpVersion = self->writeVersion = 0;
    self->ckpClock = 0;
    self->ckpLatency = 0;
    self->lastTouchDate = 0L;
    self->scratch = FALSE;
    self->readOnly = FALSE;
    self->madeBackup = FALSE;
    self->isModified = FALSE;
    self->askedAboutSymlink = FALSE;
    self->clobberSymlink = environ_GetProfileSwitch("ClobberSymlinks", TRUE);
    self->isRawFile = FALSE;
    self->viewname = NULL;
    
    return TRUE;
}

void buffer__FinalizeObject(classID, self)
    struct classheader *classID;
    struct buffer *self;
{
    int counter;

    for (counter = 0; counter < self->viewsUsed; counter++) {
        if (self->viewList[counter].bufferApplicationView != NULL)
            view_DeleteApplicationLayer(self->viewList[counter].bufferView, self->viewList[counter].bufferApplicationView);
        view_Destroy(self->viewList[counter].bufferView);
    }
    free(self->viewList);
    if (self->bufferData) {
	dataobject_RemoveObserver(self->bufferData, self);
	if(self->destroyData) dataobject_Destroy(self->bufferData);
    }
    if (self->filename != NULL)
        free(self->filename);
    if (self->ckpFilename != NULL)
        free(self->ckpFilename);
    if (self->bufferName != NULL)
        free(self->bufferName);
    if(self->viewname != NULL)
	free(self->viewname);
}

/* Changed bufferlist */
struct buffer *buffer__Create(classID, bufferName, filename, objectName, data)
    struct classheader *classID;
    char *bufferName, *filename, *objectName;
    struct dataobject *data;
{
    return bufferlist_CreateBuffer(allBuffers, bufferName, filename, objectName, data);
}

struct view *buffer__GetView(self, inputFocus, targetView, viewName)
    struct buffer *self;
    struct view **inputFocus, **targetView;
    char *viewName;
{
    int counter;

    for (counter = 0; (counter < self->viewsUsed) && self->viewList[counter].used && (viewName == NULL ||
      strcmp(viewName, class_GetTypeName(self->viewList[counter].bufferView)) != 0); counter++)
        ;

    if (counter >= self->viewsUsed) {
        if (self->viewsAllocated <= self->viewsUsed) {
            self->viewList = (struct bufferContents *) realloc(self->viewList, sizeof(struct bufferContents) * (self->viewsAllocated + 3));
            self->viewsAllocated += 3;
        }
	if(viewName == NULL){
	    if(self->viewname != NULL) viewName = self->viewname;
            else viewName = dataobject_ViewName(self->bufferData);
	}
        if ((self->viewList[counter].bufferView = (struct view *) class_NewObject(viewName)) == NULL)
            return NULL;
        view_SetDataObject(self->viewList[counter].bufferView, self->bufferData);
        self->viewList[counter].bufferInputFocus = NULL;
        ++self->viewsUsed;
    }

    self->viewList[counter].bufferApplicationView = view_GetApplicationLayer(self->viewList[counter].bufferView);
    self->viewList[counter].used = TRUE;
    if (inputFocus != NULL)
        *inputFocus = self->viewList[counter].bufferInputFocus;
    if (targetView != NULL)
        *targetView = self->viewList[counter].bufferView;

    return self->viewList[counter].bufferApplicationView;
}

void buffer__RemoveView(self, unusedView)
    struct buffer *self;
    struct view *unusedView;
{
    int counter;

    for (counter = 0; counter < self->viewsUsed; counter++)
        if ((self->viewList[counter].bufferApplicationView == unusedView) && (self->viewList[counter].used == TRUE)) {
            view_UnlinkTree(unusedView); /* Remove it from tree. */
            view_DeleteApplicationLayer(self->viewList[counter].bufferView, unusedView); /* Deallocate application layer to save space. */
            self->viewList[counter].bufferApplicationView = NULL; /* NULL out field for safety. */
            self->viewList[counter].used = FALSE;
            break;
        }
}

boolean buffer__Visible(self)
    struct buffer *self;
{
    int counter;

    for (counter = 0; counter < self->viewsUsed; counter++)
        if (self->viewList[counter].used)
            return TRUE;
    return FALSE;
}

/* Changed bufferlist */
struct buffer *buffer__Enumerate(classID, mapFunction, functionData)
    struct classheader *classID;
    boolean (*mapFunction)();
    long functionData;
{
    return bufferlist_Enumerate(allBuffers, mapFunction, functionData);
}

void buffer__SetData(self, bufferData)
    struct buffer *self;
    struct dataobject *bufferData;
{
    self->bufferData = bufferData;
    dataobject_AddObserver(bufferData, self);
    self->isModified = FALSE;

    self->ckpVersion = self->writeVersion = dataobject_GetModified(bufferData);
    self->destroyData = FALSE;
    if(self->viewname != NULL){
	free(self->viewname);
	self->viewname = NULL;
    }
    buffer_NotifyObservers(self, 0);
}

void buffer__SetName(self, bufferName)
    struct buffer *self;
    char *bufferName;
{
    if (self->bufferName != NULL)
        free(self->bufferName);
    self->bufferName = malloc(strlen(bufferName) + 1);
    strcpy(self->bufferName, bufferName);
    buffer_NotifyObservers(self, 0);
}

void buffer__SetFilename(self, filename)
    struct buffer *self;
    char *filename;
{

    char realName[MAXPATHLEN];
    int len;

    if (self->filename)
        free(self->filename);
    if(filename != NULL && *filename != '\0') {
	filetype_CanonicalizeFilename(realName, filename, sizeof(realName) - 1);
    }
    else {
	realName[0] = '\0';
    }
    filename = realName;
    len = strlen(filename);
    self->filename = malloc(len + 1);
    strcpy(self->filename, filename);
    buffer_SetCheckpointFilename(self);
    self->lastTouchDate = buffer_GetFileDate(self);
    buffer_NotifyObservers(self, 0);/* Tuck it into slot. */
}

void buffer__SetWriteVersion(self, version)
    struct buffer *self;
    long version;
{
    long dobjVersion = dataobject_GetModified(buffer_GetData(self));

    self->writeVersion = version;

    buffer_SetIsModified(self, version < dobjVersion);

    buffer_NotifyObservers(self, 0);/* Tuck it into slot. */
}

void buffer__SetCkpVersion(self, version)
    struct buffer *self;
    long version;
{

    self->ckpVersion = version;
    buffer_NotifyObservers(self, 0);/* Tuck it into slot. */
}

void buffer__SetCkpClock(self, clock)
    struct buffer *self;
    long clock;
{

    self->ckpClock = clock;
}

void buffer__SetCkpLatency(self, latency)
    struct buffer *self;
    long latency;
{
    self->ckpLatency = latency;
}

void buffer__SetScratch(self, scratch)
    struct buffer *self;
    boolean scratch;
{

    self->scratch = scratch;
    buffer_NotifyObservers(self, 0);/* Tuck it into slot. */
}

struct view *buffer__EnumerateViews(self, mapFunction, functionData)
    struct buffer *self;
    procedure mapFunction;
    long functionData;
{

    int counter;
    struct view *value;

    value = NULL;

    for (counter = 0; counter < self->viewsUsed; counter++) {
        if (self->viewList[counter].used) {
	    if ((value = (*((struct view *(*)()) mapFunction)) (self->viewList[counter].bufferApplicationView,
					self->viewList[counter].bufferView,
					self->viewList[counter].bufferInputFocus,
					functionData)) != NULL) {
		return value;
	    }
	}
    }
		
    return NULL;
}

/* Changed bufferlist */
struct buffer *buffer__FindBufferByFile(classID, filename)
    struct classheader *classID;
    char *filename;
{
    return bufferlist_FindBufferByFile(allBuffers, filename);
}

/* Changed Bufferlist */
struct buffer *buffer__FindBufferByData(classID, bufferData)
    struct classheader *classID;
    struct dataobject *bufferData;
{
    return bufferlist_FindBufferByData(allBuffers, bufferData);
}

/* Changed Bufferlist */

struct buffer *buffer__FindBufferByName(classID, bufferName)
    struct classheader *classID;
    char *bufferName;
{
    return bufferlist_FindBufferByName(allBuffers, bufferName);
}

/* This routine is supposed to be used for re-reading a file into a buffer.
 * Unfortunately, at present it is somewhat special cased for use within ez and
 * does not handle reseting all buffer state "intelligently."
 * Refuses to read directory; the caller (framecmds) tries GetBufferOnFile
 * if this call fails.
 */

int buffer__ReadFile(self, filename)
    struct buffer *self;
    char *filename;
{
    long objectID;
    int returnCode = 0;
    char realName[MAXPATHLEN];
    char *objectName;
    FILE *thisFile;
    struct stat stbuf;
    struct attributes *attributes;
    struct attributes *tempAttribute;

    filetype_CanonicalizeFilename(realName, filename, sizeof(realName) - 1);
    filename = realName;

    if (stat(filename, &stbuf) < 0 || (stbuf.st_mode & S_IFMT) == S_IFDIR)
        return -1;

    if ((thisFile = fopen(filename, "r")) == NULL)
            return -1;

    self->lastTouchDate = (long) stbuf.st_mtime;

    objectName = filetype_Lookup(thisFile, filename, &objectID, &attributes);

/* This next thing is a hack. Really need a flags parameter so we can keep
 * the thing readonly if need be.
 */
    self->readOnly = FALSE;
    for (tempAttribute = attributes; tempAttribute != NULL; tempAttribute = tempAttribute->next)
        if (strcmp(tempAttribute->key, "readonly") == 0)
            self->readOnly = tempAttribute->value.integer;

    if (objectName == NULL)
        objectName = "text"; /* The default... */

    if (strcmp(class_GetTypeName(buffer_GetData(self)), objectName) == 0) {

	long version;

        dataobject_SetAttributes(buffer_GetData(self), attributes);
        dataobject_Read(buffer_GetData(self), thisFile, objectID);
        dataobject_NotifyObservers(buffer_GetData(self), 0);
        buffer_SetFilename(self, filename);

	version = dataobject_GetModified(buffer_GetData(self));
        buffer_SetCkpClock(self, 0);
        buffer_SetCkpVersion(self, version);
	buffer_SetWriteVersion(self, version);
    }
    else
        returnCode = -1;

    fclose(thisFile);
    buffer_NotifyObservers(self, 0);/* Tuck it into slot. */
    return returnCode;
}

/* Changed bufferlist */

struct buffer *buffer__GetBufferOnFile(classID, filename, flags)
struct classheader *classID;
char *filename;
long flags;
{
    return bufferlist_GetBufferOnFile(allBuffers, filename, flags);
}

/* Changed bufferlist */

void buffer__GuessBufferName (classID, filename, bufferName, nameSize)
    struct classheader *classID;
    char *filename, *bufferName;
    int nameSize;
{
    bufferlist_GuessBufferName(allBuffers, filename, bufferName, nameSize);
}

void buffer__GetUniqueBufferName (classID, proposedBufferName, bufferName, nameSize)
    struct classheader *classID;
    char *proposedBufferName, *bufferName;
    int nameSize;
{
    bufferlist_GuessBufferName(allBuffers, proposedBufferName, bufferName, nameSize);
}

extern char *sys_errlist[];

static int ResolveLink(linkname, buffer)
char *linkname, *buffer;
{
    struct stat sbuf;
    char name[MAXPATHLEN];
    int len;

    strcpy(name, linkname);
    do {
	if ((len = readlink(name, buffer, MAXPATHLEN)) <= 0) {
	    return -1;
	}
	buffer[len] = '\0';
	strcpy(name, buffer);
	if (lstat(name, &sbuf) < 0) {
	    return -1;
	}
    } while ((sbuf.st_mode & S_IFMT) == S_IFLNK);
    if (*buffer != '/') {	/* It's a relative link.  Make it absolute. */
	char *dirname = strrchr(linkname, '/');

	if (dirname) {
	    strncpy(name, linkname, dirname - linkname + 1);
	    strcpy(&(name[0]) + (dirname - linkname + 1), buffer);
	    filetype_CanonicalizeFilename(buffer, name, MAXPATHLEN);
	}
    }
    return 0;
}

int buffer__WriteToFile(self, filename, flags)
    struct buffer *self;
    char *filename;
    long flags;
{

    char realName[MAXPATHLEN], linkdest[MAXPATHLEN];
    char tempFilename[MAXPATHLEN];
    char shortName[65], shortLinkdest[129];
    char *originalFilename = NULL;
    int closeCode;
    int errorCode;
    int originalMode;
    int fd;
    FILE *outFile;
    struct stat statBuf;
    boolean makeBackup = !buffer_GetMadeBackup(self);
    boolean differentFile = FALSE;
    boolean fileExists = TRUE;
    boolean alreadyAsked = FALSE;
    boolean isSymlink = FALSE;

    errorCode = 0;
    *linkdest = '\0';
    if (filename == NULL) {
	if ((filename = self->filename) == NULL)
	    return -1;
    }
    else {
	filetype_CanonicalizeFilename(realName, filename, sizeof(realName) - 1);
	filename = realName;
	if ((self->filename == NULL) || (strcmp(filename, self->filename) != 0)) { /* If writing to a different file than normal. */
	    differentFile = TRUE;
	    makeBackup = TRUE;
	}
    }

    if (access(filename, W_OK) < 0) {
	if (errno == EACCES) {
	    return -1;
	} else if (errno == ENOENT) {
	    fileExists = makeBackup = FALSE;
	}
    }

    if (lstat(filename, &statBuf) >= 0) {
	if ((statBuf.st_mode & S_IFMT) == S_IFLNK) {
	    isSymlink = TRUE;
	    if (environ_GetProfileSwitch("AskAboutSymlinks", FALSE)
		&& !(self->askedAboutSymlink)) {
		char prompt[100 + MAXPATHLEN];
		long choice;

		path_TruncatePath(filename, shortName,
				  (sizeof (shortName)) - 1, TRUE);
		if (ResolveLink(filename, linkdest)) {
		    static char *choices[] = {
			"Replace link with file contents",
			"Cancel",
			NULL
		    };

		    sprintf(prompt, "%s is an unresolvable symbolic link",
			    shortName);
		    if (message_MultipleChoiceQuestion(NULL, 80, prompt, 0,
						       &choice, choices,
						       "rc") < 0)
			return -1;
		    if (choice == 1)
			return -1;
		    self->clobberSymlink = TRUE;
		    self->askedAboutSymlink = TRUE;
		    alreadyAsked = TRUE;
		}
		else {
		    static char *choices[] = {
			"Replace link with file contents",
			"Follow link, replacing pointed-to file",
			"Cancel",
			NULL
		    };

		    path_TruncatePath(linkdest, shortLinkdest,
				      (sizeof (shortLinkdest)) - 1, TRUE);
		    sprintf(prompt, "%s is a symbolic link pointing to %s",
			    shortName, shortLinkdest);
		    if (message_MultipleChoiceQuestion(NULL, 80,
						       prompt,
						       (self->clobberSymlink ?
							0 : 1),
						       &choice,
						       choices,
						       "roc") < 0) {
			return -1;
		    }
		    if (choice == 2)
			return -1;
		    self->clobberSymlink = (choice == 0);
		    self->askedAboutSymlink = TRUE;
		    alreadyAsked = TRUE;
		}
	    }
	    if (!(self->clobberSymlink)) {
		if (*linkdest == '\0') {
		    if (ResolveLink(filename, linkdest)) {
			return -1;
		    }
		}
		if (stat(linkdest, &statBuf) < 0) {
		    return -1;
		}
	    }
	    originalMode = statBuf.st_mode & (~S_IFMT);
	} else {
	    originalMode = statBuf.st_mode & (~S_IFMT);
	}
    }
    else
	originalMode = 0666; /* Default open mode. */

    if (!overwriteFiles && differentFile && makeBackup && !alreadyAsked) {
        char prompt[MAXPATHLEN + sizeof("``'' already exists. Overwrite? ")];
        char answer[5];

        sprintf(prompt, "``%s'' already exists. Overwrite? ", filename);

        if (message_AskForString(NULL, 0, prompt, NULL, answer, sizeof(answer)) < 0 || (answer[0] != 'y' && answer[0] != 'Y'))
            return -1;
    }

    if ((flags & buffer_MakeBackup) && backupExtension && makeBackup) {
	strcpy(tempFilename, filename);
	strcat(tempFilename, backupExtension);
	if (isSymlink && !(self->clobberSymlink)) {
	    FILE *infp, *outfp;
	    int c;

	    if (!(infp = fopen(filename, "r"))
		|| !(outfp = fopen(tempFilename, "w"))) {
		return -1;
	    }
	    while ((c = fgetc(infp)) != EOF)
		fputc(c, outfp);
	    fclose(infp);
	    fclose(outfp);
	} else {
	    if ((rename(filename, tempFilename) < 0) && errno != ENOENT)
		return -1;
	}
	buffer_SetMadeBackup(self, TRUE);
    }
    else if ((flags & buffer_ReliableWrite) && fileExists) {
	char *endString;
	int counter = 1;

#ifdef USESHORTFILENAMES
	char *basename;
#endif /* USESHORTFILENAMES */

	if (isSymlink && !(self->clobberSymlink)) {
	    if (*linkdest == '\0') {
		if (ResolveLink(filename, linkdest)) {
		    return -1;
		}
	    }
	    originalFilename = linkdest;
	    filename = linkdest;
	}
#ifndef USESHORTFILENAMES
	strcpy(tempFilename, filename);
	strcat(tempFilename, ".NEW");
	endString = tempFilename + strlen(tempFilename);
	while (access(tempFilename, F_OK) >= 0) /* While the file exists. */
	    sprintf(endString, ".%d", counter++);
#else /* USESHORTFILENAMES */
	strcpy(tempFilename, filename);
	basename = strrchr(tempFilename, '/');
	if (basename == NULL) basename = tempFilename;
	else basename++;
	if (strlen(basename) > 8) basename[8] = '\0';
	strcat(tempFilename, ".NEW");
	endString = tempFilename + strlen(tempFilename);
	while (access(tempFilename, F_OK) >= 0 && counter < 10) /* While the file exists. */
	    sprintf(endString, ".%d", counter++);
	if (counter == 10) return -1;
#endif /* USESHORTFILENAMES */
	if (!originalFilename)
	    originalFilename = filename;
	filename = tempFilename;
    }
    else {
	if (isSymlink && !(self->clobberSymlink)) {
	    if (*linkdest == '\0') {
		if (ResolveLink(filename, linkdest)) {
		    return -1;
		}
	    }
	    filename = linkdest;
	}
    }

    if ((fd = open(filename, O_WRONLY | O_TRUNC | O_CREAT, originalMode)) < 0
	 || (outFile = fdopen(fd, "w")) == NULL)
	return -1;
    dataobject_Write(self->bufferData, outFile, im_GetWriteID(), 0);
    fflush(outFile);

/* This next test is somewhat bogus. In theory, if an error occured while
 * writing the file, this will catch it. In practice, dataobjects are not
 * required to use stdio to write objects and stdio also seems to lose error
 * indications on occasion. In any event, we assume that if the error has
 * occured, there is no point in using vclose to close the file and we
  * preserve the original file. The code here actually simulates the error
  * return (with an unknown error 0 since stdio doesn't give any indication of
  * what actually went wrong.
  */
    if (ferror(outFile)) {
	fclose(outFile);
	errorCode = 0;
	closeCode = -1;
    }
    else {
	/* Now for GNU-Emacs compatibility, we chmod the file. */
	/* This is so that we preserve the modes of the original */
	/* file, un-modified by umask. */
	if (fileExists) chmod(filename, originalMode);
#ifdef AFS_ENV
	if (flags & buffer_ReliableWrite) { /* Go for the expensive but safe operation. */
	    if ((closeCode = vclose(fileno(outFile))) < 0) /* stdio can trash errno. */
		errorCode = errno; /* Protect it from the fclose below. */
	    else
		if (originalFilename != NULL)
		    if ((closeCode = rename(filename, originalFilename)) < 0)
			errorCode = errno;
	}
	else /* Fast and loose. */
	    if ((closeCode = close(fileno(outFile))) < 0) /* stdio can trash errno. */
		errorCode = errno; /* Protect it from the fclose below. */
#else /* AFS_ENV */
	if ((closeCode = close(fileno(outFile))) < 0) /* stdio can trash errno. */
	    errorCode = errno; /* Protect it from the fclose below. */
	else
	    if (originalFilename != NULL)
		if ((closeCode = rename(filename, originalFilename)) < 0)
		    errorCode = errno;
#endif /* AFS_ENV */
	fclose(outFile); /* Free stdio resources. */
#if 0
	/* This code resets the readonly flag based on the file
	 * we just wrote.  Often the file is a checkpoint of the
	 * buffer, so this may set the buffer to read/write
	 * by accident.  The fix:  comment out this code and let
	 * the caller reset the readonly flag.
	 */
	if (closeCode >= 0) { /* Reset readonly mode. */

	    struct attributes attributes;

	    attributes.next = NULL;
	    attributes.key = "readonly";
	    if (access(filename, W_OK) == -1 && errno == EACCES)
		attributes.value.integer = self->readOnly = TRUE;
	    else
		attributes.value.integer = self->readOnly = FALSE;
	    dataobject_SetAttributes(buffer_GetData(self), &attributes);
	}
#endif
    }
    errno = errorCode;
/* Get a fresh stat() on the file after we've now written it. */
    if(differentFile == FALSE){
	/* don't modify if writing to another file */
	self->lastTouchDate = buffer_GetFileDate(self);
    }
    return closeCode;
}

long buffer__GetFileDate(self)
struct buffer *self;
{
    struct stat stbuf;
    if (self->filename == NULL)
        return 0L;
    if (stat(self->filename, &stbuf) < 0)
        return 0L;
    return (long) stbuf.st_mtime;
}

boolean buffer__InitializeClass(classID)
    struct classheader *classID;
{
    char *s;

    if ((s = environ_GetProfile("BackupExtension")) != NULL) {
        backupExtension = malloc(strlen(s) + 1);
        strcpy(backupExtension, s);
    }

    if ((s = environ_GetProfile("CheckpointExtension")) != NULL) {
        checkpointExtension = malloc(strlen(s) + 1);
        strcpy(checkpointExtension, s);
    }

    if ((s = environ_GetProfile("CheckpointDirectory")) != NULL) {
        checkpointDirectory = malloc(strlen(s) + 1);
        strcpy(checkpointDirectory, s);
    }

    overwriteFiles = environ_GetProfileSwitch("OverwriteFiles", overwriteFiles);
    checkpointGawdyNames = environ_GetProfileSwitch("CheckpointGawdyNames", FALSE);

    allBuffers = bufferlist_New();
    return TRUE;
}

/* Changed BufferList */

void buffer__SetDefaultObject(classID, objectname)
struct classheader *classID;
char *objectname;
{
    bufferlist_SetDefaultObject(allBuffers, objectname);
}

void buffer__SetDefaultViewname(self,name)
struct buffer *self;
char *name;
{
    if(self->viewname != NULL)
	free(self->viewname);
    if(name == NULL) self->viewname = NULL;
    else {
	self->viewname = malloc(strlen(name) + 1);
	if(self->viewname == NULL) return;
	strcpy(self->viewname, name);
    }
}

void buffer__SetDestroyData(self, destroy)
    struct buffer *self;
    boolean destroy;
{
    self->destroyData = destroy;
}

void buffer__SetLastTouchDate(self, dateStamp)
    struct buffer *self;
    long dateStamp;
{
    self->lastTouchDate = dateStamp;;
}

void buffer__SetReadOnly(self, readOnly)
    struct buffer *self;
    boolean readOnly;
{
    struct attributes attributes;

    attributes.next = NULL;
    attributes.key = "readonly";
    attributes.value.integer = readOnly;
    dataobject_SetAttributes(buffer_GetData(self), &attributes);
    self->readOnly = readOnly;
}

struct bufferlist *buffer__GetGlobalBufferList(classID)
    struct classheader *classID;
{
    return allBuffers;
}

void buffer__ObservedChanged(self, object, value)
struct buffer *self;
struct observable *object;
long value;
{
   if (value == observable_OBJECTDESTROYED) {
	/* this makes little to no sense.
    observable_RemoveObserver(object, self); */
    }
    else if (! buffer_GetIsModified(self)) {
	buffer_SetIsModified(self, TRUE);
    }
}


void buffer__SetIsModified(self, value)
struct buffer *self;
boolean value;
{
    self->isModified = value;
    buffer_NotifyObservers(self, observable_OBJECTCHANGED);
}

void buffer__SetIsRawFile(self, value)
struct buffer *self;
boolean value;
{
    self->isRawFile = value;
    buffer_NotifyObservers(self, observable_OBJECTCHANGED);
}
