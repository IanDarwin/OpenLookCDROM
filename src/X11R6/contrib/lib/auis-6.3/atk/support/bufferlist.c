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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/bufferlist.c,v 1.12 1994/03/03 05:29:30 rr2b Exp $";
#endif


 

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <class.h>
#include <sys/errno.h>
#include <sys/stat.h>

#include <observe.ih>
#include <dataobj.ih>
#include <view.ih>
#include <im.ih>
#include <filetype.ih>
#include <attribs.h>
#include <environ.ih>
#include <message.ih>
#include <buffer.ih>

#include <bufferlist.eh>

#ifndef MAXPATHLEN 
#include <sys/param.h>
#endif
#define DEFAULTOBJECT "text"

struct listentry {
    struct buffer *buffer;
    struct listentry *next;
};

extern int errno;
static char defaultobjectname[64] = DEFAULTOBJECT;

boolean bufferlist__InitializeObject(classID, self)
    struct classheader *classID;
    struct bufferlist *self;
{
    self->head = NULL;

    return TRUE;
}

void bufferlist__FinalizeObject(classID, self)
    struct classheader *classID;
    struct bufferlist *self;
{
    struct listentry *traverse;
    struct listentry *next;

    for (traverse = self->head; traverse != NULL; traverse = next) {
	next = traverse->next;
	free(traverse);
    }
}

void bufferlist__ObservedChanged(self, object, changeType)
struct bufferlist *self;
struct observable *object;
long changeType;
{
    if (changeType == observable_OBJECTDESTROYED) {
	struct buffer *buffer = (struct buffer *) object;

	buffer_RemoveObserver(buffer, self);
	bufferlist_RemoveBuffer(self, buffer);
	bufferlist_NotifyObservers(self, observable_OBJECTCHANGED);
    }
}

void bufferlist__AddBuffer(self, buffer)
    struct bufferlist *self;
    struct buffer *buffer;
{
    struct listentry *newElement;
    struct listentry *traverse;

    for (traverse = self->head; traverse != NULL && traverse->buffer != buffer; traverse = traverse->next) {
    }

    if (traverse == NULL) {
	newElement = (struct listentry *) malloc(sizeof(struct listentry));
	if (newElement != NULL) {
	    newElement->buffer = buffer;
	    newElement->next = self->head;
	    self->head = newElement;
	    buffer_AddObserver(buffer, self);
	    bufferlist_NotifyObservers(self, observable_OBJECTCHANGED);
	}
    }
}

void bufferlist__RemoveBuffer(self, buffer)
    struct bufferlist *self;
    struct buffer *buffer;
{
    struct listentry **previous = &self->head;
    struct listentry *traverse;

    for (traverse = self->head; traverse != NULL && traverse->buffer != buffer; traverse = traverse->next) {
	previous = &traverse->next;
    }

    if (traverse != NULL) {    /* Bad error if this is false. */
        *previous = traverse->next;
	bufferlist_NotifyObservers(self, observable_OBJECTCHANGED);
	free(traverse);
    }
}

struct buffer *bufferlist__CreateBuffer(self, bufferName, filename, objectName, data)
    struct bufferlist *self;
    char *bufferName, *filename, *objectName;
    struct dataobject *data;
{
    char realName[MAXPATHLEN];
    struct buffer *thisBuffer;

/* Probably ought to insure that buffer names are unique. */
    thisBuffer = buffer_New();

    buffer_SetName(thisBuffer, bufferName);

    if (filename != NULL) {
        filetype_CanonicalizeFilename(realName, filename, sizeof(realName) - 1);
	filename = realName;

	buffer_SetFilename(thisBuffer, filename);
    }

    buffer_SetCheckpointFilename(thisBuffer);

    if (data == NULL) {
	struct attributes *attributes = NULL;

	if (objectName == NULL)  {
	    if (filename != NULL)
		objectName = filetype_Lookup(NULL, filename, NULL, &attributes);
	    else
		objectName = filetype_Lookup(NULL, bufferName, NULL, &attributes);
	    if (objectName == NULL)
		objectName = defaultobjectname;
	}
	if(!class_IsTypeByName(objectName, "dataobject")) {
	    fprintf(stderr, "Warning: bad dataobject class name %s substituting %s.\n", objectName, defaultobjectname);
	    objectName = defaultobjectname;
	}
        if ((data = (struct dataobject *) class_NewObject(objectName)) == NULL) {
            buffer_Destroy(thisBuffer);
            return NULL;
        }
        if (attributes != NULL) {
	    dataobject_SetAttributes(data, attributes);
	}
	buffer_SetData(thisBuffer, data);
	buffer_SetDestroyData(thisBuffer, TRUE);
    }
    else {
	buffer_SetData(thisBuffer, data); 
    }

    buffer_SetCkpVersion(thisBuffer, dataobject_GetModified(data));
    buffer_SetWriteVersion(thisBuffer, dataobject_GetModified(data));
    buffer_SetLastTouchDate(thisBuffer, buffer_GetFileDate(thisBuffer));

    bufferlist_AddBuffer(self, thisBuffer);

    return thisBuffer;
}

struct buffer *bufferlist__Enumerate(self, mapFunction, functionData)
    struct bufferlist *self;
    boolean (*mapFunction)();
    long functionData;
{
    struct listentry *traverse, *next;

    for (traverse = self->head; traverse != NULL; traverse = next) {
        next = traverse->next; /* So mapFunction is allowed to delete the buffer. */
        if ((*mapFunction)(traverse->buffer, functionData))
            return traverse->buffer;
    }
    return NULL;
}

struct buffer *bufferlist__FindBufferByFile(self, filename)
    struct bufferlist *self;
    char *filename;
{
    char realName[MAXPATHLEN];
    char *bufferFilename;
    struct listentry *traverse;

    filetype_CanonicalizeFilename(realName, filename, sizeof(realName) - 1);
    
    for (traverse = self->head; traverse != NULL; traverse = traverse->next) {
	bufferFilename = buffer_GetFilename(traverse->buffer);
	if (bufferFilename != NULL && (strcmp(bufferFilename, realName) == 0)) {
	    return traverse->buffer;
	}
    }

    return NULL;
}

struct buffer *bufferlist__FindBufferByData(self, bufferData)
    struct bufferlist *self;
    struct dataobject *bufferData;
{
    struct listentry *traverse;

    for (traverse = self->head; traverse != NULL; traverse = traverse->next) {
	if (buffer_GetData(traverse->buffer) == bufferData) {
	    return traverse->buffer;
	}
    }
    return NULL;
}

/* Changed Bufferlist */

struct buffer *bufferlist__FindBufferByName(self, name)
    struct bufferlist *self;
    char *name;
{
    char *bufferName;
    struct listentry *traverse;

    for (traverse = self->head; traverse != NULL; traverse = traverse->next) {
	bufferName = buffer_GetName(traverse->buffer);
	if (bufferName != NULL && (strcmp(bufferName, name) == 0)) {
	    return traverse->buffer;
	}
    }

    return NULL;
}

/* The interaction between this routine and buffer_Create is treacherous.
 * I have buffer_Create allocate the data object so buffer_Destroy will
 * deallocate it.  Yet this routine must look up the class of the data
 * object because it has the FILE *. Perhaps this is all bogus...
 */

struct buffer *bufferlist__GetBufferOnFile(self, filename, flags)
struct bufferlist *self;
char *filename;
long flags;
{
    char realName[MAXPATHLEN];
    struct buffer *thisBuffer;
    struct stat statBuf;
    boolean fileExists, fileIsDir;
    long objectID;
    long version;
    boolean readOnly;
    char bufferName[100], *objectName;
    FILE *thisFile;
    struct attributes *attributes, *tempAttribute, readOnlyAttribute, rawModeAttribute;

    filetype_CanonicalizeFilename(realName, filename, sizeof (realName) - 1);
    filename = realName;

    fileExists = fileIsDir = FALSE;
    if (stat(filename, &statBuf) >= 0) {
	fileExists = TRUE;
	if ((statBuf.st_mode & S_IFMT) == S_IFDIR) {
	    fileIsDir = TRUE;
	}
    }


    if (fileIsDir && ((flags & buffer_RawMode) == 0)) {
	struct attributes attr;
	struct dataobject *dobj;
	/* Force filename to not end in '/' before visiting directories. */
	/* Do this so there aren't two different buffers on the same dir */
	/* with different names; one with slash, one without */
	if (filename[strlen(filename) - 1] == '/')
	    filename[strlen(filename) - 1] = '\0';
	objectName = "dired";
	/* Use existing dired buffer and dired if exists */
	bufferlist_GuessBufferName(self, filename, bufferName, sizeof(bufferName));
	thisBuffer = bufferlist_FindBufferByName(self, bufferName);
	if (thisBuffer) {
	    /* In case it exists by accident */
	    dobj = buffer_GetData(thisBuffer);
	    if (strcmp(class_GetTypeName(dobj), objectName) != 0)
		thisBuffer = NULL;
	} else {
	    /* Create a new dired and dired buffer */
	    if ((flags & buffer_MustExist) && ! fileExists)
		return NULL;
	    dobj = (struct dataobject *) class_NewObject(objectName);
	    if (dobj == NULL)
		thisBuffer = NULL;
	    else
		thisBuffer = bufferlist_CreateBuffer(self, bufferName, NULL, NULL, dobj);
	}
	if (thisBuffer == NULL) {
	    errno = EISDIR;
	    return NULL;
	}
	/* Tell it which dir to use */
	attr.key = "dir";
	attr.value.string = filename;
	attr.next = NULL;
	dataobject_SetAttributes(dobj, &attr);
	buffer_SetFilename(thisBuffer, NULL);
    } else {

	if ((flags & buffer_MustExist) && ! fileExists) {
	    return NULL;
	}

	/* Try to find existing buffer. */

	if ((flags & buffer_ForceNew) == 0)
	    if ((thisBuffer = bufferlist_FindBufferByFile(self, filename)) != NULL)
		return thisBuffer;

	if ((thisFile = fopen(filename, "r")) == NULL) {
	    if (access(filename, W_OK) < 0) {
		char *slash;
		if (errno != ENOENT)
		    return NULL;
		slash = rindex(filename, '/');
		if (slash == NULL)
		    return NULL;
		*slash = '\0';
		if (access(filename, W_OK) < 0) {
		    *slash = '/'; return NULL;
		}
		*slash = '/';
	    }
	}

	if ((flags & buffer_RawMode) != 0) {
	    objectName = "text";
	    objectID = 0;
	    attributes = &rawModeAttribute;
	    rawModeAttribute.next = NULL;
	    rawModeAttribute.key = "datastream";
	    rawModeAttribute.value.string = "no";
	    readOnly = TRUE;
	}
	else {
	    objectName = filetype_Lookup(thisFile, filename, &objectID, &attributes);
	}

	readOnly = FALSE;
	for (tempAttribute = attributes; tempAttribute != NULL; tempAttribute = tempAttribute->next)
	    if (strcmp(tempAttribute->key, "readonly") == 0)
		readOnly = tempAttribute->value.integer;

	if (!readOnly && (flags & buffer_ReadOnly)) {
	    readOnlyAttribute.next = attributes;
	    readOnlyAttribute.key = "readonly";
	    readOnlyAttribute.value.integer = TRUE;
	    attributes = &readOnlyAttribute;
	    readOnly = TRUE;
	}

	bufferlist_GuessBufferName(self, filename, bufferName, sizeof(bufferName));
	thisBuffer = bufferlist_CreateBuffer(self, bufferName, filename, objectName, NULL);

	if (thisBuffer == NULL) {
	    errno = 0;      /* Don't signal Unix error unless one occurred */
	    return NULL;
	}

	dataobject_SetAttributes(buffer_GetData(thisBuffer), attributes);
	if (thisFile != NULL) {
	    dataobject_Read(buffer_GetData(thisBuffer), thisFile, objectID);
	    fclose(thisFile);
	    buffer_SetLastTouchDate(thisBuffer, (long) statBuf.st_mtime);
	}

	buffer_SetReadOnly(thisBuffer, readOnly);
    }

    version = dataobject_GetModified(buffer_GetData(thisBuffer));
    buffer_SetCkpClock(thisBuffer, 0);
    buffer_SetCkpVersion(thisBuffer, version);
    buffer_SetWriteVersion(thisBuffer, version);
    buffer_SetIsRawFile(thisBuffer, (flags & buffer_RawMode) != 0);
    return thisBuffer;
}

void bufferlist__GetUniqueBufferName(self, proposedName, bufferName, nameSize)
struct bufferlist *self;
char *proposedName;
char *bufferName;
int nameSize;
{
    register int uniquefier, nameLength;

    strcpy(bufferName, proposedName);

/* Find out if buffer exists. */
    if (buffer_FindBufferByName(bufferName) == NULL)
        return;

/* Otherwise we must uniquify it.
 * This is a bug, it may overflow the string buffer we were given...
 */
    nameLength = strlen(bufferName);
    for (uniquefier = 1; uniquefier < 100; uniquefier++) {
        sprintf(bufferName + nameLength, "-%d", uniquefier);
        if (buffer_FindBufferByName(bufferName) == NULL)
            return;
    }
    *bufferName = '\0'; /* Make sure we don't return a non-unique buffername. */
    return;
}

void bufferlist__GuessBufferName (self, filename, bufferName, nameSize)
    struct bufferlist *self;
    char *filename, *bufferName;
    int nameSize;
{
    char *slash;
    char newBufferName[MAXPATHLEN];

    slash = rindex(filename, '/');
    if (slash != NULL)
        strncpy(newBufferName, ++slash, nameSize - 3); /* Save room for uniquefier. */
    else
	strncpy(newBufferName, filename, nameSize - 3); /* Save room for uniquefier. */

    newBufferName[nameSize-3] = '\0';

    bufferlist_GetUniqueBufferName(self, newBufferName, bufferName, nameSize);

}


void bufferlist__SetDefaultObject(self, objectname)
struct bufferlist *self;
char *objectname;
{
    if (objectname != NULL)
        strncpy(defaultobjectname,objectname, sizeof(defaultobjectname));
    else
        strncpy(defaultobjectname, DEFAULTOBJECT, sizeof(defaultobjectname));
}
