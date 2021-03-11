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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/filetype.c,v 2.16 1993/01/07 21:08:51 rr2b R6tape $";
#endif


 

#include <andrewos.h> /* sys/types.h sys/file.h */
#include <stdio.h>
#include <class.h>
#include <sys/param.h>
#include <sys/errno.h>
#include <sys/stat.h>
#include <ctype.h>
#include <pwd.h>

/* #include <im.ih> */
#include <attribs.h>
#include <environ.ih>
#include <path.ih>
#include <filetype.eh>

#define DEFAULTTYPE "text"

static struct mapEntry {
    struct mapEntry *next;
    char *fileExtension;
    char *dataName;
    struct attributes *newAttributes;
    struct attributes *existingAttributes;
} *allEntries = NULL, defaultMapping = {NULL, NULL, NULL, NULL, NULL};

extern int errno;

/* These next statics are fairly bogus. */
static char lastFilename[MAXPATHLEN];
static char lastExtension[32]; /* Extensions > 32 char's are trunc'ed. */

/* Some static space for holding file dependent attributes (as opposed to
 * extension specific ones).
 */
static struct attributes hardWiredAttributes[4];

#define SIZEATTRINDEX 0 /* Where in above array to put file's size. */
#define NAMEATTRINDEX 1 /* Where to put filename attribute. */
#define EXTATTRINDEX  2 /* Where to put file's extension. */
#define READONLYINDEX 3 /* Where to put the readonly flag. */

static void FreeAttributes(attributes)
struct attributes *attributes;
{
    struct attributes *thisAttr, *nextAttr;

    for (thisAttr = attributes; thisAttr != NULL; thisAttr = nextAttr) {
        free(thisAttr->key);
        if (thisAttr->value.string != NULL) /* Attributes on this list are guaranteed to be string attributes... */
            free(thisAttr->value.string);
        nextAttr = thisAttr->next;
        free(thisAttr);
    }
}

static struct mapEntry *GetEntry(extension, dataName)
char *extension;
char *dataName;
{
    register struct mapEntry *thisEntry;

    if (strcmp(extension, "*") == 0) {
        thisEntry = &defaultMapping;
	defaultMapping.fileExtension = "*";
    }
    else {
        for (thisEntry = allEntries; thisEntry != NULL && strcmp(thisEntry->fileExtension, extension); thisEntry = thisEntry->next)
	    ;
    }

    if (thisEntry == NULL) { /* Allocate an entry if we didn't find one. */
        thisEntry = (struct mapEntry *) malloc(sizeof(struct mapEntry));
        thisEntry->fileExtension = (char *) malloc(strlen(extension) + 1);
	strcpy(thisEntry->fileExtension, extension);
	thisEntry->dataName = NULL;
	thisEntry->newAttributes = NULL;
	thisEntry->existingAttributes = NULL;
        thisEntry->next = allEntries;
        allEntries = thisEntry;
    }

/* Fill in name. */
    if (thisEntry->dataName != NULL)
	free(thisEntry->dataName);
    thisEntry->dataName = (char *) malloc(strlen(dataName) + 1);
    strcpy(thisEntry->dataName, dataName);
    
    return thisEntry;
}

struct attributes *ParseAttributes(attributes)
char *attributes;
{
/* If necessary, parse the colon seperated string of assignments to generate the
 * values list. The string looks like "key1=value1;key2=value2;...". All values
 * that result from this parsing are string values, integer values are never
 * generated. The only way integer values show up at present is in the hardwired
 * attributes from filetype_Lookup below.
 *
 * This function does no whitespace parsing in order to be "literal" about the
 * values it generates.
 */

    struct attributes *attr = NULL;

    if (attributes != NULL) {
        char *thisKey = attributes, *thisVal;
        struct attributes *thisAttr;

        while (*thisKey != '\0') {
            for (thisVal = thisKey; *thisVal != '=' && *thisVal != '\0'; ++thisVal)
                ; /* Skip to find value */
            if (thisVal != thisKey) { /* If we found something. */
                thisAttr = (struct attributes *) malloc(sizeof(struct attributes));
                thisAttr->key = (char *) malloc(thisVal - thisKey + 1);
                strncpy(thisAttr->key, thisKey, thisVal - thisKey);
                thisAttr->key[thisVal - thisKey] = '\0';
                if (*thisVal != '\0') /* Guaranteed to be either '=' or '\0' */
                    thisVal++;
                for (thisKey = thisVal; *thisKey != ';' && *thisKey != '\0'; ++thisKey)
                    ;
                if (thisKey != thisVal) {
                    thisAttr->value.string = (char *) malloc(thisKey - thisVal + 1);
                    strncpy(thisAttr->value.string, thisVal, thisKey - thisVal);
                    thisAttr->value.string[thisKey - thisVal] = '\0';
                }
                else
                    thisAttr->value.string = NULL;
                thisAttr->next = attr;
                attr = thisAttr;
                if (*thisKey == ';')
                    ++thisKey;
            }
	    else /* Else, guarantee the loop will terminate. */
		if (*thisKey != '\0')
		    ++thisKey;
        }
    }

    return attr;
}

void filetype__AddEntry(classID, extension, dataName, attributes)
    struct classheader *classID;
    char *extension, *dataName;
    char *attributes;
{
    struct mapEntry *thisEntry;

    thisEntry = GetEntry(extension, dataName);

    FreeAttributes(thisEntry->newAttributes);
    thisEntry->newAttributes = ParseAttributes(attributes); 

}

void filetype__AddExistingAttributes(classID, extension, dataName, attributes)
    struct classheader *classID;
    char *extension, *dataName;
    char *attributes;
{
    struct mapEntry *thisEntry;

    thisEntry = GetEntry(extension, dataName);

    FreeAttributes(thisEntry->existingAttributes);
    thisEntry->existingAttributes = ParseAttributes(attributes); 
}

int filetype__DeleteEntry(classID, extension)
    struct classheader *classID;
    register char *extension;
{

    register struct mapEntry *traverse, **previous;

    if (strcmp(extension, "*")) {
        if (defaultMapping.dataName)
            free(defaultMapping.dataName);
	FreeAttributes(&defaultMapping.newAttributes);
	defaultMapping.newAttributes = NULL;
	FreeAttributes(&defaultMapping.existingAttributes);
	defaultMapping.existingAttributes = NULL;
        defaultMapping.fileExtension = NULL;
        return 1;
    }
    previous = &allEntries;
    for (traverse = allEntries; traverse != NULL; traverse = traverse->next) {
        if (strcmp(traverse->fileExtension, extension) == 0) {
            free(traverse->fileExtension);
            free(traverse->dataName);
            FreeAttributes(traverse->newAttributes);
            FreeAttributes(traverse->existingAttributes);
            *previous = traverse->next;
            free(traverse);
            return 1;
        }
        previous = &traverse->next;
    }
    return 0;
}

/* NOTE: filetype_Lookup.
 *    The value returned through the attributes parameter to this function
 *    contains pointers to static storage. Its value should be used before
 *    the next call to this routine. If this can't be guaranteed, the
 *    programmer must copy the attributes list.
 */
char *filetype__Lookup(classID, file, filename, objectID, attributes)
    struct classheader *classID;
    FILE *file;
    register char *filename;
    long *objectID;
    struct attributes **attributes;
{

    register struct mapEntry *thisEntry;
    register char *extension;
    static char objectName[100]; /* The place to put the name of the class that should be used to read this file. */
    char *targetObject = NULL; /* Holds potential value for objectName. */
    struct attributes *newAttributes = NULL; /* Only used if the file is in non-datastream format. */
    struct attributes *existingAttributes = NULL;

    if (attributes)
        *attributes = NULL; /* In case we don't set it otherwise. */
/* First, if possible, do the lookup on the filename extension
 * to get the attributes and possibly the object (class) type.
 * This information may be overrriden by that in the file.
 */
    if (filename != NULL && filename[0] != '\0') {
        register char *s = rindex(filename, '/');
	char prefix[100];

	if (s != NULL) {
	    char *dotpos;
	    strcpy(prefix, s+1);
	    dotpos = index(prefix, '.');
	    if (dotpos != NULL) *(dotpos+1) = '\0';
	}
	else prefix[0] = '\0';

        extension = rindex(filename, '.');
        if (extension == NULL || (s != NULL && s > extension))
            extension = "";

	for(thisEntry = allEntries; thisEntry != NULL; thisEntry = thisEntry->next) {
	    if (!strcmp(thisEntry->fileExtension, extension))  {
                break;
	    }
	    if (!strcmp(thisEntry->fileExtension, prefix))  {
		extension = prefix;
		break;
	    }
	}

	if (thisEntry == NULL) {
	    thisEntry = &defaultMapping;
	}

	targetObject = thisEntry->dataName;
	newAttributes = thisEntry->newAttributes;
	existingAttributes = thisEntry->existingAttributes;


/* Setup hardwired attributes in static storage.
 * Currently, we support the following hardwired attributes:
 *    "filename" => The full pathname of the file as given to this routine.
 *    "extension" => The file's extension, either of the form ".x"
 *                    (where x is a string) or "".
 *    "filesize" => An integer attribute giving the size (in bytes) of the file.
 *                  This is filled in below in the file part of this routine.
 */
        if (attributes != NULL) {
            strcpy(lastFilename, filename);
            /* Why can't the string library do the right thing? */
            strncpy(lastExtension, extension, sizeof(lastExtension));
            lastExtension[sizeof(lastExtension) - 1] = '\0';

            hardWiredAttributes[EXTATTRINDEX].key = "extension";
            hardWiredAttributes[EXTATTRINDEX].value.string = lastExtension;
	    hardWiredAttributes[EXTATTRINDEX].next = existingAttributes;

            hardWiredAttributes[NAMEATTRINDEX].key = "filename";
            hardWiredAttributes[NAMEATTRINDEX].value.string = lastFilename;
            hardWiredAttributes[NAMEATTRINDEX].next = &hardWiredAttributes[EXTATTRINDEX];

            if ((access(filename, W_OK) < 0) && (errno == EACCES || errno == EROFS)) {
                hardWiredAttributes[READONLYINDEX].key = "readonly";
                hardWiredAttributes[READONLYINDEX].value.integer = TRUE;
                hardWiredAttributes[READONLYINDEX]. next = &hardWiredAttributes[NAMEATTRINDEX];
                *attributes = &hardWiredAttributes[READONLYINDEX];
            }
            else
                *attributes = &hardWiredAttributes[NAMEATTRINDEX];
        }
    }

/* Now try and get any info we need out of the file. */
    if (file != NULL) {
        long origPos;
        int c;
        char *s = "\\begindata{",
             objectIDBuffer[20], *readID = objectIDBuffer; /* This size should hold a number alright. */
        struct stat statBuf;

        origPos = ftell(file);

        if ((attributes != NULL) && (fstat(fileno(file), &statBuf) >= 0)) { /* This may not work for pipes... */
            hardWiredAttributes[SIZEATTRINDEX].key = "filesize";
            hardWiredAttributes[SIZEATTRINDEX].value.integer = statBuf.st_size;
            hardWiredAttributes[SIZEATTRINDEX].next = *attributes;
            *attributes = &hardWiredAttributes[SIZEATTRINDEX];
	}

        while (getc(file) == *s && *++s != '\0')
            ;
        if (*s == '\0') {
            s = objectName;
            while ((c = getc(file)) != EOF && c != ',')
                *s++ = c;
            if (c == ',') {
                *s = '\0';
                while ((c = getc(file)) != EOF && c != '}' && (readID < (objectIDBuffer + (sizeof(objectIDBuffer) - 1))))
                    *readID++ = c;
                if (c == '}') {
                    if ((c = getc(file)) != '\n')
                        ungetc(c, file);
                    if (objectID != NULL) {
                        *readID = '\0';
                        *objectID = atoi(objectIDBuffer);
                    }

                    return objectName;
                }
            }
        }

        fseek(file, origPos > 0 ? origPos : 0, 0L);
    }

    hardWiredAttributes[EXTATTRINDEX].next = newAttributes;

    if (objectID != NULL)
        *objectID = 0;

    if (targetObject != NULL) {
        strcpy(objectName, targetObject);
        return objectName;
    }
    else
        return NULL;
}

void filetype__CanonicalizeFilename(classID, canonicalName, name, maxSize)
    struct classheader *classID;
    char *canonicalName, *name;
    int maxSize;
{

    char fullName[MAXPATHLEN];

    strncpy(canonicalName, path_UnfoldFileName(name, fullName, 0), maxSize);
}
