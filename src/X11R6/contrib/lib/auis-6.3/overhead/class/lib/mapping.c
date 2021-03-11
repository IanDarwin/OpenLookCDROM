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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/class/lib/RCS/mapping.c,v 1.8 1993/08/25 20:34:23 susan Exp $";
#endif

/*
 * Package of routines to manipulate the mapping between
 * class names and class name keys.
 */

/*
 * Hints, etc.
 * 
 ****
 *
 * Please note that the number of HASH root entries must
 * be a power of 2.  The code will work correctly if 
 * only HASHENTRIESLOG2 is changed.
 *
 ****
 *
 * This code may not work correctly on machines that do not
 * use 2's complement representation of integers.
 *
 ****
 *
 *
 *
 */

#include <stdio.h>
#include <andrewos.h>
#include <mapping.h>


/* 
 * various constants and other twiddle-able
 * values.
 */
#define	HASHENTRIESLOG2	(6)
#define	HASHENTRIES	(1 << HASHENTRIESLOG2)



/*
 * local data structures.
 */

struct PathEntryStruct {
    char * Name;
    struct PathEntryStruct * Link;
};




/* 
 * data maintained by routines
 */
static int MappingIsInitialized = 0;
static struct PathEntryStruct * PathEntries;
static struct MapEntryStruct * NameEntryRoots[HASHENTRIES];
static struct MapEntryStruct * KeyEntryRoots[HASHENTRIES];


/*
 *
 * local routines.
 *
 */


/** 
 ** Generate a hash value for name.  This may not be the 
 ** greatest algorithm for the hash but it can be changed
 ** if it doesn't spread things out well enough.  It should
 ** keep names that start the same way from hashing to
 ** same value.
 **/
static int HashName(name)
char * name;

{
char * p;
unsigned int result;

    result = 0;
    p = name;
    while (*p != NULL) {
	result = (result < 1) ^ *p;
	p++;
    }
    return (result & (HASHENTRIES - 1));
}


/**
 ** Create a new mapping entry and initialize the data.
 ** If the entry can not be created return NULL.
 **/
static struct MapEntryStruct *
CreateMapEntry(name, key, version, pathindex, data)
char * name;
char * key;
class_VersionNumberType version;
int pathindex;
char * data;

{
struct MapEntryStruct * NewEntry;


    NewEntry = (struct MapEntryStruct *) malloc(sizeof(struct MapEntryStruct));
    if (NewEntry == NULL) {
	return NULL;
    }
    NewEntry->Name = (char *) malloc(strlen(name) + 1);
    if (NewEntry->Name == NULL) {
	free(NewEntry);
	return NULL;
    }
    NewEntry->Key = (char *) malloc(strlen(key) + 1);
    if (NewEntry->Key == NULL) {
	free(NewEntry->Name);
	free(NewEntry);
	return NULL;
    }
    NewEntry->Data = (char *) malloc(strlen(data) + 1);
    if (NewEntry->Data == NULL) {
	free(NewEntry->Name);
	free(NewEntry->Key);
	free(NewEntry);
	return NULL;
    }


    /* move in the data */

    (void) strcpy(NewEntry->Name, name);
    (void) strcpy(NewEntry->Key, key);
    NewEntry->Version = version;
    NewEntry->PathIndex = pathindex;
    (void) strcpy(NewEntry->Data, data);

    NewEntry->NameLink = NULL;  /* always add to the end of the lists */
    NewEntry->KeyLink = NULL;


    return NewEntry;
}


/** 
 ** Construct a string that describes the place to find a dynamically
 ** loaded object.  If the string cannot be constructed NULL is returned.
 ** 
 ** Please note the entries with "/" as the first character of the data
 ** field are a special case.  These entries will not have the path and "/"
 ** prepended to the load string.
 **/
static char *
ConstructLoadString(entry)
struct MapEntryStruct * entry;

{
char * ThisPath;
char * ThisString;
int ThisStringLength;

    if (*(entry->Data) == '/') {
	ThisString = (char *) malloc(strlen(entry->Data) + 1);
	strcpy(ThisString, entry->Data);
    } else {
	ThisPath = GetPathEntry(entry->PathIndex);
	if (ThisPath == NULL) {
	    return	NULL;	    /* on error return NULL */
	}
	ThisStringLength = strlen(entry->Data) + strlen(ThisPath) + 2; /* space for "/" and "\000" */
	ThisString = (char *) malloc(ThisStringLength);
	if (ThisString == NULL) {
	    return	NULL;	    /* on error return NULL */
	}
	(void) strcpy(ThisString, ThisPath);
	(void) strcat(ThisString, "/");	    /* insert a "/" between path and name */
	(void) strcat(ThisString, entry->Data);
    }

    return ThisString;
}



/* 
 * 
 * These routines handle the initialization and other
 * control of this package of routines.
 * 
 */


/** 
 ** Initialize this package.  All data is free'd,
 ** data structures are initialized, etc.  0 return
 ** indicates success, -1 indicates failure.
 **/
int
InitializeMappingPackage()

{
int i;

    /* if it has been initialize free all data */
    if (MappingIsInitialized != 0) {
	return -1;
    } else {
	/* initialize the data structures form scratch */
	MappingIsInitialized = 1;
	PathEntries = NULL;
	for (i = 0; i < HASHENTRIES; i++) {
	    NameEntryRoots[i] = NULL;
	    KeyEntryRoots[i] = NULL;
	}
	return 0;
    }
}


/**
 ** Clear all the entries in the mapping table.  All the pointers are left
 ** pointing to NULL.
 ** 
 ** Note that each map entry is free'd by traversing the name table.
 ** The key table is just set to NULLs.
 **/
int
ClearAllMappingEntries()

{
struct PathEntryStruct * ThisPath;
struct MapEntryStruct * ThisMap;
int i;

    if (MappingIsInitialized != 0) {
	while (PathEntries != NULL) {
	    ThisPath = PathEntries;
	    PathEntries = PathEntries->Link;
	    free(ThisPath);
	}
	for (i = 0; i < HASHENTRIES; i++) {
	    while (NameEntryRoots[i] != NULL) {
		ThisMap = NameEntryRoots[i];
		NameEntryRoots[i] = NameEntryRoots[i]->NameLink;
		free(ThisMap->Name);
		free(ThisMap->Key);
		free(ThisMap->Data);
		free(ThisMap);
	    }
	    KeyEntryRoots[i] = NULL;
	}
    } else {
	return -1;  /* can't clear mapping table if not initialized */
    }
    return 0;
}


/* 
 * 
 * These routines manipulate path entries.
 * 
 */


/**
 ** Enter the string path to the list of paths and return
 ** the index assigned to this path.  -1 is returned if 
 ** the path cannot be registered.
 **/
int
EnterPathEntry(path)
char * path;

{
struct PathEntryStruct * ThisEntry;
int i;

    if (MappingIsInitialized == 0) {
	return -1;
    }

    /* the first entry is a special case */
    if (PathEntries == NULL) {
	PathEntries = (struct PathEntryStruct *) malloc(sizeof(struct PathEntryStruct));	
	if (PathEntries == NULL) {
	     return -1;
	}
	PathEntries->Name = (char *) malloc(strlen(path) + 1);
	if (PathEntries->Name == NULL) {
	    free(PathEntries);
	    return -1;
	}
	PathEntries->Link = NULL;
	strcpy(PathEntries->Name, path);
	return 0;
    } else {

	/* general case */
	ThisEntry = PathEntries;
	i = 1;
	while (ThisEntry->Link != NULL)	{   /* end of the list? */
	    ThisEntry = ThisEntry->Link;
	    i = i+1;
	}
	ThisEntry->Link = (struct PathEntryStruct *) malloc(sizeof(struct PathEntryStruct));	
	if (PathEntries->Link == NULL) {
	    return -1;
	}
	ThisEntry = ThisEntry->Link;	/* for convenience */
	ThisEntry->Name = (char *) malloc(strlen(path) + 1);
	if (ThisEntry->Name == NULL) {
	    free(ThisEntry);
	    return -1;
	}
	ThisEntry->Link = NULL;
	strcpy(ThisEntry->Name, path);
	return i;

    }
}


/**
 ** Retrieve the a pointer to the path 
 ** with registered as index.  NULL is returned if
 ** something is wrong.
 **/
char *
GetPathEntry(index)
int index; 

{
struct PathEntryStruct * ThisEntry;
int i;

    if (MappingIsInitialized == 0) {
	return NULL;
    }

    ThisEntry = PathEntries;
    for (i = 0; i < index; i++) {
	if (ThisEntry == NULL) {
	    return NULL;
	}
	ThisEntry = ThisEntry->Link;
    }
    if (ThisEntry == NULL) {
	return NULL;
    } else {
	return ThisEntry->Name;
    }
}


/* 
 * 
 * These routines insert and retrieve map
 * entries in various ways.  Please note there
 * is no way to delete an entry.
 * 
 */


/**
 ** Enter an entry into the mapping database.  0 return
 ** indicates success, -1 indicates failure.  If an entry for 
 ** this name-key pair already exists, the new entry will
 ** not be added.  This is not considered to be a failure and
 ** 0 is returned.  If there is a match for only the name or 
 ** only the key, the entry will be added to the mapping
 ** information and the new part of the entry that matches an
 ** old entry will be inserted AFTER the existing entry so the
 ** existing entry will be used.
 **/
int
EnterMapEntry(name, key, version, pathindex, data)
char * name;
char * key;
class_VersionNumberType version;
int pathindex;
char * data;

{
struct MapEntryStruct * ThisEntry;
struct MapEntryStruct * NewEntry;

int NameHash;
int KeyHash;


    if (MappingIsInitialized == 0) {
	return -1;
    }

    /* if both of these indexing values are already entered,  don't bother */
    if ((RetrieveByName(name, version) != NULL) && (RetrieveByKey(key, version) != NULL)) {
	return 0;
    }


    /* make a new entry */

    NewEntry = CreateMapEntry(name, key, version, pathindex, data);


    /* get the hash values */

    NameHash = HashName(name);
    KeyHash = HashName(key);


    /* find out where to insert the new entry */


    /* Insert the entry in the lists and */

    if (NameEntryRoots[NameHash] == NULL) {
	NameEntryRoots[NameHash] = NewEntry;
    } else {
	ThisEntry = NameEntryRoots[NameHash];
	while (ThisEntry->NameLink != NULL) {
	    ThisEntry = ThisEntry->NameLink;
	}
	ThisEntry->NameLink = NewEntry;	    /* add new entry to end of list */
    }

    if (KeyEntryRoots[KeyHash] == NULL) {
	KeyEntryRoots[KeyHash] = NewEntry;
    } else {
	ThisEntry = KeyEntryRoots[KeyHash];
	while (ThisEntry->KeyLink != NULL) {
	    ThisEntry = ThisEntry->KeyLink;
	}
	ThisEntry->KeyLink = NewEntry;	    /* add new entry to end of list */
    }


    return 0;
}


/**
 ** Use the class name to get string that is 
 ** used to load the object.  NULL is returned
 ** if something goes wrong.  The string
 ** pointed to by the returned value must be 
 ** free'd by the caller.
 **/
char *
MapByName(name, version)
char * name;
class_VersionNumberType version;

{
struct MapEntryStruct * ThisEntry;

    if (MappingIsInitialized == 0) {
	return NULL;
    }

    ThisEntry = RetrieveByName(name, version);
    if (ThisEntry == NULL) {
	return NULL;
    }

    return ConstructLoadString(ThisEntry);
}


/**
 ** Use the key to get string that is 
 ** used to load the object.  NULL is returned
 ** if something goes wrong.  The string
 ** pointed to by the returned value must be 
 ** free'd by the caller.
 **/
char *
MapByKey(key, version)
char * key;
class_VersionNumberType version;

{
struct MapEntryStruct * ThisEntry;

    if (MappingIsInitialized == 0) {
	return NULL;
    }

    ThisEntry = RetrieveByKey(key, version);
    if (ThisEntry == NULL) {
	return NULL;
    }

    return ConstructLoadString(ThisEntry);
}


/**
 ** Use the class name to retrieve the mapping entry.
 ** NULL is returned if something goes wrong.  The 
 ** returned value points to a string that must not
 ** be modified or free'd by the caller.
 **/
struct MapEntryStruct *
RetrieveByName(name, version)
char * name;
class_VersionNumberType version;

{
struct MapEntryStruct * ThisEntry;

    if (MappingIsInitialized == 0) {
	return NULL;
    }

    ThisEntry = NameEntryRoots[HashName(name)];

    while (ThisEntry != NULL) {
	if (strcmp(name, ThisEntry->Name) == 0) {
	    if ((version == class_VERSIONNOTKNOWN) || (version == ThisEntry->Version) || (ThisEntry->Version == class_VERSIONNOTKNOWN)) {
		return ThisEntry;
	    }
	}
	ThisEntry = ThisEntry->NameLink;
    }

    return NULL;
}


/**
 ** Use the key to retrieve the mapping entry.
 ** NULL is returned if something goes wrong.  The 
 ** returned value points to a string that must not
 ** be modified or free'd by the caller.
 **/
struct MapEntryStruct *
RetrieveByKey(key, version)
char * key;
class_VersionNumberType version;

{
struct MapEntryStruct * ThisEntry;

    if (MappingIsInitialized == 0) {
	return NULL;
    }

    ThisEntry = KeyEntryRoots[HashName(key)];

    while (ThisEntry != NULL) {
	if (strcmp(key, ThisEntry->Key) == 0) {
	    if ((version == class_VERSIONNOTKNOWN) || (version == ThisEntry->Version) || (ThisEntry->Version == class_VERSIONNOTKNOWN)) {
		return ThisEntry;
	    }
	}
	ThisEntry = ThisEntry->KeyLink;
    }

    return NULL;
}


/* 
 * 
 * These routines help with debugging by
 * dumping the contents of the package.
 * 
 */


/** 
 ** Dump a formatted list of the mapping entries
 ** to file.
 **/
void
DumpMappingInfo(file)
FILE *file;

{
struct PathEntryStruct * ThisPath;
struct MapEntryStruct * ThisMap;
int i, j;

    if (MappingIsInitialized == 0) {
	return;
    }

    fprintf(file, "\n");
    fprintf(file, "Dumping search path entries:\n");
    fprintf(file, "\n");

    ThisPath = PathEntries;
    i = 0;
    while (ThisPath != NULL) {
	fprintf(file, "    [%02d] \"%s\"\n", i, ThisPath->Name);
	i++;
	ThisPath = ThisPath->Link;
    }

    fprintf(file, "\n");
    fprintf(file, "Dumping mapping entries by name:\n");
    fprintf(file, "\n");

    for (i = 0; i < HASHENTRIES; i++) {
	ThisMap = NameEntryRoots[i];
	j = 0;
	while (ThisMap != NULL) {
	    fprintf(file, "    entry [%02d][%02d] \"%s\" [%02d] \"%s\" 0x%08x\n", i, j, ThisMap->Name, ThisMap->PathIndex, ThisMap->Data, ThisMap->Version);
	    j++;
	    ThisMap = ThisMap->NameLink;
	}
    }

    fprintf(file, "\n");
    fprintf(file, "Dumping mapping entries by key:\n");
    fprintf(file, "\n");

    for (i = 0; i < HASHENTRIES; i++) {
	ThisMap = KeyEntryRoots[i];
	j = 0;
	while (ThisMap != NULL) {
	    fprintf(file, "    entry [%02d][%02d] \"%s\" [%02d] \"%s\" 0x%08x\n", i, j, ThisMap->Key, ThisMap->PathIndex, ThisMap->Data, ThisMap->Version);
	    j++;
	    ThisMap = ThisMap->KeyLink;
	}
    }
}


/** 
 ** Dump a formatted list of the mapping statistics
 ** to file.
 **/
void
DumpMappingStats(file)
FILE *file;

{
struct PathEntryStruct * ThisPath;
struct MapEntryStruct * ThisMap;
int i, j;

    if (MappingIsInitialized == 0) {
	return;
    }

    fprintf(file, "\n");
    fprintf(file, "Dumping search path statistics:\n");
    fprintf(file, "\n");

    ThisPath = PathEntries;
    i = 0;
    while (ThisPath != NULL) {
	i++;
	ThisPath = ThisPath->Link;
    }
    fprintf(file, "    %d search paths are registered\n", i);
    fprintf(file, "\n");

    fprintf(file, "Name hash table entries:\n");
    for (i = 0; i < HASHENTRIES; i++) {
	ThisMap = NameEntryRoots[i];
	j = 0;
	while (ThisMap != NULL) {
	    j++;
	    ThisMap = ThisMap->NameLink;
	}
	fprintf(file, "        [%02d] contains %d entries.\n", i, j);
    }

    fprintf(file, "\n");
    fprintf(file, "Key hash table entries:\n");
    for (i = 0; i < HASHENTRIES; i++) {
	ThisMap = KeyEntryRoots[i];
	j = 0;
	while (ThisMap != NULL) {
	    j++;
	    ThisMap = ThisMap->KeyLink;
	}
	fprintf(file, "        [%02d] contains %d entries.\n", i, j);
    }

    fprintf(file, "\n");
}



