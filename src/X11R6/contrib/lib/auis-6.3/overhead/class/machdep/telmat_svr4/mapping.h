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
 *  $Disclaimer: This software is part of version 5.2.0 of the 
 * Andrew User Interface System and is the 
 * property of IBM, Carnegie Mellon University, 
 * and the other copyright holders.  The source 
 * code of this version is for the sole use of 
 * members of the Andrew Consortium with 
 * memberships extending into calendar year 
 * 1993.  This source code is not to be distributed 
 * to non-members of the consortium nor beyond 
 * a fifty-mile radius from the membership address.  
 * Binary object code compiled or derived from 
 * these sources is not to be distributed to non-
 * members.  Members may have additional 
 * distribution rights granted by prior written 
 * permission of Carnegie Mellon University.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, 
 * AND THE OTHER COPYRIGHT HOLDERS
 *  DISCLAIM ALL WARRANTIES WITH 
 * REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANT-
 * ABILITY AND FITNESS. IN 
 * NO EVENT SHALL  IBM, CARNEGIE 
 * MELLON UNIVERSITY, OR ANY OTHER 
 * COPYRIGHT HOLDER BE LIABLE FOR 
 * ANY SPECIAL, INDIRECT OR CONSE-
 * QUENTIAL DAMAGES OR ANY DAMAGES 
 * WHATSOEVER RESULTING FROM LOSS OF
 * USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR
 * OTHER TORTIOUS ACTION, ARISING OUT 
 * OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *  $
 *  $
*/




#ifndef mapping_DEFINED
#define mapping_DEFINED 1

/*
 * Package of routines to manipulate the mapping between
 * class names and class name keys.
 */

#include <class.h>	/* get data structures for classheader data */




struct MapEntryStruct {
    char * Name;
    char * Key;
    class_VersionNumberType Version;
    char * Data;
    int	PathIndex;
    struct MapEntryStruct * NameLink;
    struct MapEntryStruct * KeyLink;
};




/* 
 * Initialize this package.  All data is free'd,
 * data structures are initialized, etc.  0 return
 * indicates success, -1 indicates failure.
 */
extern int
InitializeMappingPackage(/* */);


/*
 * Clear all the entries in the mapping table.  
 * All the pointers are left pointing to NULL.
 * 0 is returned for success, -1 for failure.
 */
extern int
ClearAllMappingEntries(/* */);


/*
 * Enter the string path to the list of paths and return
 * the index assigned to this path.  -1 is returned if 
 * the path cannot be registered.
 */
extern int
EnterPathEntry(/* path */);


/*
 * Retrieve the a pointer to the path 
 * with registered as index.  NULL is returned if
 * something is wrong.
 */
extern char *
GetPathEntry(/* index */);


/*
 * Enter an entry into the mapping database.  0 return
 * indicates success, -1 indicates failure.
 */
extern int
EnterMapEntry(/* name, key, version, pathindex, data */);


/*
 * Use the class name to get string that is 
 * used to load the object.  NULL is returned
 * if something goes wrong.  The string
 * pointed to by the returned value must be 
 * free'd by the caller.
 */
extern char *
MapByName(/* name, version */);


/*
 * Use the key to get string that is 
 * used to load the object.  NULL is returned
 * if something goes wrong.  The string
 * pointed to by the returned value must be 
 * free'd by the caller.
 */
extern char *
MapByKey(/* key, version */);


/*
 * Use the class name to retrieve the mapping entry.
 * NULL is returned if something goes wrong.  The 
 * returned value points to a string that must not
 * be modified or free'd by the caller.
 */
extern struct MapEntryStruct *
RetrieveByName(/* name, version */);


/*
 * Use the key to retrieve the mapping entry.
 * NULL is returned if something goes wrong.  The 
 * returned value points to a string that must not
 * be modified or free'd by the caller.
 */
extern struct MapEntryStruct *
RetrieveByKey(/* key, version */);


/* 
 * Dump a formatted list of the mapping entries
 * to file.
 */
extern void
DumpMappingInfo(/* file */);


/* 
 * Dump a formatted list of the mapping statistics
 * to file.
 */
extern void
DumpMappingStats(/* file */);



#endif /* #ifndef mapping_DEFINED */

