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





#include <class.h>



/*
 * some constants, etc.
 */

#define INDEXLOCKFILE "index.lck"
#define INDEXFILE "index"
#define INDEXNEWFILE "index.new"




struct IndexEntry {
    char * Name;
    char * Key;
    unsigned long Version;
    char * Data;
};






extern struct IndexEntry *
ReadEntry(/* fd */);

extern boolean
WriteEntry(/* fd, entry */);


extern struct IndexEntry *
NewEntry(/* */);

extern struct IndexEntry *
CreateEntry(/* name, key, version, data */);

extern void
DestroyEntry(/* entry */);


extern char * 
GetEntryName(/* entry */);

extern char * 
GetEntryKey(/* entry */);

extern unsigned long
GetEntryVersion(/* entry */);

extern char * 
GetEntryData(/* entry */);


extern boolean
SetEntryName(/* entry, name */);

extern boolean
SetEntryKey(/* entry, key */);

extern boolean
SetEntryVersion(/* entry, version */);

extern boolean
SetEntryData(/* entry, data */);







