/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 


#include <atom.ih>
#include <namespc.ih>

struct property {
    struct atom * type;
    long data;
};

/* "Other Format" codes -- so far only mail */
#define dataobject_OTHERFORMAT_MAIL 1

/* Encoding codes for 7 bit transport */
#define dataobject_ENCODING_BASE64 1
#define dataobject_ENCODING_QUOTEDPRINTABLE 2

class dataobject[dataobj]: observable[observe] {
methods:
    Read (FILE *file, long id) returns long;
    Write (FILE *file, long writeid, int level) returns long;
    ReadOtherFormat(FILE *file, char *fmt, char *encoding, char *description) returns boolean;
    WriteOtherFormat(FILE *file, long writeid, int level, int usagetype, char *boundary) returns long;
    GetModified() returns long;
    SetModified();
    ViewName() returns char *;
    SetAttributes(struct attributes *attributes);
    Put( struct atom * property, struct atom * type, long rock );
    Get( struct atom * property, struct atom ** type, long * rock )
      returns short;
    ListCurrentViews(struct view **array,int size) returns int;
    RestoreModified(long oldmodified);
macromethods:
    UniqueID() ((long)(self))
    GetWriteID() ( (self)->writeID )
    SetWriteID(long newValue) ( ((self)->writeID) = (newValue) )
    GetID()      ( (self)->id )
    SetID(long newValue)      ( ((self)->id) = (newValue) )
classprocedures:
    InitializeObject(struct dataobject *self) returns boolean;
    FinalizeObject(struct dataobject *self);
data:
    long id;
    long writeID;
    long modified;
    struct namespace * properties;
};

#define dataobject_UNDEFINEDID -1

/* return values from Read */
#define	dataobject_NOREADERROR	0
#define	dataobject_PREMATUREEOF	1
#define	dataobject_NOTBE2DATASTREAM 2 /* backward compatibility */
#define	dataobject_NOTATKDATASTREAM 2 /* preferred version */
#define dataobject_MISSINGENDDATAMARKER 3
#define	dataobject_OBJECTCREATIONFAILED	4
#define dataobject_BADFORMAT 5
