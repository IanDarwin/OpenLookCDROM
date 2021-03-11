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

#define cel_VERSION 1
#define cel_VISIBLE 0
#define cel_INVISIBLE 1
/* allowable args to cel_SetApplication */
#define cel_NORMAL 0
#define cel_APPLICATION 1
#define cel_NOTSET -1
#define cel_VALUE 10
#define cel_NeedsRepost 4242

#define cel_UNDEFINEDVALUE -47474747
class cel: dataobject[dataobj] {
overrides:
    ObservedChanged (struct observable *changed, long value);
    Get( struct atom * property, struct atom ** type, long * rock )
      returns short;
    Read (FILE *file, long id) returns long;
    Write (FILE *file, long writeid, int level) returns long;
    GetModified() returns long;
classprocedures:
    Create(char *viewtype, struct dataobject *dataobject) returns struct cel *;
    InitializeClass()returns boolean;
    InitializeObject() returns boolean;
    FinalizeObject(struct cel *self);
methods:
    SetChildObject(struct dataobject *newobject,char *viewName) returns boolean;
    SetObjectByName(char *dataname) returns boolean;
    GetObject() returns struct dataobject *;
    SetViewName(char *viewname,boolean UseDefault);
    SetApplication(int app);
    InsertObject (struct dataobject *newobject,char *dataname,char *viewname,int usedefaultview);
    SetRefName(char *refname) returns char *;
    UnsetRefName();
    SetVisibilityBit(mode);
    SetObject(struct dataobject *newobject) returns boolean;
    SetLinkName(char *linkname);
    SetObjectName(char *dataname) ;
    WriteLink (FILE *file, long writeid, int level) returns long;
    GetStringAtt(char *attname,char *buf,long buflen) returns char *;
    GetLongAtt(char *attname) returns long;
    SetLongAtt(char *attname,long val);
    SetStringAtt(char *attname,char *attval);
    ReadSup (FILE *file, long id) returns long;
    WriteSup (FILE *file, long writeid, int level) returns long;
    ReadFile(FILE *file) returns long;
    InitDefault() returns long;
macromethods:
    GetRefName() (self->refname)
    GetViewName() (self->viewType)
    GetObjectName() (self->dataType)
    GetRefAtom() (self->refatm)
    GetViewAtom() (self->viewatm)
    GetObjectAtom() (self->dataatm)
    GetLinkName() (self->linkname)
    GetLinkAtom() (self->linkatm)
    GetApplication() (self->application)
    GetScript() (self->script)
    SetVisible() cel_SetVisibilityBit(self,cel_VISIBLE)
    SetInvisible() cel_SetVisibilityBit(self,cel_INVISIBLE)
    Visible() (self->mode == cel_VISIBLE)
    SetNoSave(val) (self->NoSave = val)
    SetDefaultStream(S) (self->defaultStream = S)
    SetInitFile(S) (self->initfile = S)
    SetWriteChild(TF) (self->WriteChild = TF)
    GetDefaultStream() (self->defaultStream)
    GetInitFile() (self->initfile)
    GetWriteChild() (self->WriteChild )
    GetReadCount() (self->count)
    GetNextChain() ((self)->chain)
    GetArbiter() ((self)->ab)
    SetArbiter(AA) ((self)->ab = (AA))
data:
    struct atom *viewatm,*dataatm,*refatm,*linkatm;
    char *viewType, *dataType, *refname, *linkname;
    long viewID;
    struct dataobject *dataObject;
    long desw,desh;
    int application;
    boolean readfromfile,usedefaultview,NoSave;
    struct text *script;
    int mode;
    char *defaultStream,*initfile;
    long count;
    boolean WriteChild;
    struct cel *chain;
    struct arbiter *ab;
};
