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


 

#define text_VERSION 1

enum textwritestyle {text_DefaultWrite, text_NoDataStream, text_DataStream};

class text: simpletext[smpltext] {
overrides:
    SetAttributes(struct attributes *attributes);
    Clear();
    GetModified() returns long;
    ReplaceCharacters(long pos, long len, char *repStr, long replen) returns boolean;
    AlwaysReplaceCharacters(long pos, long len, char *repStr, long replen);
    LengthChanged(long pos, long len);
    HandleKeyWord(long pos, char *keyword, FILE *file) returns long;
    HandleCloseBrace(long pos, FILE *file) returns long;
    Read(FILE *file, long id) returns long;
    Write(FILE *file, long writeID, int level) returns long;
    WriteOtherFormat(FILE *file, long writeid, int level, int usagetype, char *boundary) returns long;
    ReadSubString(long pos, FILE *file, boolean quoteCharacters) returns long;
    WriteSubString(long pos, long len, FILE *file, boolean quoteCharacters);
    ObservedChanged (struct observable *changed, long value);
    AlwaysCopyText(long pos,struct thisobject *srctext,long srcpos,long len);
    AlwaysDeleteCharacters(long position, long size);
    CheckHighBit() returns boolean;
    CopyTextExactly(long pos, struct thisobject *srctext, long srcpos, long len) returns boolean;
    AlwaysCopyTextExactly(long pos, struct thisobject *srctext, long srcpos, long len);
    
methods:
    InsertObject(long pos, char *name,char *viewname) returns struct viewref *;
    AddStyle(long pos, long len, struct style *style) returns struct environment *; 
    AddView(long pos, char *viewtype, struct dataobject *dataobject) returns struct environment *;
    AlwaysAddStyle(long pos, long len, struct style *style) returns struct environment *; 
    AlwaysAddView(long pos, char *viewtype, struct dataobject *dataobject) returns struct environment *;
    SetEnvironmentStyle(struct environment *envptr, struct style *styleptr);
    SetGlobalStyle(struct style *styleptr);
    GetGlobalStyle() returns struct style *;
    ReadTemplate(char *templateName, boolean inserttemplatetext) returns long;
    FindViewreference(long pos, long len) returns struct viewref *;
    AlwaysInsertFile(FILE * file,char *filename, long position) returns long;
    InsertFile(FILE * file,char *filename, long position) returns long;
    ClearCompletely();
    EnumerateEnvironments(long pos,long len,procedure callBack,long rock) returns struct environment *;
    SetBaseTemplateName(char *name);
    AlwaysWrapViewChar(long pos, char *view, struct dataobject *dobj) returns struct environment *;
    
macromethods:
    SetExportEnvironments(boolean val) (self->exportEnvs = val)
    GetExportEnvironments() (self->exportEnvs)
    GetStyleSheet() (self->styleSheet)
    SetWriteAsText(TorF)  ((self)->WriteAsText = TorF)
    GetWriteAsText()  ((self)->WriteAsText)
    SetCopyAsText(TorF)  ((self)->CopyAsText = TorF)
    GetCopyAsText()  ((self)->CopyAsText)
    GetWriteStyle() ((self)->writeStyle)
    SetWriteStyle(style) ((self)->writeStyle = style)
    GetBaseTemplateName() ((self)->templateName)

classprocedures:
    FinalizeObject(struct text *self);
    ApplyEnvironment(struct text_statevector *sv,struct style *defaultStyle, struct environment *env);
    InitializeObject(struct text *self) returns boolean;
    InitStateVector(struct text_statevector *sv);
    FinalizeStateVector(struct text_statevector *sv);
data:
    struct environment *rootEnvironment;
    struct stylesheet *styleSheet;
    char *templateName; /* Template to read if uninterpreted document. */
    struct viewref *currentViewreference;
    boolean exportEnvs;
    boolean executingGetModified;
    boolean WriteAsText,CopyAsText;
    enum textwritestyle writeStyle;
    struct dataobject **objs;
    long nobjs, objssize;
};
