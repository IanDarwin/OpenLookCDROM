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


 

struct bufferContents { /* May not have to be in this file. */
    struct view *bufferView; /* The actual view that is looking at the data object. */
    struct view *bufferInputFocus; /* The view that is taking input. */
    struct view *bufferApplicationView; /* The view the window is pointing at. (May not be neccesary). */
    boolean used; /* Probably should be boolean. */
};

/* Flag values for buffer_GetBufferOnFile function. */
#define buffer_MustExist 1 /* File must exist for operation to succeed. */
#define buffer_ForceNew 2 /* Make a new buffer even if one already exists. */
#define buffer_ReadOnly 4 /* Buffer will be readonly regardless of file status. */
#define buffer_RawMode 8 /* REad in in raw mode using a text object */

/* Flag values for buffer_WriteToFile function. */
#define buffer_ReliableWrite 1 /* Use vclose to ensure the file is written to server or an error code is returned. */
#define buffer_MakeBackup 2 /* Keep a backup file if the user preferences so indicates. */

class buffer: observable[observe] {
    overrides:
        ObservedChanged(struct observable *object, long value);
    methods:
        EnumerateViews(procedure mapFunction, long functionData) returns struct view *; /* ??? */
        SetData(struct dataobject *data);
        SetName(char *name);
        SetFilename(char *fileName);
        SetWriteVersion(long version);
        SetCkpVersion(long version);
        SetCkpClock(long clock);
        SetCkpLatency(long latency);
        SetScratch(boolean scratch);
        GetView(struct view **inputFocus, struct view **targetView, char *ViewName) returns struct view *;
        Visible() returns boolean;
        RemoveView(struct view *unusedView);
        ReadFile(char *filename) returns int;
        WriteToFile(char *filename, long flags) returns int;
        /* Disk file date may differ from GetTouchDate if file is modified externally. */
        GetFileDate() returns long;
	SetDefaultViewname(char *name);
	SetDestroyData(boolean destroy);
	SetReadOnly(boolean readOnly);
	SetLastTouchDate(long dataStamp);
	SetCheckpointFilename();
	SetIsModified(boolean value);
	SetIsRawFile(boolean value);
    macromethods:
        GetData() ((self)->bufferData)
        GetName() ((self)->bufferName)
        GetFilename() ((self)->filename)
        GetCkpFilename() ((self)->ckpFilename)
        GetWriteVersion() ((self)->writeVersion)
        GetCkpVersion() ((self)->ckpVersion)
        GetCkpClock() ((self)->ckpClock)
        GetCkpLatency() ((self)->ckpLatency)
        GetScratch() ((self)->scratch)
        GetMadeBackup() ((self)->madeBackup)
        SetMadeBackup(boolean value) ((self)->madeBackup = (value))
        /* Unavailable/unapplicable dates are returned as 0 */
        GetLastTouchDate() ((self)->lastTouchDate)
	GetDefaultViewname() ((self)->viewname)
        GetReadOnly() (self->readOnly)
	GetIsModified() (self->isModified)
        GetIsRawFile() (self->isRawFile)
    classprocedures:
        Enumerate(procedure mapFunction, long functionData) returns struct buffer *;
	Create(char *bufferName, char *fileName, char *objetName, struct dataobject *data) returns struct buffer *;
	GetGlobalBufferList() returns struct bufferlist *;

        InitializeClass() returns boolean;
	
/* Lookup functions */
        FindBufferByName(char *bufferName) returns struct buffer *;
        FindBufferByFile(char *filename) returns struct buffer *;
        FindBufferByData(struct dataobject *data) returns struct buffer *;

/* File functions. */
        GetBufferOnFile(char *filename, long flags) returns struct buffer *;
        GuessBufferName( char *filename, char *bufferName, int nameSize);
        GetUniqueBufferName(char *proposedBufferName, char *bufferName, int nameSize);
        FinalizeObject(struct buffer *self);
        InitializeObject(struct buffer *self) returns boolean;
        SetDefaultObject(char *objectname);

    data:
        struct dataobject *bufferData; /* The dataobject associated with this buffer. Can't be NULL.*/
        char *bufferName; /* This buffers name. Must be filled in. */
        char *filename; /* The filename this dataobject is saved in. May be NULL. */
        char *ckpFilename; /* Checkpoint file, filled in automatically. */
        struct bufferContents *viewList; /* Array of view triples viewing this buffer's dataobj. */
        int viewsAllocated, viewsUsed; /* viewsAllocated = number of slots in above table. viewsUsed = number of slots containing views. */
        long ckpVersion, writeVersion, ckpClock, ckpLatency; /* Dataobject version last checkpointed, last written. ckpClock is a sequence clock used for intelligent checkpointing. */
        long lastTouchDate; /* When was the file last read or written by the buffer */
        boolean destroyData; /* Are we supposed to destroy this when we are destroyed. */
        boolean scratch; /* Indicates that this buffer is temporary in nature. Namely, don't checkpoint it. */
        boolean readOnly; /* This is only a hint. If the dataobject ignores the read only attribute, this won't be true. */
	boolean madeBackup; /* This is used to tell if we have made a backup file yet this session. */
	boolean	isRawFile;  /* This is set to indicate that the file read in was read in in raw mode */
	boolean isModified;
	boolean askedAboutSymlink; /* TRUE iff the user wants to be asked about clobbering symlinks AND s/he's already been asked for this instance */
	boolean clobberSymlink;	/* TRUE means replace symlinks when writing */
        char *viewname;
};
