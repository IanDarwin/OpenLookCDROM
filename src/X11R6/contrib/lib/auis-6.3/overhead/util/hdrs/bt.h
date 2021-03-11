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


 

/* ************************************************************ *\
	bt.h
	Header file declaring the client interface for a B-tree package.
\* ************************************************************ */

/* Almost all procedures return an error code.  bt_NoError is generally the all-okay return.*/
typedef int bt_ErrorCode;
#define bterr_NoError 0
#define bterr_OutOfMemory 1
#define bterr_NotABTree 2
#define bterr_NoFileNamesLeft 3
#define bterr_NoSuchMode 4
#define bterr_BTreeNotCurrVersion 5
#define bterr_BTreeDamaged 6
#define bterr_NotOpeningRoot 7
#define bterr_NoLockPermission 8
#define bterr_CursorTreeDamaged 9
#define bterr_IntermediateNameTooLong 10
#define bterr_NotAtKey 11
#define bterr_NoNextKey 12
#define bterr_EmptyTree 13
#define bterr_InternalInconsistency 14
#define bterr_OpenReadOnly 15
#define bterr_NoDuplicateInsert 16
#define bterr_ModifyKeyMissing 17
#define bterr_ValueTooLarge 18
#define bterr_IllegalKey 19
#define bterr_OldValueDifferent 20
#define bterr_UninitializedCursor 21
#define bterr_MAX 21
#define bterr_FileSystemErrorBegin 400
#define bterr_FileSystemErrorEnd 999
/* to be followed by others... */

/* bt_ErrorString gives static English text interpreting an error code value.
Declaration:
	extern char *bt_ErrorString(codevalue);
	bt_ErrorCode codevalue;
*/
extern char *bt_ErrorString();

struct BTree {int **Dummy;};		/* private structure */

/* Create a new b-tree.
Declaration:
	extern bt_ErrorCode bt_Create(path, MaxFileSize, LockStyle, NULL);
	char *path;		path to target file name for root file
	int MaxFileSize;		approximate maximum file size in bytes
	int LockStyle;		how to do locking on this structure--a btlock_foo type.
NULL terminates the argument list.
*/
#define btlock_UseNoLock		1
#define btlock_UseFileLock		2
#define btlock_UseTreeLock	3
extern bt_ErrorCode bt_Create();

/* Open an existing b-tree.
Declaration:
	extern bt_ErrorCode bt_Open(btptr, path, mode);
	struct BTree **btptr;	returns a pointer to malloc'ed storage in here
	char *path;		path to target file name
	char *mode;		"r" for reading, "w" for read and update (so far)
*/
extern bt_ErrorCode bt_Open();
/* Close a b-tree collection.
Declaration:
	extern bt_ErrorCode bt_Close(btptr);
	struct BTree *bt;		pointer to b-tree to close
*/
extern bt_ErrorCode bt_Close();

/* Get status of (the root file of) an open b-tree.  Returns as much information as will fit in longwords in the caller's area.  If the callee's BTHead structure is smaller than the caller's, the remaining words will be set to -1.
Declaration:
	extern bt_ErrorCode bt_GetFixedHead(bt, hdrloc, (int) sizeof(BTHead));
	struct BTree *bt;
	struct BTHead *hdrloc;
*/
struct BTHead {
	unsigned long bthVersion;
	unsigned long bthMaxFileSize;
	unsigned long bthDepth;	/* 0 => info is data; increases by 1 each step up the tree */
	unsigned long bthID2Top;	/* only significant in root--a counter */
	unsigned long bthFixedHeadSize;	/* how many longs in this FixedHead */
	unsigned long bthCTime1;	/* CTime is when the database was created */
	unsigned long bthCTime2;
	unsigned long bthMTime1;	/* MTime is when the database was last updated */
	unsigned long bthMTime2;
	unsigned long bthMWhere;	/* MWhere is the net address of the last updater */
	unsigned long bthMWho;	/* MWho is the Vice login ID of who last updated it */
	unsigned long bthLockStyle;	/* how this database should be locked */
};
extern bt_ErrorCode bt_GetFixedHead();

struct btCursor {int **Dummy;};	/* another private structure */
/* Create a cursor into an open b-tree.
Declaration:
	extern bt_ErrorCode bt_NewCursor(btp, cursptr);
	struct BTree *btp;
	struct btCursor **cursptr;
*/
extern bt_ErrorCode bt_NewCursor();
/* Remove a cursor into an open b-tree.
Declaration:
	extern bt_ErrorCode bt_FreeCursor(curs);
	struct btCursor *curs;
*/
extern bt_ErrorCode bt_FreeCursor();

/* Give the current state of the cursor (from J. N. Gray, Notes for a Data Base Operating System).
Declaration:
	extern enum bt_CursorState bt_GetCursorState(curs);
	struct btCursor *curs;
The cursor-state Error will be returned if the argument doesn't seem to be a btCursor.
*/
enum bt_CursorState {Error, Null, BeforeFirst, AtKey, BetweenKeys, AfterLast, UnInitialized};
extern enum bt_CursorState bt_GetCursorState();

/* Make a new cursor that's a copy of the old one, pointing to the same place in the same file.
Declaration:
	extern bt_ErrorCode bt_CopyCursor(cursptr, curs);
	struct btCursor **cursptr, *curs;
*/
extern bt_ErrorCode bt_CopyCursor();

/* Move an existing cursor to before the start or past the end of the database.
Declarations:
	extern bt_ErrorCode bt_CursorToStart(curs);
	struct btCursor *curs;
	extern bt_ErrorCode bt_CursorToEnd(curs);
	struct btCursor *curs;
*/
extern bt_ErrorCode bt_CursorToStart(), bt_CursorToEnd();

/* Search for the entry matching the given key.
If there's a match, leave the cursor pointing to the key; if none, leave the cursor in one of the other states.
Declaration:
	extern bt_ErrorCode bt_Search(curs, key);
	struct btCursor *curs;
	char *key;
*/
extern bt_ErrorCode bt_Search();

/* If the cursor is in AtKey state, return a pointer to freshly-allocated storage that holds a copy of the key to which the cursor points.
Declaration:
	extern bt_ErrorCode bt_GetCursorKey(curs, keyLoc);
	struct btCursor *curs;
	char **keyLoc;
*/
extern bt_ErrorCode bt_GetCursorKey();

/* If the cursor is in AtKey state, return the number of bytes in the value of the key-value pair being pointed to.
Declaration:
	extern bt_ErrorCode bt_GetCursorValueLen(curs, valueLen);
	struct btCursor *curs;
	unsigned int *valueLen;
*/
extern bt_ErrorCode bt_GetCursorValueLen();

/* Similarly, if the cursor is in AtKey state, return the first valueLocSize bytes in the value of the key-value pair being pointed to.
Declaration:
	extern bt_ErrorCode bt_GetCursorValueData(curs, valueLoc, valueLocSize, returnedLen);
	struct btCursor *curs;
	char *valueLoc;
	unsigned int valueLocSize, *returnedLen;
*/
extern bt_ErrorCode bt_GetCursorValueData();

/* Similarly, if the cursor is in AtKey state, allocate memory to contain the entire Value of the key-value pair being pointed to, read the Value into that memory, store the pointer to the Value in valueLoc, and its length into returnedLen.
Declaration:
	extern bt_ErrorCode bt_GetCursorValue(curs, valueLoc, returnedLen);
	struct btCursor *curs;
	char **valueLoc;
	unsigned int *returnedLen;
*/
extern bt_ErrorCode bt_GetCursorValue();

/* Advance the cursor to the next key-value pair.
Declaration:
	extern bt_ErrorCode bt_NextCursor(curs);
	struct btCursor *curs;
*/
extern bt_ErrorCode bt_NextCursor();


/* Get attributes of the current cursor location--the ID1, ID2, and filename of the current file, a pointer to a stat(2) buffer of that file, and the offset within that file.  Pointers returned are pointers to areas owned by the b-tree package: do not change them, and copy the contents if you want to save them.
Declaration:
	extern bt_ErrorCode bt_GetCursorAttributes(curs, ID1, ID2, StatPtr,
						Filename, Offset);
	struct btCursor *curs;
	unsigned long *ID1, *ID2, *Offset;
	struct stat **StatPtr;
	char **Filename;
*/
extern bt_ErrorCode bt_GetCursorAttributes();


/* Modifying the data structure.  These functions do not use cursors, because they must determine for  themselves (within a lock) how to modify the database. */

/* Insert the given key/value pair into the database.  Returns an error if there is already an entry with that key.
Declaration:
	extern bt_ErrorCode bt_Insert(bt, key, valueLoc, valueLen);
	struct BTree *bt;
	char *key, *valueLoc;
	unsigned int valueLen;
*/
extern bt_ErrorCode bt_Insert();

/* Similarly to bt_Insert, put the key/value pair into the database, overwriting the value currently associated with the given key.  If there is no existing entry with that key, it returns an error.
Declaration:
	extern bt_ErrorCode bt_Replace(bt, key, valueLoc, valueLen);
	struct BTree *bt;
	char *key, *valueLoc;
	unsigned int valueLen;
*/
extern bt_ErrorCode bt_Replace();

/* Delete the record associated with the given key.  Returns an error if there is no such record.
Declaration:
	extern bt_ErrorCode bt_Delete(bt, key);
	struct BTree *bt;
	char *key;
*/
extern bt_ErrorCode bt_Delete();

/* Do a conditional replacement in the B-tree.  This operation will do an indivisible test-and-set on the database: if the value associated with the given key is oldValueLoc/oldValueLen, it will be replaced with newValueLoc/newValueLen.  If the existing value is not oldValueLoc/oldValueLen, the code bterr_OldValueDifferent will be returned and no replacement will be done.  As with bt_Replace, if there is no existing entry with this key, it returns an error.
Declaration:
	extern bt_ErrorCode bt_CondReplace(bt, key, oldValueLoc, oldValueLen, newValueLoc, newValueLen);
	struct BTree *bt;
	char *key, *oldValueLoc, *newValueLoc;
	unsigned int oldValueLen, newValueLen;
*/
extern bt_ErrorCode bt_CondReplace();

/* Do a conditional deletion in the B-tree.  This operation will do an indivisible test-and-set on the database: if the value associated with the given key is oldValueLoc/oldValueLen, it will be deleted.  If the existing value is not oldValueLoc/oldValueLen, the code bterr_OldValueDifferent will be returned and no deletion will be done.  As with bt_Delete, if there is no existing entry with this key, it returns an error.
Declaration:
	extern bt_ErrorCode bt_CondDelete(bt, key, oldValueLoc, oldValueLen);
	struct BTree *bt;
	char *key, *oldValueLoc;
	unsigned int oldValueLen;
*/
extern bt_ErrorCode bt_CondDelete();

