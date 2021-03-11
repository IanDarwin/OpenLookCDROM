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
	btint.h
	Header file declaring the internal interface for a B-tree package.
\* ************************************************************ */

#include "bt.h"

#define BTSignatureValue ((unsigned long) 0x7ff19606)	/* B-tree files start with this longword. */
#define BTSeparatorByte   ((unsigned char) 0x82)		/* precedes all key-value pairs */

/*
All B-tree files have identical format.  They contain 16-bit and 32-bit integers (in network byte order, not necessarily aligned on four-byte boundaries) and sequences of characters and other bytes.

All files are in three gross sections.  The primary content of the file is the sequence of key-value pairs.  Each pair is represented in the file by the sequence:
	one separator byte (fixed value)
	one byte of flag bits
	the key: a null-terminated sequence of characters
	a two-byte count of value bytes
	the value: that many subsequent bytes.

The sequence of these pairs forms the third, and generally largest, section of the file.  The first section is a fixed sequence of 32-bit integers describing the rest of the file.  The second section is a table of 32-bit unsigned integers giving the byte offsets in the file for the start of each key-value sequence; these numbers assume that the first byte of the file is numbered 0.  The offsets give the address of the separator byte--two bytes before the first byte of the key, if all is well.

The last key-value pair is the pointer to the right-brother file in all files except the root of the tree.  This key-value pair will have the flag bit BTIsBrotherLink set in the flag bit byte; all other pairs will have this bit field clear.

The files in a B-tree have some position-dependent information.  The leaf files of the B-tree have zero in their BTDepth fields.  The root file of the B-tree will have no right-brother node.

The file path given to the bt_Open routine is the name of the root file.  The names of all other files in the tree are found by appending suffixes to the root file's name.  The value of the non-leaf key-value pairs is the text string (minus the leading period) that should be appended to the root file's name to get the name of the file one level down the tree.
*/
#define BTIsLeftmostKey	0004
#define BTIsLeafPair	0010
#define BTIsBrotherLink	0020


/* Structure of the fixed header--a sequence of unsigned longs.  We call it an array here so that we can read and write it as a sequence of words in the file, but we use it like a structure with separate fields.
*/
#define BTSignature	BTarr[0]	/* must contain BTSignatureValue */
#define BTVersion		BTarr[1]
#define BTMaxFileSize	BTarr[2]	/* Maximum file size given at tree-creation time */
#define BTDepth		BTarr[3]	/* 0 => info is data; increases by 1 each step up the tree */
#define BTID1		BTarr[4]	/* two identifiers of file in tree */
#define BTID2		BTarr[5]
#define BTIndexStart	BTarr[6]	/* zero-orig starting byte for first index in index section */
#define BTIndexCount	BTarr[7]	/* count of valid entries in index */
#define BTIndexSize	BTarr[8]	/* size of allocated space in index */
#define BTKVStart		BTarr[9]	/* zero-orig starting byte for key-value pair data */
#define BTKVFF		BTarr[10]	/* zero-orig first free byte past key-value pairs */
#define BTFixedHeadSize	BTarr[11]	/* how many longs in this FixedHead */
#define BTFixedHeadSizeOffset	11	/* for reading the thing */
#define BTCTime1		BTarr[12]	/* CTime is when the database file was created */
#define BTCTime2		BTarr[13]
#define BTMTime1	BTarr[14]    /* MTime is when the database file was last updated */
#define BTMTime2	BTarr[15]
#define BTMWhere	BTarr[16]	/* MWhere is the net address of the last updater */
#define BTMWho		BTarr[17]    /* MWho is the Vice login ID of who last updated it */
#define BTMWhoE	BTarr[18]	/* MWhoE is effective login ID of last updater */
#define BTLockStyle	BTarr[19]	/* how this database should be locked */
#define BTExpectedSize1	BTarr[20]	/* two cells to track the size of the index */
#define BTExpectedSize2	BTarr[21]
#define BTarrSIZE		22

struct BTFixedHead {unsigned long BTarr[BTarrSIZE];};


struct btFile {	/* Refers to one of the files making up the B-tree */
	int		Tag;
	struct BTFixedHead Head;		/* in host (not net) format */
	char		*FileName;
	FILE		*File;		/* stdio FILE structure */
	unsigned long	*Index;		/* pointer to copy of index from file */
	long		FileOrigin;	/* ftell result from file at origin */
	int		IndexAlloc;	/* size of malloc'd Index structure */
	int		IndexNum;	/* number of valid elements in Index */
	int		RefCount;	/* number of BTr's and btC's pointing here */
	struct stat	FileStat;		/* fstat() information */
};

struct BTr {	/* The private representation of the public BTree structure */
	int		Tag;
	struct btFile	*Root;
	struct btC	*Cursors;
	int		WriteEnabled;
};

struct btC {	/* The private representation of the public btCursor structure */
	int		Tag;
	struct BTr	*Tree;		/* points to parent BTr structure */
	struct btC	*Next;		/* next in list of cursors linked from that struct */
	struct btFile	*FP;		/* the file to which cursor points */
	int		IndexPos;	/* index into FP's index giving position pointed to */
	enum bt_CursorState State;		/* at key, between keys, at beginning or end, ... */
};

#define	BTrTag	83
#define	btCTag	85
#define	btFileTag	91

/* The procedures exported by the library generally begin with the ``bt_'' prefix.  (The exceptions to this rule are the btr_SetDebugging and btw_SetDebugging procedures.)  Procedures used for inter-module but intra-library communication (i.e., the procedures that one library module exports solely for use by other library modules) begin with the ``b_'' prefix. */

#define ThisBTVersion	1
extern unsigned short b_ReadNetShort();
extern unsigned long b_ReadNetLong();
extern struct btFile *b_NewbtFileStr();
extern bt_ErrorCode b_InitbtFileStr();
extern bt_ErrorCode b_AddIndex();
extern bt_ErrorCode b_WriteHeadIndex();
extern bt_ErrorCode b_GetFlags();
extern bt_ErrorCode b_GetValueLength();
extern bt_ErrorCode b_ReadbtFile();
extern bt_ErrorCode b_ScanNode();
extern bt_ErrorCode b_DecrRefCount();
extern bt_ErrorCode b_StoreFilePtr();
extern int b_FileIsRoot();
