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

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Bush Data-object

MODULE	bush.ch

VERSION	0.0

AUTHOR	TC Peters & GW Keim
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Bush Data-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  08/21/85	Created (TCP)
  01/15/89	Convert to ATK (GW Keim)

END-SPECIFICATION  ************************************************************/


#include <tree.ih>

struct entry_mode {
    unsigned int do_rescan:1;
    unsigned int destroyed:1;
    unsigned int selected:1;
    unsigned int detailed:1;
    unsigned int stat_failed:1;
    unsigned int scan_failed:1;
};

struct entry_kind {
    unsigned int dir:1;
    unsigned int soft_link:1;
    unsigned int file:1;
};

struct Dir_Entry {
    char *name;
    char *link_name;
    struct entry_kind type;
    struct entry_mode mode;
    size_t size;
    time_t time_stamp;
    char *owner;
    int nlinks;
    unsigned permissions;
    int position;
    tree_type_node parent, instance;
};

struct Dir_Entries {
    int count;
    struct Dir_Entry **Entry;
};

struct Dir_ {
    time_t time_stamp;
    char *path, *name;
    struct entry_mode mode;
    tree_type_node tn;
    struct Dir_Entries *Dir_Entries;
};

class bush : apt {
    classprocedures:

	Create(char *init_dir) returns struct bush *;

    methods:

	InitTree(char *root_path) returns void;
	ScanDir(tree_type_node tn) returns int;
	BuildSubDirs(tree_type_node tn)	returns void;
	DestroySubDirs(tree_type_node tn) returns void;
	DestroyDirEntries(tree_type_node tn) returns void;
	DestroyDirEntry(tree_type_node tn) returns void;
	FreeSubDirs(tree_type_node tn) returns void;
	DestroyEntry(tree_type_node tn,struct Dir_Entry *Entry) returns int; 
	ScanRequired(tree_type_node tn) returns boolean;
	RenameDir(tree_type_node tn,char *newPath, char *newName) returns int;
	MoveEntry(tree_type_node tn,struct Dir_Entry *Entry,char *newName) returns int;
	PerformSystemAction( char* name, char **argv ) returns int;

    overrides:

  	Read(FILE *file,long id) returns long;
  	Write(FILE *file,long id,long level) returns long;
	ViewName() returns char *;

    macromethods:

	Tree() (self->tree)
	TreeRoot() (tree_RootNode(bush_Tree(self)))
	RootPathName() (self->root_pathname)
	GivenRootPathName() (self->given_dir_name)
	Dir(tn) ((struct Dir_ *)tree_NodeDatum(bush_Tree(self),tn))
	DirMode(tn) (bush_Dir(self,tn)->mode)
	DirPath(tn) (bush_Dir(self,tn)->path)
	DirName(tn) (bush_Dir(self,tn)->name)
	DirTimeStamp(tn) (bush_Dir(self,tn)->time_stamp)
	RootDirPath() (bush_DirPath(self,bush_TreeRoot(self)))
	DirEntries(tn) (bush_Dir(self,tn)->Dir_Entries)
	DirEntriesCount(tn) (bush_DirEntries(self,tn)->count)
	DirEntryPtr(tn) (bush_DirEntries(self,tn)->Entry)
	DirEntry(tn,i) (bush_DirEntries(self,tn)->Entry[i])
	DirEntryMode(tn,i) (bush_DirEntry(self,tn,i)->mode)
	DirEntryPos(tn,i) (bush_DirEntry(self,tn,i)->position)
	DirEntryName(tn,i) (bush_DirEntry(self,tn,i)->name)
        DirEntryLinkName(tn,i) (bush_DirEntry(self,tn,i)->link_name)
	DirEntryType(tn,i) (bush_DirEntry(self,tn,i)->type)
	DirEntryOwner(tn,i) (bush_DirEntry(self,tn,i)->owner)
	DirEntryNLinks(tn,i) (bush_DirEntry(self,tn,i)->nlinks)
	DirEntryTimeStamp(tn,i) (bush_DirEntry(self,tn,i)->time_stamp)
	DirEntrySize(tn,i) (bush_DirEntry(self,tn,i)->size)
	DirEntryPerms(tn,i) (bush_DirEntry(self,tn,i)->permissions)
	DirEntryParent(tn,i) (bush_DirEntry(self,tn,i)->parent)
	DirEntryInstance(tn,i) (bush_DirEntry(self,tn,i)->instance)
	DirEntryParentDir(tn,i) (bush_Dir(self,bush_DirEntryParent(self,tn,i)))
	RootDir() (bush_Dir(self,bush_TreeRoot(self)))
	Parent(tn) (tree_ParentNode(bush_Tree(self),tn))
	Child(tn) (tree_ChildNode(bush_Tree(self),tn))
	Left(tn) (tree_LeftNode(bush_Tree(self),tn))
	Right(tn) (tree_RightNode(bush_Tree(self),tn))
	ParentDir(tn) (bush_Dir(self,bush_Parent(self,tn)))
	ChildDir(tn) (bush_Dir(self,bush_Child(self,tn)))
	LeftDir(tn) (bush_Dir(self,bush_Left(self,tn)))
	RightDir(tn) (bush_Dir(self,bush_Right(self,tn)))

    data:

	char		 given_dir_name[1025];
	struct tree	*tree;
	char		*root_pathname;
	struct vector	*uid_uname_map;
	char		 mycellname[1000];
	int		 debug;
};
