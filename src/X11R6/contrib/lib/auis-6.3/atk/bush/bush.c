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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/bush/RCS/bush.c,v 1.39 1993/09/22 19:20:13 gk5g Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Bush Data-object

MODULE	bush.c

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

struct map_item { 
  int	 uid;
  char	*uname;
  char	*ucell;
};

#include <bush.h>
#include <apts.ih>
#include <dataobj.ih>
#include <filetype.ih>
#include <im.ih>
#include <vector.ih>
#include <environ.ih>
#include <bush.eh>  /* includes tree.ih */

#define	GivenDirName		    (self->given_dir_name)
#define	RootPath		    (self->root_pathname)
#define	UidUnameMap		    (self->uid_uname_map)
#define	MyCellName		    (self->mycellname)
#define	Debug			    (self->debug)
#define	Tree			    ((self)->tree)
#define	Root			    (tree_RootNode(Tree))
#define	RootPathName		    ((self)->root_pathname)
#define	Dir(tn)			    ((struct Dir_*)tree_NodeDatum(Tree,tn))
#define	DirMode(tn)		    (Dir(tn)->mode)
#define	DirPath(tn)		    (Dir(tn)->path)
#define	DirName(tn)		    (Dir(tn)->name)
#define	DirTimeStamp(tn)	    (Dir(tn)->time_stamp)
#define	RootDirPath		    (DirPath(Root))
#define	DirEntries(tn)		    (Dir(tn)->Dir_Entries)
#define	DirEntriesCount(tn)	    (DirEntries(tn)->count)
#define	DirEntryPtr(tn)		    (DirEntries(tn)->Entry)    
#define	DirEntry(tn,i)		    (DirEntries(tn)->Entry[i])
#define	DirEntryMode(tn,i)	    (DirEntry(tn,i)->mode)
#define	DirEntryPos(tn,i)	    (DirEntry(tn,i)->position)
#define	DirEntryName(tn,i)	    (DirEntry(tn,i)->name)
#define	DirEntryLinkName(tn,i)	    (DirEntry(tn,i)->link_name)
#define	DirEntryType(tn,i)	    (DirEntry(tn,i)->type)
#define	DirEntryOwner(tn,i)	    (DirEntry(tn,i)->owner)
#define	DirEntryNLinks(tn,i)	    (DirEntry(tn,i)->nlinks)
#define	DirEntryTimeStamp(tn,i)	    (DirEntry(tn,i)->time_stamp)
#define	DirEntrySize(tn,i)	    (DirEntry(tn,i)->size)
#define	DirEntryPerms(tn,i)	    (DirEntry(tn,i)->permissions)
#define	DirEntryParent(tn,i)	    (DirEntry(tn,i)->parent)
#define	DirEntryInstance(tn,i)	    (DirEntry(tn,i)->instance)
#define	DirEntryParentDir(tn,i)	    (Dir(DirEntryParent(tn,i)))    
#define	RootDir			    (Dir(Root))
#define	Parent(tn)		    (tree_ParentNode(Tree,tn))
#define	Child(tn)		    (tree_ChildNode(Tree,tn))
#define	Left(tn)		    (tree_LeftNode(Tree,tn))
#define	Right(tn)		    (tree_RightNode(Tree,tn))
#define	ParentDir(tn)		    (Dir(Parent(tn)))
#define	ChildDir(tn)		    (Dir(Child(tn)))
#define	LeftDir(tn)		    (Dir(Left(tn)))
#define	RightDir(tn)		    (Dir(Right(tn)))

#define	no_space		    2
#define	no_superior		    3
#define	no_inferior		    4
#define	scan_failure		    5
#define	read_failure		    6

#define	AllocNameSpace(s,t)	    apts_CaptureString(s,t)

tree_Specification DirTree[] = {tree_Order(tree_PreOrder),NULL};

extern int			    errno, sys_nerr;
extern char			   *sys_errlist[];

char				baseName[] = "/afs"; /*Pathname to give to pioctl()*/
#define	MAX_PIOCTL_BUFF_SIZE	1000
static char			*gethomecell(), *getcell();

static int
NodeFilter( dir )
  register DIRENT_TYPE    *dir;
{
  return(!(*dir->d_name == '.' && 
	  (*(dir->d_name+1) == '.' || 
	 *(dir->d_name+1) == '\0')));
}

struct bush *
bush__Create( ClassID, init_dir )
  struct classheader	*ClassID;
  char			*init_dir;
{
  register struct bush  *self = NULL;

  if(self = bush_New()) {
    if(init_dir && (init_dir[0] != '\0'))
      strcpy(GivenDirName,init_dir);
    else im_GetDirectory(GivenDirName);
    bush_InitTree(self,GivenDirName);
    bush_BuildSubDirs(self,bush_TreeRoot(self));
  }
  OUT(bush_Create);
  return(self);
}

boolean
bush__InitializeObject( ClassID, self )
  register struct classheader	*ClassID;
  register struct bush		*self;
{
  Tree = tree_Create(DirTree,self);
  RootPath = NULL;
  *GivenDirName = 0;
  Debug = 0;
  UidUnameMap = vector_Create(30,3);
  MyCellName[0] = '\0';
  return(TRUE);
}

static int
ExtractNodePath( self, source, path )
  register struct bush    *self;
  register char		  *source, **path;
{
  register long     status = 0, i = 0, len;
  char		    full_path[MAXPATHLEN + 1], 
		    workingPathName[MAXPATHLEN + 1];
  register char	   *ptr;

  IN(ExtractNodePath);
  getwd(workingPathName);
  strcpy(full_path, workingPathName);
  switch(*source) {
    case '.':
      while(*(source + i) == '.' && *(source + i + 1) == '.' &&
  	     (*(source + i + 2) == '\0' || *(source + i + 2) == '/')) {
	if(ptr = (char *)rindex(full_path, '/')) 
	  *ptr = '\0';
	if(*(source + i + 2) == '\0') {
	  i += 2;
	  break;
	}
	else i += 3;
      }
      if(i && *(source + i) != '\0') {
	strcat(full_path, "/");
	strcat(full_path, source + i);
      }
      source = full_path;
      break;
    default:
      if(*source != '/') {
	strcat(full_path,"/");
	strcat(full_path, GivenDirName);
	source = full_path;
      }
  }
  len = strlen(source);
  if ((len > 1) && source[len - 1] == '/')
      source[len - 1] = (char) 0;
  if(!status && (*path = (char *)malloc(len + 1)))
    strcpy(*path, source);
  else status = -1;
  OUT(ExtractNodePath);
  return(status);
}

static int
ExtractNodeName( source, name )
  register char *source;
  register char **name;
{
  register long status = 0, len;
  register char *ptr = NULL;

  if(source && (len = strlen(source)) > 0) {
      if((len > 1) && source[len-1] == '/')
	  source[len-1] = (char) 0;
      if((ptr = (char *)rindex(source, '/')) && *(source + 1) != '\0')
	  source = ++ptr;
      if(*name = (char *)malloc(len + 1))
	  strcpy(*name, source);
      else
	  status = -1;
  }
  else status = -1;
  return(status);
}

void
bush__InitTree( self, root_path )
  register struct bush	*self;
  register char		*root_path;
{
  tree_type_node	 root = NULL;
  struct Dir_		*rootDir = (struct Dir_ *)calloc(1,sizeof(struct Dir_));
  char			*nodeName = NULL, tmp[MAXPATHLEN];
  struct stat		 stats;

  IN(bush_InitTree);
  if(RootPath) {
    free(RootPath);
    RootPath = NULL;
  }
  ExtractNodePath(self,root_path,&RootPath);
  if(access(RootPath,F_OK) < 0) {
      printf("bush: directory '%s' does not exist or cannot be searched.\n",
	       RootPath);
      free(RootPath);
      im_GetDirectory(GivenDirName);
      AllocNameSpace(GivenDirName,&RootPath);
  }
  else strcpy(GivenDirName,root_path);
  strcpy(tmp,RootPath);
  ExtractNodeName(tmp,&nodeName);
  AllocNameSpace(nodeName,&rootDir->name);
  if(stat(RootPath,&stats) < 0) {
      printf("bush: error '%s' encountered while scanning '%s'.\n", sys_errlist[errno]);
      return;
  }
  else {
    gethomecell(self,baseName);
    root = tree_CreateRootNode(Tree,rootDir->name,(char*)rootDir);
    rootDir->tn = root;
    AllocNameSpace(RootPath,&rootDir->path);
    bush_ScanDir(self,root);
    DirMode(root).selected = TRUE;
  }
  OUT(bush_InitTree);
}

void
bush__DestroySubDirs( self, tn )
  register struct bush	    *self;
  register tree_type_node    tn;
{
  IN(bush_DestroySubDirs);
  bush_FreeSubDirs(self,tn);
  tree_DestroyNodeChildren(Tree,tn);
  IN(bush_DestroySubDirs);
}

static char *
gethomecell( self, filename )
  register struct bush  *self;
  register char		*filename;
{
#ifdef AFS_ENV
  struct ViceIoctl	 blob;
  int			 outcome;

  blob.in_size  = sizeof(baseName);
  blob.in       = baseName;
  blob.out_size = MAX_PIOCTL_BUFF_SIZE;
  blob.out      = MyCellName;
  
  outcome = pioctl(baseName,VIOC_GET_PRIMARY_CELL,&blob,1);
  if(outcome) {
    blob.in_size  = sizeof(baseName);
    blob.in       = baseName;
    blob.out_size = MAX_PIOCTL_BUFF_SIZE;
    blob.out      = MyCellName;

    outcome = pioctl(baseName,VIOC_GET_WS_CELL,&blob,1);
    if(outcome) 
      sprintf(MyCellName,"%s","andrew.cmu.edu");
    return(MyCellName);
  }
#else
  return("");
#endif /* AFS_ENV */
}

static char *
getcell( self, filename )
  register struct bush  *self;
  register char		*filename;
{
#ifdef AFS_ENV
  struct ViceIoctl	 blob;
  static char		 residence[MAX_PIOCTL_BUFF_SIZE];

  blob.in_size  = sizeof(filename);
  blob.in       = filename;
  blob.out_size = MAX_PIOCTL_BUFF_SIZE;
  blob.out      = residence;

  if(pioctl(filename,VIOC_FILE_CELL_NAME,&blob,1))
    return(MyCellName);
  return(residence);
#else
  return("");
#endif /* AFS_ENV */
}

static char *
getname( self, uid, cell )
  register struct bush  *self;
  register int   	 uid;
  register char		*cell;
{
  register int		     i = 0;
  register struct map_item  *item = NULL;
  char			    *uname = NULL;
  register struct passwd    *pw = NULL;
#ifdef AFS_ENV
  for( i = 0 ; i < vector_Count(UidUnameMap) ; i++ ) {
    item = (struct map_item*)vector_Item(UidUnameMap,i);
    if((uid == item->uid) && cell && item->ucell && !strcmp(cell,item->ucell)) {
      uname = item->uname;
      break;
    }
  }
  if(!uname) {
    if(pw = (struct passwd *) getcpwuid(uid,cell)) {
      item = (struct map_item*)calloc(1,sizeof(struct map_item));
      item->uid = uid;
      AllocNameSpace(pw->pw_name,&item->uname);
      AllocNameSpace(cell,&item->ucell);
      vector_AddItem(UidUnameMap,(long)item);
      uname = item->uname;
    }
    else {
      char	    uid_str[200];

      item = (struct map_item*)calloc(1,sizeof(struct map_item));
      item->uid = uid;
      sprintf(uid_str,"%u@%s",uid,cell);
      AllocNameSpace(uid_str,&item->uname);
      AllocNameSpace(cell,&item->ucell);
      vector_AddItem(UidUnameMap,(long)item);
      uname = item->uname;
    }
  }
#else /* AFS_ENV */
  for( i = 0 ; i < vector_Count(UidUnameMap) ; i++ ) {
    item = (struct map_item*)vector_Item(UidUnameMap,i);
    if(uid == item->uid) {
      uname = item->uname;
      break;
    }
  }
  if(!uname) {
    if(pw = getpwuid(uid)) {
      item = (struct map_item*)calloc(1,sizeof(struct map_item));
      item->uid = uid;
      AllocNameSpace(pw->pw_name,&item->uname);
      AllocNameSpace("",&item->ucell);
      vector_AddItem(UidUnameMap,(long)item);
      uname = item->uname;
    }
    else {
      char	    uid_str[200];

      item = (struct map_item*)calloc(1,sizeof(struct map_item));
      item->uid = uid;
      sprintf(uid_str,"%u",uid);
      AllocNameSpace(uid_str,&item->uname);
      AllocNameSpace("",&item->ucell);
      vector_AddItem(UidUnameMap,(long)item);
      uname = item->uname;
    }
  }
#endif /* AFS_ENV */
  return(uname);
}

int
bush__ScanDir( self, tn )
  register struct bush	    *self;
  register tree_type_node    tn;
{
  register long		     i = 0, status = ok, count = 0;
  register char		    *ptr = NULL;
  DIRENT_TYPE		   **anchor = NULL;
  struct stat		     stats, lstats;
  int			     alphasort(), cc = 0;
  char			     fullEntryName[MAXPATHLEN+25], buf[MAXPATHLEN];
  char			     workingDir[MAXPATHLEN];

  IN(bush_ScanDir);
  if(!tn) return(scan_failure);
  getwd(workingDir);
  chdir(DirPath(tn));
  if(stat(DirPath(tn),&stats)) {
    status = scan_failure;
    DirMode(tn).stat_failed = TRUE;
  }
  else DirTimeStamp(tn) = stats.st_mtime;
  if(status == ok  && 
     (count = scandir(DirPath(tn), &anchor, NodeFilter, alphasort)) < 0) {
    status = read_failure;
    DirMode(tn).scan_failed = TRUE;
  }
  else if(status == ok) {
    if(Child(tn))
	bush_DestroySubDirs(self, tn);
    else bush_DestroyDirEntries(self, tn);
    if(count)
      if(!(DirEntries(tn) = (struct Dir_Entries*) 
	calloc(1, sizeof(struct Dir_Entries))))
	  status = no_space;
      else {
        DirEntryPtr(tn) = 
	    (struct Dir_Entry **) calloc(count,sizeof(struct Dir_Entry*));
	DirEntriesCount(tn) = count;
      }
    for( i = 0; i < count && status == ok; i++ ) {
      DirEntry(tn,i) = (struct Dir_Entry*)calloc(1,sizeof(struct Dir_Entry));
      DirEntryPos(tn,i) = i;
      AllocNameSpace(anchor[i]->d_name, &DirEntryName(tn,i));
      ptr = DirEntryName(tn,i);
      while(*ptr != '\0') {
	if(!(isascii(*ptr) && isprint(*ptr))) *ptr = '.';
	ptr++;
      }
      sprintf(fullEntryName,"%s/%s",DirPath(tn),anchor[i]->d_name);
      if(lstat(fullEntryName,&stats) < 0) 
	DirEntryMode(tn,i).stat_failed = TRUE;
      else {
	if((stats.st_mode & S_IFMT) == S_IFDIR)	
	  DirEntryType(tn,i).dir = TRUE;
#ifdef S_IFLNK
	else if((stats.st_mode & S_IFMT) == S_IFLNK) {
	  DirEntryType(tn,i).soft_link = TRUE;
	  if(cc = readlink(fullEntryName,buf,MAXPATHLEN)) {
	    buf[cc] = '\0';
	    AllocNameSpace(buf,&DirEntryLinkName(tn,i));
	  }
	  if(lstat(buf,&lstats) >= 0) {
	    stats = lstats;
	    if((stats.st_mode & S_IFMT) == S_IFDIR)
	      DirEntryType(tn,i).dir = TRUE;
	    else DirEntryType(tn,i).file = TRUE;
	  }
	}
#endif /* S_IFLNK */
	else DirEntryType(tn,i).file = TRUE;
	AllocNameSpace(getname(self,stats.st_uid,getcell(self,fullEntryName)),
		       &DirEntryOwner(tn,i));
	DirEntryTimeStamp(tn,i) = stats.st_mtime;
	DirEntrySize(tn,i) = stats.st_size;
	DirEntryPerms(tn,i) = stats.st_mode;
	DirEntryNLinks(tn,i) = stats.st_nlink;
      }
      DirEntryParent(tn,i) = tn;
      if(anchor[i]) free(anchor[i]);
    }
  }
  if(anchor) free(anchor);
  DirMode(tn).do_rescan = FALSE;
  chdir(workingDir);
  OUT(bush_ScanDir);
  return(status);
}

void
bush__BuildSubDirs( self, tn )
  register struct bush	    *self;
  register tree_type_node    tn;
{
  register long		     i = 0, count = 0;
  tree_type_node	     newTreeNode = NULL;
  struct Dir_		    *newDir = NULL;
  char			     newDirPath[MAXPATHLEN];

  IN(bush_BuildSubDirs);
  if(tn && DirEntries(tn)) {
    count = DirEntriesCount(tn);
    while(i < count) {
      if(DirEntry(tn,i) && !(DirEntryMode(tn,i).destroyed) && 
	  DirEntryType(tn,i).dir) {
        newDir = (struct Dir_ *) calloc(1,sizeof(struct Dir_));
	sprintf(newDirPath,"%s%s%s",DirPath(tn),
		!strcmp(DirPath(tn),"/") ? "": "/",DirEntryName(tn,i));
	AllocNameSpace(newDirPath,&newDir->path);
	AllocNameSpace(DirEntryName(tn,i),&newDir->name);
	newTreeNode = tree_CreateChildNode(Tree, DirEntryName(tn, i), (char *) newDir, tn);
	DirMode(newTreeNode).do_rescan = TRUE;
	DirEntryInstance(tn, i) = newTreeNode;
	newDir->tn = newTreeNode;
      }
      i++;
    }
  }
  OUT(bush_BuildSubDirs);
}

void
bush__DestroyDirEntries( self, tn )
  register struct bush    *self;
  register tree_type_node  tn;
{
  register long		   i = 0, count = 0;

  IN(bush_DestroyDirEntries);
  if(tn && DirEntries(tn)) {
    count = DirEntriesCount(tn);
    while(i < count) {
      if(DirEntry(tn,i)) {
	if(DirEntryLinkName(tn,i)) {
	  free(DirEntryLinkName(tn,i));
	  DirEntryLinkName(tn,i) = NULL;
	}
	if(DirEntryName(tn,i)) {
	  free(DirEntryName(tn,i));
	  DirEntryName(tn,i) = NULL;
	}
	if(DirEntry(tn,i)) {
	  free(DirEntry(tn,i));
	  DirEntry(tn,i) = NULL;
	}
      }
      i++;
    }
    if(DirEntryPtr(tn)) {
      free(DirEntryPtr(tn));
      DirEntryPtr(tn) = NULL;
    }
    if(DirEntries(tn)) {
      free(DirEntries(tn));
      DirEntries(tn) = NULL;
    }
  }
  OUT(bush_DestroyDirEntries);
}

void
bush__DestroyDirEntry( self, tn )
  register struct bush    *self;
  register tree_type_node  tn;
{
  IN(bush_DestroyDirEntry);
  if(tn && bush_Dir(self,tn) && DirEntries(tn)) {
    bush_DestroyDirEntries(self,tn);
    if(DirPath(tn)) {
      free(DirPath(tn));
      DirPath(tn) = NULL;
    }
    if(DirName(tn)) {
      free(DirName(tn));
      DirName(tn) = NULL;
    }
    free(bush_Dir(self,tn));
    tree_SetNodeDatum(Tree,tn,NULL);
  }
  OUT(bush_DestroyDirEntry);
}

void
bush__FreeSubDirs( self, tn )
  register struct bush    *self;
  register tree_type_node  tn;
{
  register tree_type_node  tmp = NULL;
  register int		   level = 0;

  IN(bush_FreeSubDirs);
  if((tmp = tn) && ((level = tree_NodeLevel(Tree,tmp)) > 0))
    while((tmp = tree_NextNode(Tree,tmp)) && (tree_NodeLevel(Tree,tmp) > level))
      bush_DestroyDirEntry(self,tmp);
}

boolean
bush__ScanRequired( self, tn )
  register struct bush    *self;
  register tree_type_node  tn;
{
  boolean	     status = FALSE;
  struct stat	     stats;

  IN(bush_ScanRequired);
  if(!DirMode(tn).destroyed &&
     (stat(DirPath(tn),&stats) ||
      DirTimeStamp(tn) != stats.st_mtime ||
      DirMode(tn).do_rescan))
	status = TRUE;
  OUT(bush_ScanRequired);
  return(status);
}

int
bush__DestroyEntry( self, tn, Entry )
  register struct bush	    *self;
  register tree_type_node    tn;
  register struct Dir_Entry *Entry;
{
  char			     item[MAXPATHLEN*2];
  register long		     status = 0;

  sprintf(item,"%s/%s",DirPath(tn),Entry->name);
  if(Entry->type.dir) {
    static char	*argv[4] = {"rm","-rf",NULL,NULL};
    argv[2] = item;
    status = bush_PerformSystemAction(self,"/bin/rm",argv);
  }
  else status = unlink(item);
  if(!status) {
    Entry->mode.destroyed = TRUE;
    DirMode(tn).do_rescan = TRUE;
  }
  return(status);
}

int
bush__MoveEntry( self, tn, Entry, newName )
  register struct bush	    *self;
  register tree_type_node    tn;
  register struct Dir_Entry *Entry;
  register char		    *newName;
{
  char			     oldPath[MAXPATHLEN*2], newPath[MAXPATHLEN];
  register long		     status;

  sprintf(oldPath,"%s/%s",DirPath(tn),Entry->name);
  sprintf(newPath,"%s/%s",DirPath(tn),newName );
  if((status = rename(oldPath,newPath)) != -1 )
    AllocNameSpace(newName,&Entry->name);
  return(status);
}

int
bush__RenameDir( self, tn, newPath, newName )
  register struct bush	    *self;
  register tree_type_node    tn;
  register char		    *newPath, *newName;
{
  register long		     status = ok, i = 0;
  register char		    *newFullName = NULL;

  IN(bush_RenameDir);
  newFullName = (char*)malloc(strlen(newPath)+strlen(newName)+2);
  sprintf(newFullName,"%s/%s",newPath,newName);
  if(status = rename(DirPath(tn),newFullName)) return(status);
  else {
    AllocNameSpace(newFullName,&DirPath(tn));
    AllocNameSpace(newName,&DirName(tn));
    for(i=0;i<DirEntriesCount(Parent(tn));i++)
      if(DirEntryInstance(Parent(tn),i)==tn) {
	AllocNameSpace(DirName(tn),&DirEntryName(Parent(tn),i));
	break;
      }
  }	
  OUT(bush_RenameDir);
  return(status);
}

long
bush__Read( self, file, id )
  register struct bush	    *self;
  register FILE		    *file;
  register long		     id;
{
  char			     RootPathIfInset[MAXPATHLEN];
  long			     status = dataobject_NOREADERROR;

  IN(bush_Read);
  self->header.dataobject.id = dataobject_UniqueID(self);
  fscanf(file,"%s",GivenDirName);
  im_GetDirectory(RootPathIfInset);
  bush_InitTree(self,RootPathIfInset);
  bush_BuildSubDirs(self,bush_TreeRoot(self));
  chdir(bush_DirPath(self,bush_TreeRoot(self)));
  fscanf(file,"%s",RootPathIfInset); /* to get past enddata token */
  OUT(bush_Read);
  return(status);
}

long
bush__Write( self, file, id, level )
  register struct bush  *self;
  register FILE		*file;
  register long		 id;
  register long		 level;
{
  IN(bush_Write);
  if(self->header.dataobject.writeID != id) {
    self->header.dataobject.writeID = id;
    if(level) {
      fprintf(file,"\\begindata{%s,%d}\n",
	       class_GetTypeName(self),
	       dataobject_UniqueID(&self->header.dataobject));
      fprintf(file,"%s",DirPath(bush_TreeRoot(self)));
      fprintf(file,"\n\\enddata{%s,%d}\n",
	       class_GetTypeName(self),
	       dataobject_UniqueID(&self->header.dataobject));
    }
    else {
      fprintf(file,"\\begindata{%s,%d}\n",
	       class_GetTypeName(self),
	       dataobject_UniqueID(&self->header.dataobject));
      fprintf(file,"\n%s\n",DirPath(bush_TreeRoot(self)));
      fprintf(file,"\n\\enddata{%s,%d}\n",
	       class_GetTypeName(self),
	       dataobject_UniqueID(&self->header.dataobject));
    }
  }
  OUT(bush_Write);
  return((long)self);
}

char *
bush__ViewName( self )
  register struct bush    *self;
{
  return("bushv");
}

int
bush__PerformSystemAction( self, name, argv )
  register struct bush	    *self;
  register char		    *name;
  register char		    *argv[];
{
  int			     pid = 0, status = 0;

  if(environ_GetProfileSwitch("SecurityConscious", FALSE)) {
      fprintf(stderr, "SecurityConsciousness does not allow bush to run programs.\n");
      return(-1);
  }
  if((pid = osi_vfork()) == 0) {
      register int	fd;

      pid = getpid();
      NEWPGRP();
      fd = open("/dev/null",O_WRONLY,0644);
      if(fd >= 0) dup2(fd,1);
      close(fd);
      execvp(name,argv);
    /* flow should never reach here, but just in case.... */
      return(-1);
  }	
  while(pid != wait(&status));
  return(status);
}
