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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/menulist.c,v 2.16 1994/05/13 17:20:06 rr2b Exp $";
#endif

/* menulist.c
 * Provides an abstraction for cooperative menu usage among views.
 */

#include <andrewos.h>
#include <class.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <proctbl.ih>
#include <environ.ih>
#define class_StaticEntriesOnly
#include <view.ih>
#undef class_StaticEntriesOnly
#include <messitem.ih>
#include <menulist.eh>

#define MLITEM_UNDEF   0
#define MLITEM_DELETE  1
#define MLITEM_REPLACE 2

#define MPITEM_UNDEF   0
#define MPITEM_NOLOAD  1
#define MPITEM_LOAD    2

struct mlitem {
  char *oldname, *newname;
  short state;
  struct mlitem *next;
};

struct mpitem {
  char *name;
  short load;
  struct mlitem *ml;
  struct mpitem *next;
};

static void 
ExplodeMenuString(str, paneStr, paneStrLen, panePriority, 
		  selectionStr, selectionStrLen, selectionPriority)
char *str;
char *paneStr;
long paneStrLen;
long *panePriority;
char *selectionStr;
long selectionStrLen;
long *selectionPriority;
{
    char *p;
    char *pLimit;
    boolean inSelection = FALSE;
    boolean inPriority = FALSE;
    long priority = 0;

    p = paneStr;
    pLimit = paneStr + paneStrLen;
    for ( ; ; str++) switch (*str)  {
	case '\0':
	case ',':
	    *p = '\0';
	    if (inSelection)  {
		if (selectionPriority != NULL)
		    *selectionPriority = priority;
		return;
	    }
	    else if (*str == '\0')  {
		strncpy(selectionStr, paneStr, selectionStrLen);
		selectionStr[selectionStrLen - 1] = '\0';
		paneStr[0] = '\0';
		if (selectionPriority != NULL)
		    *selectionPriority = priority;

	/* Pane priority of -1 is special to cmenu_AddPane. 
	   It essentially means put this pane at the end of the list... 
	 In the InstallMenus code we special case the empty 
	title pane to give it a priority of 0. Life sucks... */
		if (panePriority != NULL)
		    *panePriority = -1;
		return;
	    }
	    if (panePriority != NULL) 
		*panePriority =  (priority == 0) ? -1 : priority;
	    p = selectionStr;
	    pLimit = selectionStr + selectionStrLen;
	    inSelection = TRUE;
	    inPriority = FALSE;
	    priority = 0;
	    break;
	case '~':
	    inPriority = TRUE;
	    break;
	default:
	    if (inPriority)
		priority = priority * 10 + *str - '0';
	    else
		if (p < pLimit)  *p++ = *str;
	    break;
    }
}
    
static struct mpitem *currentmp = NULL; 
static struct mpitem *firstmp = NULL; 

static struct mlitem *mlitem_New(s1, s2)
char *s1, *s2;
{
  struct mlitem *ml;
  ml = (struct mlitem *) malloc(sizeof(struct mlitem));
  if (s1 && *s1) {
    ml->oldname = (char *) malloc(strlen(s1) + 1);
    strcpy(ml->oldname,s1);
  } else 
    ml->oldname = NULL;
  if (s2 && *s2) {
    ml->newname = (char *) malloc(strlen(s2) + 1);
    strcpy(ml->newname,s2);
  } else 
    ml->newname = NULL;
  ml->state = MLITEM_UNDEF;
  ml->next = NULL;
  return ml;
}

static  struct mlitem *mlitem_Exists(mp, str)
struct mpitem *mp;
char *str;
{
    struct mlitem *tmpml = mp->ml;
    while(tmpml) {
	if(tmpml->oldname && str) {
	    if(strcmp(tmpml->oldname, str)==0) return tmpml;
	}
	tmpml=tmpml->next;
    }
    return tmpml;
}

static void mlitem_Add(mp, ml)
struct mpitem *mp;
struct mlitem *ml;
{
  struct mlitem *tmpml = mp->ml;
  if (mp == NULL || ml == NULL)
    return;
  if (mp->ml == NULL)
    mp->ml = ml;
  else { 
    while (tmpml->next)
      tmpml = tmpml->next;
    tmpml->next = ml;
  }
}

static struct mpitem *mpitem_New(str)
char *str;
{
  struct mpitem *mp;
  mp = (struct mpitem *) malloc(sizeof(struct mpitem));
  mp->name = (char *) malloc(strlen(str) + 1);
  strcpy(mp->name,str);
  mp->next = NULL;
  mp->ml = NULL;
  mp->load = MPITEM_LOAD;
  if (currentmp == NULL) firstmp=mp; else currentmp->next = mp;
  currentmp = mp;
  return mp;
}

static  struct mpitem *mpitem_Exists(str)
char *str;
{
  struct mpitem *tmp = firstmp;

  if (str == NULL || *str == NULL) return NULL;
  while (tmp && strcmp(tmp->name,str) != 0)
    tmp = tmp->next;
  return tmp;
}

static short mpitem_DoResolv(str)
char *str;
{
  struct mpitem *tmp = NULL;
  if ((tmp=mpitem_Exists(str)) != NULL)
    return tmp->load;
  return MPITEM_UNDEF;
}

#define INITIALSIZE 512

/* Hacked routine to rea a "whole file" into memory. */
static char *MapMenuFile(filename, fileLength)
    char *filename;
    long *fileLength; /* OUT */
{

    int fd;
    char *buffer;
    long length = 0;

    if ((fd = open(filename, O_RDONLY, 0)) >= 0) {

        struct stat statBuf;

        if (fstat(fd, &statBuf) >= 0) {

            long bufferSize; /* Current size of malloced block. */
            long bytesRead = 0;

            /* Find the size. In the case of special files, use a suitable default. */
            if ((statBuf.st_mode & S_IFMT) == S_IFREG)
                bufferSize = statBuf.st_size ;
            else
                bufferSize = INITIALSIZE;

            buffer = (char *) malloc(bufferSize + 1); /* +1 for NUL at end. */

            while (buffer != NULL && (bytesRead = read(fd, buffer + length, bufferSize - length )) > 0) {
                length += bytesRead;
                if (length >= bufferSize) {
                    bufferSize *= 2;
                    buffer = (char *) realloc(buffer, bufferSize + 1); /* +1 for NUL at end. */
                }
            }
            if (bytesRead < 0) {
                free(buffer);
                buffer = NULL;
            }
            else
                buffer[length] = '\0';
        }
        else
            buffer = NULL;

        close(fd);
    }
    else
        buffer = NULL;

    if (fileLength != NULL)
        *fileLength = length;
    return buffer;
}

#define UnmapMenuFile(mappedMemory) free(mappedMemory)

static int ReadMenuFile(filename, executeImmediately)
char *filename;
boolean executeImmediately;
{

    char *buffer;
    long length;

    if ((buffer = MapMenuFile(filename, &length)) != NULL) {

        char *p;
        int currentLine;

        currentLine = 0;

        p = buffer;
        while (p < buffer + length) {
	    char str[128];
	    char strvalue[128];
	    int i = 0;
            ++currentLine;
	    /* Skip to the end of line. */
	    while (*p != '\n' && *p != '\0' && *p != ' ')
		str[i++] = *p++;
	    while (*p == ' ') p++;
	    
	    str[i] = '\0';
	    if (str[0] != '\0' && *p != '\n' && *p != '\0') {
	      struct mpitem *mp;
	      struct mlitem *ml;	
	      char s1[128], s2[128];
	      char strcmd[128];
	      i = 0;
	      while (*p != '\n' && *p != '\0' && *p != ' ')
		strcmd[i++] = *p++;
	      strcmd[i] = '\0';
	      if ((mp=mpitem_Exists(str)) == NULL)
		mp = mpitem_New(str);
	      if (strcmp(strcmd,"deleteallmenus") == 0)
		mp->load = MPITEM_NOLOAD;
	      else if (strcmp(strcmd,"deletemenu") == 0 || 
		       strcmp(strcmd,"replacemenu") == 0) {
		while (*p == ' ') p++;
		i = 0;
		if (*p == '"') p++;
		while (*p != '\n' && *p != '\0' && *p != '"')
		  s1[i++] = *p++;
		s1[i] = '\0';
		if (*p == '"') p++;
		if (strcmp(strcmd,"replacemenu") == 0) {
		  i = 0;
		  while (*p == ' ') p++;
		  if (*p == '"') p++;
		  while (*p != '\n' && *p != '\0' && *p != '"')
		    s2[i++] = *p++;
		  s2[i] = '\0';
		  if (*p == '"') p++;
		}
		if ((ml=mlitem_Exists(mp,s1)) == NULL) {
		  ml = mlitem_New(s1,s2);
		  mlitem_Add(mp,ml);
		}
		if (strcmp(strcmd,"replacemenu") == 0)
		  ml->state = MLITEM_REPLACE;
		else
		  ml->state = MLITEM_DELETE;
		mp->load = MPITEM_LOAD;
	      }
	    }
	    while (*p != '\n' && *p != '\0') /*end of line */
	      *p++;
	    if (*p == '\n') p++;
	  }

        UnmapMenuFile(buffer);

        return 0;
    }
    else
        return -1;
}    


void InitMenuFile()
{
    char *al=environ_Get("ANDREWLANGUAGE");
    char *alf=environ_Get("ANDREWLANGUAGEMENUFILE");
    if(alf==NULL) alf=environ_GetProfile("AndrewLanguageMenuFile");
    if(al==NULL) al=environ_GetProfile("AndrewLanguage");
    if(alf==NULL && al) {
	char MenuFile[MAXPATHLEN];
	strcpy(MenuFile, environ_AndrewDir("/lib/"));
	strcat(MenuFile, al);
	strcat(MenuFile, ".menu");
	ReadMenuFile(MenuFile, TRUE);
    } else if(alf) ReadMenuFile(alf, TRUE);
}

static long nextMLVersion = 0;
static boolean initedmenufile=FALSE;
static boolean translatemenus=TRUE;

boolean menulist__InitializeObject(classID, self)
struct classheader *classID;
struct menulist *self;
{
    if(!initedmenufile) {
	InitMenuFile();
	initedmenufile=TRUE;
	translatemenus=environ_GetProfileSwitch("TranslateMenus", TRUE);
    }
    self->regionID = -1;
    self->curIM = NULL;
    self->version = 0;
    self->installVersion = -1;
    self->object = NULL;
    self->menus = NULL;
    self->refcount = (int *) malloc(sizeof(int));
    *self->refcount = 1;
    self->curMenu = NULL;
    self->menuChainBefore = NULL;
    self->menuChainAfter = NULL;
    self->curChainBefore = NULL;
    self->curChainAfter = NULL;
    self->selectMask=0;
    return TRUE;
}

void menulist__FinalizeObject(classID, self)
    struct classheader *classID;
    struct menulist *self;
{

    if (*self->refcount == 1) {
	menulist_ClearML(self);
	free(self->refcount);
    }
    else if (*self->refcount > 1) {
	*self->refcount -= 1;
    }
    menulist_ClearChain(self);
}

void menulist__SetView(self, view)
    struct menulist *self;
    struct view *view;
{

    self->object = (struct basicobject *) view;
}

struct menulist *menulist__Create(classID, view)
    struct classheader *classID;
    struct view *view;
{

    struct menulist *thisMenu;

    thisMenu = menulist_New();
    menulist_SetView(thisMenu, view);
    return thisMenu;
}

struct menulist *menulist__DuplicateML(self, view)
    struct menulist *self;
    struct view *view;
{

    struct menulist *newMenus;

    newMenus = menulist_Create(view);
    free(newMenus->refcount);
    *self->refcount += 1;
    newMenus->refcount = self->refcount;
    newMenus->menus = self->menus;
    return newMenus;
}

/* Copy a menu list's menu items. Used to implement copy-on-write for item lists.
 * This routine should only be called when *menulist->refcount > 1
 */
static void copyItems(menulist)
struct menulist *menulist;
{
    struct itemlist *traverse = menulist->menus;

    menulist->menus = NULL;
    for (; traverse != NULL; traverse = traverse->next) {
        struct itemlist *thisItem;

        thisItem = (struct itemlist *) malloc(sizeof(struct itemlist));
        thisItem->string = (char *) malloc(strlen(traverse->string) + 1);
        strcpy(thisItem->string, traverse->string);
        thisItem->proc = traverse->proc;
        thisItem->functionData = traverse->functionData;
	thisItem->enableMask = traverse->enableMask;
        thisItem->next = menulist->menus;

        menulist->menus = thisItem;
    }
    *menulist->refcount -= 1;
    menulist->refcount = (int *) malloc(sizeof(int));
    *menulist->refcount = 1;
}

static void SetPrio(p, sp)
int p;
char *sp;
{
    if(p<1) sp[0]='\0';
    else sprintf(sp, "~%d", p);
}
    
void menulist__AddToML(self, string, menuProc, functionData, mask)
    struct menulist *self;
    char *string;
    struct proctable_Entry *menuProc;
    long functionData; /* Actually any 32 bit crufty... */
    long mask;
{

    struct itemlist *thisItem;
    boolean link = FALSE;
    int doResolvMenu=MPITEM_UNDEF;
    struct mpitem *mp=NULL;
    struct mlitem *ml=NULL;
    char transbuf[1024];
    char cname[1024];
    char iname[1024];
    long cprio=0;
    long iprio=0;
    char scprio[16];
    char siprio[16];
    
    if (string == NULL)
        return;

    if(menuProc && menuProc->type) {
	doResolvMenu=mpitem_DoResolv(class_GetTypeName(menuProc->type));
	if(doResolvMenu==MPITEM_NOLOAD) return;
	mp=mpitem_Exists(class_GetTypeName(menuProc->type));
	if(mp) {
	    ml=mlitem_Exists(mp, string);
	    if(ml && ml->state==MLITEM_REPLACE) {
		cname[0]=iname[0]='\0';
		ExplodeMenuString(string, cname, sizeof(cname), &cprio, iname, sizeof(iname), &iprio);
		cname[0]=iname[0]='\0';
		ExplodeMenuString(ml->newname, cname, sizeof(cname), NULL, iname, sizeof(iname), NULL);
		SetPrio(cprio, scprio);
		SetPrio(iprio, siprio);
		sprintf(transbuf, "%s%s%s%s%s", cname, scprio, cname[0]=='\0'?"":",", iname, siprio);
		string=transbuf;
	    }
	}
    }
    if(translatemenus) {
	cname[0]=iname[0]='\0';
	ExplodeMenuString(string, cname, sizeof(cname), &cprio, iname, sizeof(iname), &iprio);
	SetPrio(cprio, scprio);
	SetPrio(iprio, siprio);
	sprintf(transbuf, "%s%s%s%s%s", messitem_Replace(cname), scprio,(cname[0]=='\0' && cprio < 1)?"":",", messitem_Replace(iname), siprio);
	string=transbuf;
    }
	
    
    if (*self->refcount > 1)
        copyItems(self);

    for (thisItem = self->menus; thisItem != NULL && (strcmp(thisItem->string, string) != 0); thisItem = thisItem->next);

    if (thisItem == NULL) {
	char *p;
        thisItem = (struct itemlist *) malloc(sizeof(struct itemlist));
        thisItem->string = (char *) malloc(strlen(string) + 1);
        strcpy(thisItem->string, string);
        link = TRUE;
    }
    thisItem->proc = menuProc;
    thisItem->functionData = functionData;
    thisItem->enableMask=mask;
    if (link) { /* Only link it in after the data is valid. */
        thisItem->next = self->menus;
        self->menus = thisItem;
    }
    self->version = self->menuVersion = nextMLVersion;
}

void menulist__DeleteFromML(self, string)
    struct menulist *self;
    char *string;
{

    struct itemlist *traverse, **previous = &(self->menus);

    if (string == NULL)
        return;

    if (*self->refcount > 1)
        copyItems(self);

    for (traverse = self->menus; traverse != NULL && (strcmp(traverse->string, string) != 0); traverse = traverse->next) {
        previous = &(traverse->next);
    }

    if (traverse != NULL) {
        *previous = traverse->next;
        free(traverse->string);
        free(traverse);
	self->version = self->menuVersion = nextMLVersion;
    }
}

boolean menulist__SetMask(self,mask)
struct menulist *self;
long mask;
{
    if(mask!=self->selectMask){
	self->selectMask=mask;
	return TRUE;
    }else
	return FALSE;
}

void menulist__ClearML(self)
    struct menulist *self;
{

    struct itemlist *traverse, *next;

    if (*self->refcount == 1)  {
        for (traverse = self->menus; traverse != NULL; traverse = next) {
            free(traverse->string);
            next = traverse->next;
            free(traverse);
        }
    }
    else {
        if (*self->refcount < 1)
            fprintf(stderr, "menulist: internal error, refcount < 1 in ClearML\n");
        else {
            *self->refcount -= 1;
            self->refcount = (int *) malloc(sizeof(int));
            *self->refcount = 1;
        }
    }
    self->menus = NULL;
    self->version = self->menuVersion = nextMLVersion;
}

boolean menulist__NextME(self, outString, outData, outProc)
    struct menulist *self;
    char **outString;
    long *outData;
    struct proctable_Entry **outProc;
{

    if (self->curMenu != NULL) {
        *outString = self->curMenu->string;
        *outData = self->curMenu->functionData;
        *outProc = self->curMenu->proc;
        self->curMenu = self->curMenu->next;
        return TRUE;
    }
    else
        return FALSE;
}

struct menulist *menulist__NextBeforeMC(self)
    struct menulist *self;
{

    register struct menulist *value;

    if (self->curChainBefore != NULL) {
        value = self->curChainBefore->menulist;
        self->curChainBefore = self->curChainBefore->next;
        return value;
    }
    return NULL;
}

struct menulist *menulist__NextAfterMC(self)
    struct menulist *self;
{

    register struct menulist *value;

    if (self->curChainAfter != NULL) {
        value = self->curChainAfter->menulist;
        self->curChainAfter = self->curChainAfter->next;
        return value;
    }
    return NULL;
}

void menulist__ChainBeforeML(self, chainee, key)
    struct menulist *self;
    struct menulist *chainee;
    long key;
{

    struct headerlist *tempHeader, *next, **previous;

    if (chainee == NULL) /* Need to handle posting of NULL since it is used to clear menus. */
        return;

    previous = &(self->menuChainBefore);
    for (tempHeader = self->menuChainBefore; tempHeader != NULL; tempHeader = next) {
        if (tempHeader->menulist == chainee) {
            if (tempHeader->assocKey != key)
                tempHeader->assocKey = key;
            return;
        }
        else if (tempHeader->assocKey == key) {
            *previous = next = tempHeader->next;
            free(tempHeader);
            continue;
        }
        next = tempHeader->next;
    }
    
    previous = &(self->menuChainAfter);
    for (tempHeader = self->menuChainAfter; tempHeader != NULL; tempHeader = next) {
        if (tempHeader->menulist == chainee || tempHeader->assocKey == key) {
            *previous = next = tempHeader->next;
            free(tempHeader);
            continue;
        }
        next = tempHeader->next;
    }

    tempHeader = (struct headerlist *) malloc(sizeof(struct headerlist));
    tempHeader->menulist = chainee;
    tempHeader->next = self->menuChainBefore;
    tempHeader->assocKey = key;
    self->menuChainBefore = tempHeader;
    self->version = nextMLVersion;
}

void menulist__ChainAfterML(self, chainee, key)
    struct menulist *self;
    struct menulist *chainee;
    long key;
{

    struct headerlist *tempHeader, *next, **previous;

    if (chainee == NULL) /* Need to handle posting of NULL since it is used to clear menus. */
        return;

    previous = &(self->menuChainBefore);
    for (tempHeader = self->menuChainBefore; tempHeader != NULL; tempHeader = next) {
        if (tempHeader->menulist == chainee || tempHeader->assocKey == key) {
            *previous = next = tempHeader->next;
            free(tempHeader);
            continue;
        }
        next = tempHeader->next;
    }
   
    previous = &(self->menuChainAfter);
    for (tempHeader = self->menuChainAfter; tempHeader != NULL; tempHeader = next) {
        if (tempHeader->menulist == chainee) {
            if (tempHeader->assocKey != key)
                tempHeader->assocKey = key;
            return;
        }
        else if (tempHeader->assocKey == key) {
            *previous = next = tempHeader->next;
            free(tempHeader);
            continue;
        }
        next = tempHeader->next;
    }

    tempHeader = (struct headerlist *) malloc(sizeof(struct headerlist));
    tempHeader->menulist = chainee;
    tempHeader->next = self->menuChainAfter;
    tempHeader->assocKey = key;
    self->menuChainAfter = tempHeader;
    self->version = nextMLVersion;
}

void menulist__UnchainML(self, key)
    struct menulist *self;
    long key;
{

    struct headerlist *traverse, **previous;

    previous = &(self->menuChainBefore);
    for (traverse = self->menuChainBefore; traverse != NULL && (traverse->assocKey != key); traverse = traverse->next)
        previous = &(traverse->next);

    if (traverse == NULL) {
        previous = &(self->menuChainAfter);
        for (traverse = self->menuChainAfter; traverse != NULL && (traverse->assocKey != key); traverse = traverse->next)
            previous = &(traverse->next);
    }

    if (traverse != NULL) {
        *previous = traverse->next;
        free(traverse);
        self->version = nextMLVersion;
    }
}

struct menulist *menulist__GetChainedML(self, key)
    struct menulist *self;
    long key;
{

    struct headerlist *traverse;

    for (traverse = self->menuChainBefore; traverse != NULL; traverse = traverse->next)
        if (traverse->assocKey != key)
            return traverse->menulist;
    for (traverse = self->menuChainAfter; traverse != NULL; traverse = traverse->next)
        if (traverse->assocKey != key)
            return traverse->menulist;
    return NULL;
}

void menulist__ClearChain(self)
    struct menulist *self;
{

    boolean didSomething = FALSE;
    struct headerlist *traverse, *next;

    for (traverse = self->menuChainBefore; traverse != NULL; traverse = next) {
        next = traverse->next;
        free(traverse);
        didSomething = TRUE;
    }
    self->menuChainBefore = NULL;
    for (traverse = self->menuChainAfter; traverse != NULL; traverse = next) {
        next = traverse->next;
        free(traverse);
        didSomething = TRUE;
    }
    self->menuChainAfter = NULL;

    if (didSomething)
        self->version = nextMLVersion;
}

int menulist__NextMLVersion(classID)
    struct classheader *classID;
{
    return nextMLVersion;
}

void menulist__IncrementMLVersion(classID)
    struct classheader *classID;
{
    ++nextMLVersion;
}
