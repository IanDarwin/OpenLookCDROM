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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textobjects/RCS/dired.c,v 1.15 1993/09/22 19:34:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <attribs.h>

#include <sys/stat.h>
#include <stylesht.ih>
#include <style.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#include <filetype.ih>
#include <list.ih>

#include <dired.eh>

#define RootEnv(dired) \
    ((struct environment *) (dired)->header.text.rootEnvironment)

/*
 * DirIntoList returns a list of the files in a directory.
 * The elements of the list are fileinfo structures.
 * The directory is referenced from the current directory.
 * If the directory cannot be loaded, returns NULL and
 * errno contains the error code from the opendir(2) call.
 */

static int CompareFilenameProc(f1, f2)
struct fileinfo *f1, *f2;
{
    return strcmp(f1->fileName, f2->fileName);
}

LongModeLine(dname, fname, buf)
char *dname, *fname, *buf;
{
    struct stat stbuf;
    strcpy(buf, dname);
    if(buf[strlen(buf) - 1] != '/')
	strcat(buf, "/");
    strcat(buf, fname);
    if (stat(buf, &stbuf) < 0) {
        strcpy(buf, "? ");
        strcat(buf, fname);
        return;
    }
}

static struct list *DirIntoList(dname, longMode, dotFiles)
char *dname;
boolean longMode, dotFiles;
{
    struct list *list;
    DIR *dirp;
    DIRENT_TYPE *dp;

    if ((dirp = opendir(dname)) == NULL)
        return NULL;

    list = list_New();

    while ((dp = readdir(dirp)) != NULL) {
        struct fileinfo *fi;

        if (! dotFiles && dp->d_name[0] == '.' &&
          strcmp(dp->d_name, "..") != 0)
            continue;

        fi = (struct fileinfo *) malloc(sizeof (struct fileinfo));

        fi->fileName = malloc(strlen(dp->d_name) + 1);
        strcpy(fi->fileName, dp->d_name);

        if (longMode) {
            char buf[256];
            LongModeLine(dname, fi->fileName, buf);
            fi->dispName = malloc(strlen(buf) + 1);
            strcat(fi->dispName, buf);
        } else {
            fi->dispName = malloc(strlen(fi->fileName) + 1);
            strcpy(fi->dispName, fi->fileName);
        }

        fi->pos = fi->len = 0;
        fi->env = NULL;     /* Not marked (no env) */

        list_InsertSorted(list, fi, CompareFilenameProc);
    }

    closedir(dirp);
    return list;
}

/*
 * Free a list
 */

static int FreeProc(fi, rock)
struct fileinfo *fi;
long rock;
{
    free(fi->fileName);
    free(fi->dispName);
    return TRUE;
}

static void DestroyList(list)
struct list *list;
{
    list_Enumerate(list, FreeProc, 0);
    list_Destroy(list);
}

/*
 * ListIntoText clears the text, then inserts one line for each file.
 * The starting position for each file's text (and the length) is
 * updated in each list entry. A carriage return is inserted after
 * each entry.  Styles are not wrapped around the highlighted
 * (env != NULL) entries.
 */

static int InsTextProc(fi, dired)
struct fileinfo *fi;
struct dired *dired;
{
    fi->pos = dired_GetLength(dired);
    fi->len = strlen(fi->dispName);

    dired_InsertCharacters(dired, fi->pos, fi->dispName, fi->len);
    dired_InsertCharacters(dired, fi->pos + fi->len, "\n", 1);

    return TRUE;
}

static void SetupStyles();

static void ListIntoText(self, list)
struct dired *self;
struct list *list;
{

    dired_SetReadOnly(self, FALSE);
    dired_Clear(self);
    SetupStyles(self);
    list_Enumerate(self->flist, InsTextProc, self);
    dired_SetReadOnly(self, TRUE);
    dired_NotifyObservers(self, 0);
}

/*
 * Class procedures
 */

static void SetupStyles(self)
struct dired *self;
{
    struct attributes templateAttribute;
    struct style *style;

    templateAttribute.key = "template";
    templateAttribute.value.string = "dired";
    templateAttribute.next = NULL;

    dired_SetAttributes(self, &templateAttribute);

    style = stylesheet_Find(self->header.text.styleSheet, "select");
    if (style == NULL) {
        style = style_New();
        style_AddNewFontFace(style, fontdesc_Bold);
    }

    self->markedStyle = style;

    style = stylesheet_Find(self->header.text.styleSheet, "global");
    if (style != NULL)
        dired_SetGlobalStyle(self, style);
}

boolean dired__InitializeObject(classID, self)
struct classheader *classID;
struct dired *self;
{
    self->dir = NULL;
    self->flist = NULL;
    self->longMode = FALSE;
    self->dotFiles = FALSE;

    dired_SetReadOnly(self, TRUE);
    dired_SetExportEnvironments(self, FALSE);

    return TRUE;
}

void dired__FinalizeObject(classID, self)
struct classheader *classID;
struct dired *self;
{
    if (self->flist != NULL)
        DestroyList(self->flist);
    style_Destroy(self->markedStyle);
}

/*
 * Overrides
 *
 * SetAttributes is used to tell dired which dir to use.
 * It's not very proper, but it's necessary so that the buffer
 * package has to know a bare minimum about dired.
 *
 * NOTE: the "dir" attribute should be last in the attr list
 */

void dired__SetAttributes(self, attributes)
struct dired *self;
struct attributes *attributes;
{
    super_SetAttributes(self, attributes);

    for (; attributes != NULL; attributes = attributes->next)
        if (strcmp(attributes->key, "dir") == 0)
            dired_SetDir(self, attributes->value.string);
        else if (strcmp(attributes->key, "longmode") == 0)
            dired_SetLongMode(self, attributes->value.integer);
        else if (strcmp(attributes->key, "dotfiles") == 0)
            dired_SetDotFiles(self, attributes->value.integer);
}

/*
 * Prevent checkpoints
 */

long dired__GetModified(self)
struct dired *self;
{
    return 0;
}

/*
 * Methods
 */

/*
 * SetDir reads a directory into the list and into the document.
 * The current longMode and dotFiles are used.
 * If an error occurs in opening the directory, then the
 * routine: returns -1, does not at all affect the list or
 * text, and leaves the error code from opendir(2) in errno.
 */

long dired__SetDir(self, dname)
struct dired *self;
char *dname;
{
    char newDir[256];
    struct list *newList;

    if (dname == NULL)
        return -1;

    filetype_CanonicalizeFilename(newDir, dname, sizeof (newDir));

    newList = DirIntoList(newDir, self->longMode, self->dotFiles);
    if (newList == NULL)
        return -1;

    if (self->dir != NULL)
        free(self->dir);
    self->dir = malloc(strlen(newDir) + 1);
    strcpy(self->dir, newDir);

    if (self->flist != NULL)
        DestroyList(self->flist);

    self->flist = newList;
    ListIntoText(self, newList);

    return 0;
}

char *dired__GetDir(self)
struct dired *self;
{
    return self->dir;
}

static int FindNameProc(fi, name)
struct fileinfo *fi;
char *name;
{
    return (strcmp(fi->fileName, name) != 0);
}

struct fileinfo *LookupFile(self, name)
struct dired *self;
char *name;
{
    if (self->flist == NULL)
        return NULL;
    return (struct fileinfo *)
      list_Enumerate(self->flist, FindNameProc, name);
}

static int FindPosProc(fi, pos)
struct fileinfo *fi;
long pos;
{
    return (pos < fi->pos || pos > fi->pos + fi->len);
}

struct fileinfo *LookupPos(self, pos)
struct dired *self;
long pos;
{
    if (self->flist == NULL)
        return NULL;
    return (struct fileinfo *)
      list_Enumerate(self->flist, FindPosProc, pos);
}

/*
 * Given a position in the document, returns the filename in
 *  in which the document position falls.
 */

char *dired__Locate(self, pos)
struct dired *self;
long pos;
{
    struct fileinfo *fi = LookupPos(self, pos);
    return (fi == NULL) ? NULL : fi->fileName;
}

/*
 * Routine to wrap a specified style around a given file
 * entry.  Previous styles are removed.
 * If the style is NULL, then no style is wrapped but the
 * previous style is still removed.
 * Does not change the document unnecessarily.
 */

static void WrapStyle(self, fi, style)
struct dired *self;
struct fileinfo *fi;
struct style *style;
{
    dired_SetReadOnly(self, FALSE);

    if (fi->env != NULL) {
        if (fi->env->data.style == style)
             return;
        environment_Remove(self->header.text.rootEnvironment,
          fi->pos, fi->len, environment_Style, TRUE);
        dired_RegionModified(self, fi->pos, fi->len);
        dired_NotifyObservers(self, 0);
    }

    fi->env = NULL;

    if (style != NULL) {
        fi->env = environment_InsertStyle(self->header.text.rootEnvironment,
          fi->pos, style, TRUE);
        environment_SetLength(fi->env, fi->len);
        dired_RegionModified(self, fi->pos, fi->len);
        dired_NotifyObservers(self, 0);
    }

    dired_SetReadOnly(self, TRUE);
}

/*
 * Given a filename, mark it (if it wasn't marked)
 */

void dired__Mark(self, fname)
struct dired *self;
char *fname;
{
    struct fileinfo *fi;
    fi = LookupFile(self, fname);
    if (fi != NULL)
        WrapStyle(self, fi, self->markedStyle);
}

void dired__Unmark(self, fname)
struct dired *self;
char *fname;
{
    struct fileinfo *fi = LookupFile(self, fname);
    if (fi != NULL)
        WrapStyle(self, fi, NULL);
}

boolean dired__IsMarked(self, fname)
struct dired *self;
char *fname;
{
    struct fileinfo *fi = LookupFile(self, fname);
    return (fi != NULL && fi->env != NULL);
}

static int AnythingProc(fi, rock)
struct fileinfo *fi;
long rock;
{
    return (fi->env == NULL);
}

boolean dired__AnythingMarked(self)
struct dired *self;
{
    if (self->flist == NULL)
        return FALSE;
    return (list_Enumerate(self->flist, AnythingProc, 0) != NULL);
}

struct emargs {
    procedure proc;
    boolean all;
    long rock;
};

/*
 * EnumerateAll calls proc for every listed file.
 * EnumerateMarked calls proc only for currently highlighted files.
 *
 * The enumerates call the proc with arguments (filename, rock).
 * If the proc returns FALSE, the enumeration stops and returns the
 * name of the file being processed.  Otherwise, the enumeration
 * continues until the end, in which case NULL is returned.
 */

static int EnumProc(fi, args)
struct fileinfo *fi;
struct emargs *args;
{
    if (args->all || fi->env != NULL)
        return (*args->proc)(fi->fileName, args->rock);
    return TRUE;
}

static char *DoEnumerate(self, proc, rock, all)
struct dired *self;
procedure proc;
long rock;
boolean all;
{
    struct fileinfo *result;
    struct emargs args;
    if (self->flist == NULL)
        return NULL;
    args.proc = proc, args.rock = rock, args.all = all;
    result = (struct fileinfo *) list_Enumerate(self->flist, EnumProc, &args);
    return (result == NULL) ? NULL : result->fileName;
}

char *dired__EnumerateMarked(self, proc, rock)
struct dired *self;
procedure proc;
long rock;
{
    return DoEnumerate(self, proc, rock, FALSE);
}

char *dired__EnumerateAll(self, proc, rock)
struct dired *self;
procedure proc;
long rock;
{
    return DoEnumerate(self, proc, rock, TRUE);
}
