/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University All rights Reserved. */

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



#include <list.ih>
#include <prefval.ih>
#include <mark.ih>
#include <style.ih>
#include <atom.ih>

struct prefdesc {
    char *name;
    char *app;
    char *group;
    char *seps;
    char *cond;
    char *val;
    struct prefval *obj;
    struct list *defviews;
    struct list *prevlines;
    enum prefval_Type type;
    long low, high;
    long listsize;
    boolean writetype;
    boolean writeviews;
    char *views;
    struct mark *mark;
    boolean shadow;
    boolean indefs;
    int order;
    boolean expert;
    long helppos;
    boolean freehelp;
    boolean freeobject;
    struct mark *pm;
};

struct prefline {
    struct list *prevlines;
    char *type;
    struct list *vl;
    char *app;
    char *name;
    char *group;
    char *cond;
    char *val;
    char *views;
    boolean shadow;
    long low, high;
    long helppos;
    int expert;
};

struct prefgroup {
    long grouphelp;
    char *name;
};

enum style_type {
    PrefsSubsection,
    PrefsGroupname
};

struct hstyles {
    long pos;
    long len;
    enum style_type type;
};

enum prefs_SortType {
    prefs_Name,
    prefs_App,
    prefs_Group,
    prefs_Order,
    prefs_MaxSortType
};

#define prefs_DS_VERSION 1

class prefs : text {
classprocedures:
    InitializeObject(struct prefval *self) returns boolean;
    FinalizeObject(struct prefval *self);
    TranslateViewName(char *name) returns char *;
methods:
    ReadDataPart() returns long;
    Sort(enum pref_SortType sortby, boolean perm);
    DuplicatePref(struct prefdesc *pd, char *newapp, char *newcond) returns struct prefdesc *;
    DeletePref(struct prefdesc *pd);
    WritePlain(FILE *fp, long id, long level) returns long;
    UpdateText();
    UpdateOneInText(struct prefdesc *pd);
    ReScan() returns long;
overrides:
    ObservedChanged(struct observable *changed, long val);
    ViewName() returns char *;
    Write(FILE *fp, long writeID, int level) returns long;
    Read(FILE *fp, long id) returns long;
macros:
macromethods:
    SetReadingDefaults(boolean rdefaults) (self->readingdefaults=(rdefaults))
data:
    struct list *prefs;
    boolean readingdefaults;
    struct prefline lastline;
    enum prefs_SortType sortby;
    char *lastheader;
    struct style *hstyle;
    struct style *pstyle;
    char *lastgroup;
    struct list *categories;
    boolean sane;
    struct text *help;
    long version;
    struct hstyles *llist;
    long llistsize, llistcnt;
    long maxorder;
    boolean selfmod;
};
