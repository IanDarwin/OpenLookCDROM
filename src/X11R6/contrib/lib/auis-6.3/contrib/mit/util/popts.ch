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


#define PROFILETYPENONE 0
#define PROFILETYPEEXISTS 1
#define PROFILETYPESWITCH 2

struct Option {
    char *label;    /* The view's prompt */
    char *name;	    /* Unique name to identify property */
    char *env;	    /* If it has an environment variable, this is its name */
    int	profiletype;	/* what kind of value is stored in the profile */
    char *envOn;	    /* What 'value' the environment variable must 
			     have to make value an 'on'. */
    char *envOff;
    procedure func; /* The callback function for value_DATACHANGED */
    int value; /* Set by dataobject for the view and maintained by callbacks */
};

struct Check {
    struct value *obj;
    struct checkv *view;
};

struct buttonList {
  struct buttonList *next;
  struct pushbutton *butt;
  struct pushbuttonview *buttv;
};

struct lplist {
    struct lplist *next;
    struct lpair *lp;
};

#define MAXCHECKS 10

class printopts [popts] : view {
overrides:

	FullUpdate(enum view_UpdateType type, 
		long left, long top, long width, long height);
	Update();
	Hit(enum view_MouseAction action, long x, long y, long n)
			returns struct view *;
	DesiredSize(long width, long height, enum view_DSpass pass, 
				long *desiredWidth, long *desiredHeight) 
			returns enum view_DSattributes;
	Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
	GetApplicationLayer() returns struct frame *;
	PostKeyState(struct keystate *ks);
	PostMenus(struct menulist *ml);
	LinkTree(struct view *parent);

classprocedures:

	InitializeClass() returns boolean;
	InitializeObject(struct thisobject *self) returns boolean;
	FinalizeObject(struct thisobject *self);

data:

	boolean embedded;
        boolean ResourcesPosted;
	struct lpair *image;
	struct Check check[MAXCHECKS];
	struct bpair *checklist;
	int *values;
	struct strinput *printername;
	char pnamevalue[81];

	struct suite *suite;
	struct menulist *menus;
	struct keystate *keys;
	boolean OnScreen;
	struct lplist *lplist;
	struct buttonList *blist;
	struct lpair *pname;
};
