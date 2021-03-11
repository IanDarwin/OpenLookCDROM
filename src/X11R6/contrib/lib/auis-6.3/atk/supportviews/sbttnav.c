/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/sbttnav.c,v 1.5 1992/12/15 21:43:33 rr2b R6tape $";
#endif


 

#include <sys/param.h>	/* for MAXPATHLEN */
#include <stdio.h>
#include <class.h>
#include <andrewos.h>
#include <util.h>
#include <cursor.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <im.ih>
#include <menulist.ih>
#include <message.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <atom.ih>

#include "sbutton.ih"
#include "sbttnav.eh"

#define PROMPTFONT "andysans12b"

/* Forward Declarations */
static void ShadowColorProc(), LabelColorProc(), TriggerProc(), BDeleteProc(), GDeleteProc(), RenameProc(), GroupProc(), NewGroupProc(), LabelProc(), FontProc(), StyleProc(), ColorProc(), AddButtonProc(), SetRowsProc(), SetColsProc();

/* Global Variables */
static struct menulist *menulist = NULL;

struct proctable_Entry *scpe=NULL, *lcpe=NULL, *triggerpe=NULL, *bdeletepe=NULL, *gdeletepe=NULL, *renamepe=NULL, *bgpe=NULL, *labelpe=NULL, *fontpe=NULL, *stylepe=NULL, *colorpe=NULL;

static char *Intern(str)
char *str;
{
    struct atom *a=atom_Intern(str);
    if(a!=NULL) return atom_Name(a);
    else return NULL;
}

boolean sbttnav__InitializeClass(c)
struct classheader *c;
{
    /* 
      Initialize all the class data.
      Set up the proc table entries and the menu list 
      (which is cloned for each instance of this class).
      */
    struct proctable_Entry *proc = NULL;

    if ((menulist = menulist_New()) == NULL) return(FALSE);

    if ((bgpe = proctable_DefineProc("sbuttonv-set-button-group", GroupProc, &sbttnav_classinfo, NULL, "Choose which group the button will take its appearance from.")) == NULL) return FALSE;
    
    if ((labelpe = proctable_DefineProc("sbuttonv-set-label", LabelProc, &sbttnav_classinfo, NULL, "Prompts for user to set the text string of the sbutton.")) == NULL) return(FALSE);
    menulist_AddToML(menulist, "SButton~20,Set Label~10", labelpe, NULL, 0);

    if ((triggerpe = proctable_DefineProc("sbuttonv-set-trigger", TriggerProc, &sbttnav_classinfo, NULL, "Prompts for user to set the text string of the sbutton.")) == NULL) return(FALSE);
    menulist_AddToML(menulist, "SButton~20,Set Trigger~11", triggerpe, NULL, 0);

    if ((fontpe = proctable_DefineProc("sbuttonv-set-font", FontProc, &sbttnav_classinfo, NULL, "Prompts for user to set the font of the sbutton.")) == NULL) return(FALSE);

    if ((stylepe = proctable_DefineProc("sbuttonv-set-style", StyleProc, &sbttnav_classinfo, NULL, "Prompts for user to set the appearance of the SButton.")) == NULL) return(FALSE);
    if ((colorpe = proctable_DefineProc("sbuttonv-set-colors", ColorProc, &sbttnav_classinfo, NULL, "Prompts for user to set the foreground and background color of the SButton.")) == NULL) return(FALSE);

    if ((scpe = proctable_DefineProc("sbuttonv-set-shadow-colors", ShadowColorProc, &sbttnav_classinfo, NULL, "Prompts for user to set the top, bottom and middle shadow colors of the SButton.")) == NULL) return(FALSE);

    if ((lcpe = proctable_DefineProc("sbuttonv-set-label-color", LabelColorProc, &sbttnav_classinfo, NULL, "Prompts for user to set the foreground and background color of the SButton's label.")) == NULL) return(FALSE);
    
    if ((proc = proctable_DefineProc("sbuttonv-add-button", AddButtonProc, &sbttnav_classinfo, NULL, "Prompts for user to name a new button.")) == NULL) return(FALSE);

    menulist_AddToML(menulist, "SButton~20,Add Button~30", proc, NULL, 0);

    if ((proc = proctable_DefineProc("sbuttonv-set-rows", SetRowsProc, &sbttnav_classinfo, NULL, "Sets the number of rows to be used for buttons. Enough columns will be used to ensure that all buttons are visible.")) == NULL) return(FALSE);

    menulist_AddToML(menulist, "SButton~20,Set Rows~71", proc, NULL, 0);
    
    if ((proc = proctable_DefineProc("sbuttonv-set-columns", SetColsProc, &sbttnav_classinfo, NULL, "Sets the number of columns to be used for buttons. Enough rows will be used to ensure that all buttons are visible.")) == NULL) return(FALSE);

    menulist_AddToML(menulist, "SButton~20,Set Columns~72", proc, NULL, 0);

    if ((proc = proctable_DefineProc("sbuttonv-new-group", NewGroupProc, &sbttnav_classinfo, NULL, "Creates a new button group.")) == NULL) return FALSE;

    menulist_AddToML(menulist, "SButton~20,New group~95", proc, NULL, 0);

    if ((proc = proctable_DefineProc("sbuttonv-delete", BDeleteProc, &sbttnav_classinfo, NULL, "Deletes the selected button.")) == NULL) return FALSE;

    menulist_AddToML(menulist, "SButton~20,Delete button~31", proc, NULL, 0);

    if ((renamepe = proctable_DefineProc("sbuttonv-rename-group", RenameProc, &sbttnav_classinfo, NULL, "Renames the group specified by the rock given.")) == NULL) return FALSE;

    if ((gdeletepe = proctable_DefineProc("sbuttonv-delete-group", GDeleteProc, &sbttnav_classinfo, NULL, "Deletes the group specified by the string given as a rock.")) == NULL) return FALSE;
    return(TRUE);
}

static struct themenus {
    char *name;
    struct proctable_Entry **pe;
} mymenus[]= {
    {"Set Button Group~11", &bgpe},
    {"Set Font~22", &fontpe},
    {"Set Style~23", &stylepe},
    {"Set Colors~24", &colorpe},
    {"Set Shadow Colors~25", &scpe},
    {"Set Label Colors~26", &lcpe},
    {NULL, NULL},
    {"Rename Group~30", &renamepe},
    {"Delete Group~35", &gdeletepe},
    {NULL, NULL}
};

static struct sbutton_prefs *LookupGroupPrefs(self, name)
struct sbttnav *self;
char *name;
{
    struct groups *g=self->groups;
    while(g) {
	if(g->prefs->name!=NULL && !strcmp(g->prefs->name, name)) return g->prefs;
	g=g->next;
    }
    return NULL;
}

static struct groups **LookupGroup(self, name)
struct sbttnav *self;
char *name;
{
    struct groups **g=(&self->groups);
    while(*g) {
	if((*g)->prefs->name!=NULL && !strcmp((*g)->prefs->name, name)) return g;
	g=(&(*g)->next);
    }
    return NULL;
}

static void AddGroupMenu(self, prefs, prio)
struct sbttnav *self;
struct sbutton_prefs *prefs;
int prio;
{
    char buf[256];
    struct groups *g;
    struct menulist *ml;
    int len=strlen(prefs->name);
    
    g=(struct groups *)malloc(sizeof(struct groups));
    if(g==NULL) return;

    g->prefs=prefs;
    prefs->refcount++; /* declare our interest in this set of preferences */
    
    ml=menulist_Create(self);
    if(ml==NULL) {
	free(g);
	return;
    }
    
    if(prio<0) {
	g->prio=22+self->groupcount;
	self->groupcount++;
    } else g->prio=prio;
    
    g->ml=ml;
    if(len<sizeof(buf)-32) {
	struct themenus *tm=mymenus;
	while(tm->name) {
	    strcpy(buf, prefs->name);
	    sprintf(buf+len, "~%d,", g->prio);
	    strcat(buf,tm->name);
	    menulist_AddToML(ml, buf, *tm->pe, prefs->name, 0);
	    tm++;
	}
	if(strcmp(prefs->name, "Default")) {
	    tm++;
	    while(tm->name) {
		strcpy(buf, prefs->name);
		sprintf(buf+len, "~%d,", g->prio);
		strcat(buf,tm->name);
		menulist_AddToML(ml, buf, *tm->pe, prefs->name, 0);
		tm++;
	    }
	}
	g->next=self->groups;
	self->groups=g;
    } else {
	menulist_Destroy(ml);
	free(g);
    }
    return;
}
    
    
struct menusrock {
    struct sbttnav *self;
    long count;
};

static boolean domenus(b, i, si, mr)
struct sbutton *b;
int i;
struct sbutton_info *si;
struct menusrock *mr;
{
    struct groups *g=mr->self->groups;
    while(g) {
	if(g->prefs==si->prefs) {
	    menulist_ClearChain(g->ml);
	    menulist_ChainAfterML(mr->self->ml, g->ml, g->ml);
	    return FALSE;
	}
	g=g->next;
    }
    if(si->prefs->name!=NULL) AddGroupMenu(mr->self, si->prefs, -1);
    return FALSE;
}
    
void sbttnav__PostMenus(self, ml)
struct sbttnav *self;
struct menulist *ml;
{
    struct sbutton *b=sbttnav_ButtonData(self);
    struct menusrock mr;
    struct groups *g;
    mr.self=self;
    mr.count=0;
    menulist_ClearChain(self->ml);
    sbutton_Enumerate(b, domenus, &mr);
    g=self->groups;
    while(g) {
	menulist_ClearChain(g->ml);
	menulist_ChainAfterML(self->ml, g->ml, g->ml);
	g=g->next;
    }
    if (ml) menulist_ChainAfterML(self->ml, ml, ml);
    super_PostMenus(self, self->ml);
}

boolean sbttnav__Touch(self, ind, action)
struct sbttnav *self;
int ind;
enum view_MouseAction action;
{
    struct sbutton *b=sbttnav_ButtonData(self);
    switch(action) {
	case view_RightDown:
	    self->dragging=sbttnav_LastButton(self);
	    sbttnav_WantInputFocus(self, self);
	    if(sbttnav_GetIM(self)) {
		self->cursor=cursor_Create(self);
		if(self->cursor) {
		    cursor_SetStandard(self->cursor, Cursor_LeftPointer);
		    im_SetWindowCursor(sbttnav_GetIM(self), self->cursor);
		}
	    }
	    break;
	case view_RightUp:
	    sbutton_DeActivateButton(b, sbttnav_LastButton(self));
	    if(sbttnav_LastButton(self)!=self->dragging) sbutton_Swap(b, sbttnav_LastButton(self), self->dragging);
	    if(sbttnav_GetIM(self) && self->cursor) {
		im_SetWindowCursor(sbttnav_GetIM(self), NULL);
		cursor_Destroy(self->cursor);
	    }
		
    }
    return super_Touch(self, ind, action);
}

boolean sbttnav__InitializeObject(classID, self)
struct classheaded *classID;
struct sbttnav *self;
{
    (void) sbttnav_SetActiveMouseButtons(self, sbuttonv_LEFTBUTTON, sbuttonv_RIGHTBUTTON);
  
    self->ml = menulist_DuplicateML(menulist, self);
    if(self->ml == NULL) return FALSE;
    self->groups=NULL;
    self->groupcount=0;
    self->dragging=(-1);
    self->cursor=NULL;
    return TRUE;
}

void sbttnav__FinalizeObject(classID, self)
struct classheader *classID;
struct sbttnav *self;
{
    struct groups *g=self->groups;
    if(self->ml!=NULL) {
	menulist_Destroy(self->ml);
	self->ml=NULL;
    }
    while(g) {
	struct groups *h=g->next;
	sbutton_FreePrefs(g->prefs);
	menulist_Destroy(g->ml);
	free(g);
	g=h;
    }
}


static void BDeleteProc(self, param)
struct sbttnav *self;
long param;
{
    char buf[1024];
    struct sbutton *b=sbttnav_ButtonData(self);
    int i=sbttnav_LastButton(self);
    char *button=sbutton_GetLabel(b, i);

    if(button==NULL) button="Push Me";
    sbutton_Delete(b, i);

    strcpy(buf, "Deleted button ");
    strncat(buf, button, sizeof(buf)-strlen(buf)-8);
    strcat(buf, ".");
    message_DisplayString(self, 0, buf);
}

static void LabelProc(self, param)
struct sbttnav *self;
long param;
{
    /*
      This is the routine which asks the user for a new text label. */

    char buf[MAXPATHLEN];
    struct sbutton *b = sbttnav_ButtonData(self);
    char *oldtext;

    oldtext = sbutton_GetLabel(b, sbttnav_LastButton(self));
    if (message_AskForString(self,50,"Enter new label for button: ", oldtext, buf, sizeof(buf)) >= 0) {
	sbutton_SetLabel(b, sbttnav_LastButton(self), buf);
	message_DisplayString(self, 10, "Changed button label.");
    }
}

static void TriggerProc(self, param)
struct sbttnav *self;
long param;
{
    char buf[MAXPATHLEN];
    struct sbutton *b = sbttnav_ButtonData(self);
    char *oldtext;

    if(sbutton_GetTrigger(b, sbttnav_LastButton(self))) oldtext = atom_Name(sbutton_GetTrigger(b, sbttnav_LastButton(self)));
    else oldtext=NULL;
    if (message_AskForString(self,50,"Enter new trigger for button: ", oldtext, buf, sizeof(buf)) >= 0) {
	sbutton_SetTrigger(b, sbttnav_LastButton(self), buf);
	message_DisplayString(self, 10, "Changed button trigger.");
    }
}

static void FontProc(self, param)
struct sbttnav *self;
long param;
{
/*
  This is the routine which asks the user for a new font.
  It sucks, but I don't know how to smoothly integrate this button
  with a textview-like font change.  Oh well.
*/

  char buf[MAXPATHLEN], name[MAXPATHLEN];
  long style, size;
  struct sbutton *b = sbttnav_ButtonData(self);
  struct fontdesc *fd;
  struct sbutton_prefs *prefs;
  prefs=sbutton_GetDefaultPrefs(b);
  if(param>255) {
      /* we have an arg assume it is the preferences group to modify. */
      prefs=LookupGroupPrefs(self, (char *)param);
      if(prefs==NULL) {
	  message_DisplayString(self, 50, "Bad group name given.");
	  return;
      }
  }
  sprintf(name, "Enter new fontname for group %s:", (prefs->name!=NULL)?prefs->name:"Default");
  if (message_AskForString(self, 50, name, PROMPTFONT, buf, sizeof(buf)) >= 0) {
      if (!fontdesc_ExplodeFontName(buf, name, sizeof(name), &style, &size)) {
	  message_DisplayString(self, 50, "Couldn't parse fontname.");
	  return;
      }
      if ((fd = fontdesc_Create(name,style,size))!=NULL) {
	  sbutton_GetFont(prefs) = fd;
	  message_DisplayString(self, 10, "Changed font.");
      } else {
	  message_DisplayString(self, 50, "Font change failed.  Using old font.");
      }
  }
  sbutton_SetModified(b);
  sbutton_SetChangeFlag(b, sbutton_ALLCHANGED|sbutton_SIZECHANGED);
  sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
}


static void StyleProc(self, param)
struct sbttnav *self;
long param;
{
    /*
      This is the routine which asks the user for a new sbutton appearance.
	  */

    struct sbutton *b = sbttnav_ButtonData(self);
    struct sbutton_prefs *prefs = sbutton_GetDefaultPrefs(b);
    static char *style_menu[] = {
	"Plain Text",
	"Boxed Text",
	"Three Dimensional",
	"Simple Boxed Text",
	"OSF/Motif",
	NULL
    };
    int choice;
     if(param>255) {
	/* we have an arg assume it is the preferences group to modify. */
	prefs=LookupGroupPrefs(self, (char *)param);
	if(prefs==NULL) {
	    message_DisplayString(self, 50, "Bad group name given.");
	    return;
	}
    }
    choice=sbutton_GetStyle(prefs);
    if (message_MultipleChoiceQuestion(self,99,"Pick a new style:", choice, &choice, style_menu, NULL)>= 0) {
	sbutton_GetStyle(prefs) =choice;
	message_DisplayString(self, 10, "Changed button style.");
    } else {
	message_DisplayString(self, 10, "Choice cancelled.");
    }
    sbutton_SetModified(b);
    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED|sbutton_SIZECHANGED);
    sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
}

static void ColorProc(self, param)
struct sbttnav *self;
long param;
{
    /*
      This is the routine which asks the user for  new sbutton colors.
	  */

    char buf1[MAXPATHLEN], buf2[MAXPATHLEN];
    struct sbutton *b = sbttnav_ButtonData(self);
    char *oldcolor;
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(b);

    if(param>255) {
	/* we have an arg assume it is the preferences group to modify. */
	prefs=LookupGroupPrefs(self, (char *)param);
	if(prefs==NULL) {
	    message_DisplayString(self, 50, "Bad group name given.");
	    return;
	}
    }
    oldcolor = sbutton_GetForeground(prefs);
    
    if (message_AskForString(self,50,"Enter new foreground color for button: ", oldcolor?oldcolor:"black", buf1, sizeof(buf1)) >= 0) {
	char buf3[1024];
	if(buf1[0]=='\0') {
	    sbutton_GetForeground(prefs)=NULL;
	    message_DisplayString(self, 10, "Restored button foreground color to the default.");
	} else {
	    sbutton_GetForeground(prefs) = Intern(buf1);
	    if(sbutton_GetForeground(prefs)!=NULL) {
		sprintf(buf3, "Changed button foreground color to %s.", buf1);
	    } else {
		sbutton_GetForeground(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button foreground color.");
	    }
	}
	message_DisplayString(self, 10, buf3);
    } else return;

    oldcolor = sbutton_GetBackground(prefs);

    if (message_AskForString(self,50,"Enter new background color for button: ", oldcolor?oldcolor:"white", buf2, sizeof(buf2)) >= 0) {
	char buf3[1024];
	if(buf2[0]=='\0') {
	    sbutton_GetBackground(prefs) = NULL;
	    message_DisplayString(self, 10, "Restored button background color to the default.");
	} else {
	    sbutton_GetBackground(prefs) = Intern(buf2);
	    if(sbutton_GetBackground(prefs)!=NULL) {
		sprintf(buf3, "Changed button background color to %s.", buf2);
	    } else {
		sbutton_GetBackground(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button background color.");
	    }
	    message_DisplayString(self, 10, buf3);
	}
    } else return;
    sbutton_SetModified(b);
    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
    sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
}

static void GroupProc(self, rock)
struct sbttnav *self;
long rock;
{
    struct sbutton *b=sbttnav_ButtonData(self);
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(b);
    if(rock>255) {
	/* we have an arg assume it is the preferences group to modify. */
	prefs=LookupGroupPrefs(self, (char *)rock);
	if(prefs==NULL) {
	    message_DisplayString(self, 50, "Bad group name given.");
	    return;
	}
    }
    sbutton_SetPrefs(b, sbttnav_LastButton(self), prefs);
    message_DisplayString(self, 0, "Set button group.");
}

static void AddButtonProc(self, rock)
struct sbttnav *self;
long rock;
{
    char buf[1024];
    struct sbutton *b=sbttnav_ButtonData(self);
    if (message_AskForString(self,50,"Enter label for new button: ", NULL, buf, sizeof(buf)) >= 0) {
	sbutton_SetLabel(b, b->count, buf);
    }
    message_DisplayString(self, 0, "Added new button.");
}

static void SetRowsProc(self, rock)
struct sbttnav *self;
long rock;
{
    char buf[1024];
    struct sbutton *b=sbttnav_ButtonData(self);
    if (message_AskForString(self,50,"Rows: ", NULL, buf, sizeof(buf)) >= 0) {
	int i=atoi(buf);
	if(i<=0) {
	    message_DisplayString(self, 50, "The number of rows should be a number greater than zero.");
	    return;
	}
	sbutton_SetLayout(b, i, sbutton_GetCols(b), sbutton_GrowColumns);
    }
}

static void SetColsProc(self, rock)
struct sbttnav *self;
long rock;
{
    char buf[1024];
    struct sbutton *b=sbttnav_ButtonData(self);
    if (message_AskForString(self,50,"Columns: ", NULL, buf, sizeof(buf)) >= 0) {
	int i=atoi(buf);
	if(i<=0) {
	    message_DisplayString(self, 50, "The number of columns should be a number greater than zero.");
	    return;
	}
	sbutton_SetLayout(b, sbutton_GetRows(b), i, sbutton_GrowRows);
    }
}

static void NewGroupProc(self, rock)
struct sbttnav *self;
long rock;
{
    char buf[1024];
    struct sbutton *b=sbttnav_ButtonData(self);
    if (message_AskForString(self,50,"Name for new button group: ", NULL, buf, sizeof(buf)) >= 0) {
	struct sbutton_prefs *new;
	new=sbutton_DuplicatePrefs(sbutton_GetDefaultPrefs(b), buf);
	new->refcount=1;
	if(new->name!=NULL) {
	    AddGroupMenu(self, new, -1);
	}
    }
    sbttnav_PostMenus(self, NULL);
}

static void RenameProc(self, rock)
struct sbttnav *self;
long rock;
{
    char buf[1024];
    struct sbutton *b=sbttnav_ButtonData(self);
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(b);
    struct groups **g, *h=NULL;
    int prio;
    if(rock<=255) {
	message_DisplayString(self, 50, "Bad group name given.");
	return;
    }
    /* we have an arg assume it is the preferences group to modify. */
    g=LookupGroup(self, (char *)rock);
    if(g==NULL) {
	message_DisplayString(self, 50, "Bad group name given.");
	return;
    }
    if (message_AskForString(self,50,"New name for button group: ", NULL, buf, sizeof(buf)) >= 0) {
	if(LookupGroup(self, buf)!=NULL) {
	    message_DisplayString(self, 75, "That name is already in use.");
	    return;
	}
	/* get out the important data then destroy the old structures... */
	prio=(*g)->prio;
	prefs=(*g)->prefs;
	menulist_ClearChain((*g)->ml);
	menulist_Destroy((*g)->ml);
	h=(*g)->next;
	free(*g);
	*g=h;
	if(prefs->name!=NULL) free(prefs->name);
	prefs->name=NewString(buf);
	if(prefs->name!=NULL) AddGroupMenu(self, prefs, prio);
    }
    sbttnav_PostMenus(self, NULL);
}

struct deleterock {
    struct sbutton_prefs *def, *prefs;
};

static boolean dodeletion(b, i, si, rock)
struct sbutton *b;
int i;
struct sbutton_info *si;
struct deleterock *rock;
{
    if(si->prefs==rock->prefs) {
	rock->prefs->refcount--;
	si->prefs=rock->def;
	rock->def->refcount++;
    }
    return FALSE;
}

static void GDeleteProc(self, rock)
struct sbttnav *self;
long rock;
{
    struct sbutton *b=sbttnav_ButtonData(self);
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(b);
    struct groups **g, *h=NULL;
    struct deleterock deleterock;
    
    if(rock<=255) {
	message_DisplayString(self, 50, "Bad group name given.");
	return;
    }
    /* we have an arg assume it is the preferences group to delete. */
    g=LookupGroup(self, (char *)rock);
    if(g==NULL) {
	message_DisplayString(self, 50, "Bad group name given.");
	return;
    }
    h=(*g);
    if(h==NULL || h->prefs==prefs) return;
    deleterock.prefs=h->prefs;
    deleterock.def=prefs;
    sbutton_Enumerate(b, dodeletion, &deleterock);
    *g=h->next;
    sbutton_FreePrefs(h->prefs);
    menulist_ClearChain(h->ml);
    menulist_Destroy(h->ml);
    free(h);
    sbutton_SetModified(b);
    sbutton_SetChangeFlag(b, sbutton_SIZECHANGED|sbutton_ALLCHANGED);
    sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
    sbttnav_PostMenus(self, NULL);
}

static void ShadowColorProc(self, param)
struct sbttnav *self;
long param;
{
    /*
      This is the routine which asks the user for  new sbutton colors.
	  */

    char buf1[MAXPATHLEN], buf2[MAXPATHLEN];
    struct sbutton *b = sbttnav_ButtonData(self);
    char *oldcolor;
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(b);

    if(param>255) {
	/* we have an arg assume it is the preferences group to modify. */
	prefs=LookupGroupPrefs(self, (char *)param);
	if(prefs==NULL) {
	    message_DisplayString(self, 50, "Bad group name given.");
	    return;
	}
    }
    oldcolor = sbutton_GetTopShadow(prefs);    
    
    if (message_AskForString(self,50,"Enter new top shadow color for button: ", NULL, buf1, sizeof(buf1)) >= 0) {
	char buf3[1024];
	if(buf1[0]=='\0') {
	    sbutton_GetTopShadow(prefs)=NULL;
	    message_DisplayString(self, 10, "Restored button top shadow color to the default.");
	}
	else {
	    sbutton_GetTopShadow(prefs) = Intern(buf1);
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    if(sbutton_GetTopShadow(prefs)!=NULL) {
		sprintf(buf3, "Changed button top shadow color to %s.", buf1);
	    } else {
		sbutton_GetTopShadow(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button top shadow color.");
	    }
	    message_DisplayString(self, 10, buf3);
	}
    } else return;

    oldcolor = sbutton_GetBottomShadow(prefs);

    if (message_AskForString(self,50,"Enter new bottom shadow color for button: ", NULL, buf2, sizeof(buf2)) >= 0) {
	char buf3[1024];
	if(buf2[0]=='\0') {
	    sbutton_GetBottomShadow(prefs) = NULL;
	    message_DisplayString(self, 10, "Restored button bottom shadow color to the default.");
	}
	else {
	    sbutton_GetBottomShadow(prefs) = Intern(buf2);
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    if(sbutton_GetBottomShadow(prefs)!=NULL) {
		sprintf(buf3, "Changed button bottom shadow color to %s.", buf2);
	    } else {
		sbutton_GetBottomShadow(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button bottom shadow color.");
	    }
	    message_DisplayString(self, 10, buf3);
	}
    } else return;
    
    oldcolor = sbutton_GetTop(prefs);

    if (message_AskForString(self,50,"Enter new top color for button: ", oldcolor?oldcolor:"White", buf2, sizeof(buf2)) >= 0) {
	char buf3[1024];
	if(buf2[0]=='\0') {
	    sbutton_GetTop(prefs)=NULL;
	    message_DisplayString(self, 10, "Restored button top color to the default.");
	}
	else {
	    sbutton_GetTop(prefs)=Intern(buf2);
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    if(sbutton_GetTop(prefs)!=NULL) {
		sprintf(buf3, "Changed button top color to %s.", buf2);
	    } else {
		sbutton_GetTop(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button top color.");
	    }
	    message_DisplayString(self, 10, buf3);
	}
    } else return;
    
}


static void LabelColorProc(self, param)
struct sbttnav *self;
long param;
{
    /*
      This is the routine which asks the user for  new sbutton colors.
	  */

    char buf1[MAXPATHLEN], buf2[MAXPATHLEN];
    struct sbutton *b = sbttnav_ButtonData(self);
    char *oldcolor;
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(b);

    if(param>255) {
	/* we have an arg assume it is the preferences group to modify. */
	prefs=LookupGroupPrefs(self, (char *)param);
	if(prefs==NULL) {
	    message_DisplayString(self, 50, "Bad group name given.");
	    return;
	}
    }
    oldcolor = sbutton_GetLabelFG(prefs);
    
    if (message_AskForString(self,50,"Enter new foreground color for button label: ", oldcolor?oldcolor:"black", buf1, sizeof(buf1)) >= 0) {
	char buf3[1024];
	if(buf1[0]=='\0') {
	    sbutton_GetLabelFG(prefs)=NULL;
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    message_DisplayString(self, 10, "Restored button label foreground color to the default.");
	} else {
	    sbutton_GetLabelFG(prefs)=Intern(buf1);
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    if(sbutton_GetLabelFG(prefs)!=NULL) {
		sprintf(buf3, "Changed button label foreground color to %s.", buf1);
	    } else {
		sbutton_GetLabelFG(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button label foreground color.");
	    }
	    message_DisplayString(self, 10, buf3);
	}
    } else return;

    oldcolor = sbutton_GetLabelBG(prefs);

    if (message_AskForString(self,50,"Enter new background color for button label: ", oldcolor?oldcolor:"white", buf2, sizeof(buf2)) >= 0) {
	char buf3[1024];
	if(buf2[0]=='\0') {
	    sbutton_GetLabelBG(prefs)=NULL;
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    message_DisplayString(self, 10, "Restored button label background color to the default.");
	} else {
	    sbutton_GetLabelBG(prefs)=Intern(buf2);
	    sbutton_SetModified(b);
	    sbutton_SetChangeFlag(b, sbutton_ALLCHANGED);
	      sbutton_NotifyObservers(b, observable_OBJECTCHANGED);
	    if(sbutton_GetLabelBG(prefs)!=NULL) {
		sprintf(buf3, "Changed button label background color to %s.", buf2);
	    } else {
		sbutton_GetLabelBG(prefs)=oldcolor;
		sprintf(buf3, "Failed to change button label background color.");
	    }
	    message_DisplayString(self, 10, buf3);
	}
    } else return;
}
