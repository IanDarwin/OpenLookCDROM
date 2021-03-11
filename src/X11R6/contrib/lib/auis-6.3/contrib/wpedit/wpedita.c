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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/wpedit/RCS/wpedita.c,v 1.22 1992/12/15 21:57:11 rr2b R6tape $";
#endif

#include <class.h>
#include <pwd.h>
#include <stdio.h>
#include <errprntf.h>
#include <mail.h>
#include <dropoff.h>
#include <svcconf.h>
#include <sys/param.h>
#include <string.h>

#include <atom.ih>
#include <chlist.ih>
#include <chlistv.ih>
#if 0				/* exit handler code */
#include <exit.ih>
#endif /* 0 */
#include <fontdesc.ih>
#include <frame.ih>
#include <im.ih>
#include <lpair.ih>
#include <pshbttn.ih>
#include <pshbttnv.ih>
#include <scroll.ih>
#include <style.ih>
#include <text.ih>
#include <textv.ih>
#include <view.ih>

#include <wpedita.eh>

#ifndef _STD_C
#define remove(x) unlink(x)
#endif

struct wpedit_fieldrec {
  char *fieldindex;
  struct wpeditapp *self;
};

extern struct passwd *getvpwuid();
static struct frame *myframe;
static struct pushbutton *commitpushbutton;
static struct style *title_style, *field_style, *value_style;
static char *progname = "WPEdit";

/* TO DO:  
  1.  Create an Exit-hook function to try to save changes on quitting.
*/


static char *GetWPEntry(entry, key)
WPI_entry_t entry;
char *key;
{
  char *result;

  return((result = WPI_Value(key, entry))?result:"");
}

/* The following should return zero on success, anything else on error.   It should actually be
      validating the entries in the hope that the daemon will later accept them this way */

static char *
quote_wp_chg_field(s)
char *s;
{				/* Quotes output for passwd.chg format. */
  char *q, *r;

  if ((r=(char *)malloc(2*(s?strlen(s)+1:3)))==NULL)
    return(NULL);
  q=r;
  if ((s==NULL)||(*s == '\0')) {
    strcpy(r,"+ ");
  } else {
    for(; *s; ++s) {
      switch(*s) {
      case ':' : *r++ = '+'; *r++ = '='; break;
      case '+' : *r++ = '+'; *r++ = '+'; break;
      default: *r++ = *s;
      }
    }
    *r = '\0';
  }
  return(q);
}

/* The following should return zero or greater on success, the number of changes sent off to the daemon.  If the changes could not be mailed to the daemon, a negative number should be returned. */

static int CommitChanges(self)
struct wpeditapp *self;
{				/* Format the mail into a file suitable 
				   for mailing to wpid,
				   returns the name of the file */
  int i, ct = 0;
  char filename[MAXPATHLEN];
  FILE *fd;
  long t;
  static char *WPAdministrators[2];

  CheckAMSConfiguration();
  if ((WPAdministrators[0] = CheckAMSWPIAddr(WPI_GetWorkingDomain()))==NULL)
    /* Don't have an address to address request to. */
    return(-4);
  WPAdministrators[1] = NULL;

  sprintf(filename,"/tmp/wpi.%d",getpid());
  if ((fd = fopen(filename, "w"))==NULL)
    return(-3);			/* Couldn't open file for mailing. */

  time(&t);
  fprintf(fd,"Date: %s",arpadate());
  fprintf(fd,"From: %s+@%s\n",self->requestor,self->reqdomain);
  fprintf(fd,"To: %s",WPAdministrators[0]);
  for(i=1; WPAdministrators[i];++i) {
    fprintf(fd, ", %s", WPAdministrators[i]);
  }
  fprintf(fd,"\n");

  fprintf(fd,"Subject:  Request for WP Update from '%s'. (from %s v%d)\n\n",
	  self->requestor, progname, WPI_DS_VERSION);
  fprintf(fd,"version:%d\n", WPI_DS_VERSION);
  fprintf(fd,"cell:%s\n", WPI_GetWorkingDomain());
  for(i=0; self->entry[i].fieldnum != -1; ++i)
    if (self->entry[i].changed) {
      fprintf(fd,"change:%s:%s:*:%s:%ld\n",
	      self->EditUser,
	      self->entry[i].fieldname,
	      quote_wp_chg_field(self->entry[i].value),
	      t);
      ++ct;
    }

  if(fclose(fd))
    return(-1);

  if ((ct > 0) 
      && (dropoff(WPAdministrators, filename, NULL,NULL,NULL) > D_LOCALQ))
    return(-2);

  pushbutton_SetText(commitpushbutton, "No unsaved changes have been made.");
  for(i=0; self->entry[i].fieldnum != -1; ++i)
      self->entry[i].changed = false;
  remove(filename);
  return(ct);
}


static int AlterSomething(frec, ch, action, nclicks)
struct wpedit_fieldrec *frec;
struct chlist *ch;
enum view_MouseAction action;
long nclicks;
{
    char Buf[2500], Buf2[2500], Buf3[2500], *wpentry;
    char *vmsg;
    char *fieldindex;
    struct wpeditapp *self;
    struct text *desc;

    fieldindex = frec->fieldindex;
    self = frec->self;
    desc = self->desc;
    if (action == view_LeftUp || action == view_RightUp) {
        sprintf(Buf, "Description of field ``%s'':\n%s\n\nExample: ``%s''.",
		WPI_Nice(fieldindex), WPI_Description(fieldindex),
		WPI_Example(fieldindex));

        text_AlwaysDeleteCharacters(desc, 0L, text_GetLength(desc));
	text_AlwaysInsertCharacters(desc, 0L, Buf, strlen(Buf));
	text_NotifyObservers(desc, 0L);

	if (!(self->adminflag) 
	    && ((WPI_CanIChange(fieldindex)!=ALLOW_MODIFY)
		|| strcmp(self->EditUser,self->requestor))) {
	    message_DisplayString(NULL, 10, "Sorry; this value may not be modified.");
	    return 0;
	}
	sprintf(Buf2, "Correct value for %s: ", WPI_Nice(fieldindex));
	wpentry = GetWPEntry(self->entry, fieldindex);
	if (message_AskForString(NULL, 25, Buf2, wpentry, Buf, sizeof(Buf)) < 0) {
	    message_DisplayString(NULL, 10, "Cancelled.");
	    return 0;
	}

	strcpy(WPI_error_msg, "No diagnostic.");
	WPI_error_code = 0;
	switch(WPI_Validate(fieldindex, Buf, self->entry)) {
	case cool:
	  vmsg = WPI_error_code ? WPI_error_msg : "Noted a change to your White Pages entry.";
	  break;
	case drag:
	  vmsg = WPI_error_code ? WPI_error_msg : "Requested change will require administrative approval.";
	  break;
	case uncool:
	default:
	  message_DisplayString(NULL, 10, WPI_error_code ? WPI_error_msg : "Sorry; could not validate white pages entry!");
	  return 0;
	}

	pushbutton_SetText(commitpushbutton, "Click here to save changes.");

	sprintf(Buf2, "%s: %s", WPI_Nice(fieldindex), wpentry);
	sprintf(Buf3, "%s: %s", WPI_Nice(fieldindex), GetWPEntry(self->entry,fieldindex));
	if (!chlist_ChangeItem(ch, Buf2, Buf3)) {
	    message_DisplayString(NULL, 10, "White pages change noted, but display could not be updated.");
	} else {
	    message_DisplayString(NULL, 10, vmsg);
	}
    }
    return 0;
}

static void CommitPushbuttonHit(self, b, rock)
struct wpeditapp *self;
struct pushbutton *b;
long rock;
{
  int ans = CommitChanges(self);
  if (ans < 0) {
    message_DisplayString(NULL, 10, "Could not save changes.");
  } else if (ans == 0) {
    message_DisplayString(NULL, 10, "There was nothing to change.");
  } else {
    char Buf[2500];
    sprintf(Buf, "Mailed changes for %d field%s to the White Pages daemon.", ans, (ans==1)?"":"s");
    message_DisplayString(NULL, 10, Buf);
  }
}

static boolean SetupList(self)
struct wpeditapp *self;
{
    int i;
    long pos;
    char *fieldindex, *fieldname, *fieldvalue;
    char Buf[2500];
    char *welcome = "Welcome to WPEdit\n",
         *authors = "by M. McInerny and N. Borenstein.\n\n",
         *hint = "An ATK interface to the White Pages Interactive\n\nPlease select a field to change from the set of User-modifiable Fields.",
         *admin_text = "Administratively-modifiable Fields",
         *user_text = "User-modifiable Fields";
    struct chlist *ach, *uch, *wch;
    struct text *desc;
    struct wpedit_fieldrec *frec;

    ach = self->admin;
    uch = self->user;
    desc = self->desc;
    
    if (!chlist_AddItemToEnd(ach, admin_text, NULL, NULL)) return(FALSE);
    if (!chlist_AddItemToEnd(uch, user_text, NULL, NULL)) return(FALSE);

    chlist_AlwaysAddStyle(ach, 0L, strlen(admin_text), title_style);
    chlist_AlwaysAddStyle(uch, 0L, strlen(user_text), title_style);

    strcpy(Buf, welcome);
    strcat(Buf, authors);
    strcat(Buf, hint);
    text_AlwaysReplaceCharacters(desc, 0L, text_GetLength(desc), Buf, strlen(Buf));
    text_AlwaysAddStyle(desc, 0L, strlen(welcome)+strlen(authors), title_style);
    text_AlwaysAddStyle(desc, strlen(welcome), strlen(authors), field_style);
    text_NotifyObservers(desc, 0L);

    for (i=0; self->entry[i].fieldnum != -1; ++i) {
        fieldindex = self->entry[i].fieldname;
	fieldname = WPI_Nice(fieldindex);
	fieldvalue = GetWPEntry(self->entry, fieldindex);

	wch = (WPI_CanIChange(fieldindex)==ALLOW_MODIFY)?uch:ach; 
	pos = chlist_GetLength(wch);

	frec = (struct wpedit_fieldrec *)malloc(sizeof(*frec));
	if (!frec) return(FALSE);
	frec->fieldindex = fieldindex;
	frec->self = self;

	sprintf(Buf, "%s: %s", fieldname, fieldvalue);
	if (!chlist_AddItemToEnd(wch, Buf, AlterSomething, frec)) {
	    return(FALSE);
	}
	chlist_AlwaysAddStyle(wch, pos, strlen(fieldname), field_style);
	chlist_AlwaysAddStyle(wch, pos+strlen(fieldname)+2, strlen(fieldvalue), value_style);
    }

    sprintf(Buf, "Editing White Pages entry %s@%s", self->EditUser,WPI_GetWorkingDomain());
    frame_SetTitle(myframe, Buf);
    pushbutton_SetText(commitpushbutton, "No unsaved changes have been made.");
    return(TRUE);
}

static void ChangePushbuttonHit(self, b, rock)
struct wpeditapp *self;
struct pushbutton *b;
long rock;
{
    WPI_entry_t newentry;
    char Buf[2500], Buf2[2500];
    int i, ct = 0;

    if (message_AskForString(NULL, 25, "Edit white pages entry for what user: ", self->EditUser, Buf, sizeof(Buf)) < 0) {
      return;
    }
    
    if (!(newentry = WPI_Lookup(Buf, true))) {
      sprintf(Buf2, "Couldn't find entry for user ``%s''.\n(WPI error %d ``%s''.)", Buf, WPI_error_code, WPI_error_msg);
      message_DisplayString(NULL, 90, Buf2);
    } else {
      for (i=0; self->entry[i].fieldnum != -1; ++i) {
	if (self->entry[i].changed) ++ct;
      }
      if (ct > 0) {
	int result;
	static char *choices[3] = {"Yes, save changes", "No, ignore changes", NULL};
	if (message_MultipleChoiceQuestion(NULL, 25, "Save WP changes before changing user?", 0, &result, choices, NULL) < 0) return;
	if (result == 0) {
	  CommitPushbuttonHit(self, b, rock);
	}
      }

      while (self->user->numitems > 0) {
	chlist_DeleteItem(self->user, self->user->ItemList[0].str);
      }
      while (self->admin->numitems > 0) {
	chlist_DeleteItem(self->admin, self->admin->ItemList[0].str);
      }

      strcpy(self->EditUser, Buf);
      self->entry = newentry;
      if (SetupList(self)) {
	sprintf(Buf, "Now editing white pages entry for user %s.", self->EditUser);
	message_DisplayString(NULL, 10, Buf);
      } /* if (SetupList...) */
    } /* if (!(newentry...)) */
}

boolean wpeditapp__Start(self)
struct wpeditapp *self;
{
    struct chlist *ach, *uch;
    struct chlistview *achv, *uchv;
    struct text *desc;
    struct textview *descv;
    struct im *myim;
    struct scroll *s1, *s2, *s3;
    struct pushbutton *b, *b2;
    struct pushbuttonview *bv, *bv2;
    struct lpair *lp, *lp2, *lp3, *lp4;

    if (! super_Start(self)) return(FALSE);
    ach = chlist_New();
    achv = chlistview_New();
    uch = chlist_New();
    uchv = chlistview_New();
    desc = text_New();
    descv = textview_New();
    s1 = scroll_Create(achv, scroll_LEFT);
    s2 = scroll_Create(uchv, scroll_LEFT);
    s3 = scroll_Create(descv, scroll_LEFT);
    myframe = frame_New(); 
    myim = im_Create(NULL);
    b = pushbutton_New();
    commitpushbutton = b;
    bv = pushbuttonview_New();
    b2 = pushbutton_New();
    bv2 = pushbuttonview_New();
    lp = lpair_New();
    lp2 = lpair_New();
    lp3 = lpair_New();
    lp4 = lpair_New();
    if(!myim) {
	fprintf(stderr,"wpedit: Could not create new window; exiting.\n");
	return(FALSE);
    }
    if (!ach || !achv || !uch || !uchv || !desc || !descv || !myframe || !s1 || !s2 || !s3 || !b || !bv || !lp || !lp2 || !lp3 || !lp4 || !b || !b2) {
	fprintf(stderr,"wpedit: Could not allocate enough memory; exiting.\n");
	return(FALSE);
    }
    self->admin = ach;
    self->user = uch;
    self->desc = desc;
    text_SetReadOnly(desc, TRUE);
    chlistview_SetDataObject(achv, ach);
    chlistview_SetDataObject(uchv, uch);
    textview_SetDataObject(descv, desc);
    pushbuttonview_SetDataObject(bv, b);
    pushbuttonview_AddRecipient(bv, atom_Intern("buttonpushed"), self, CommitPushbuttonHit, 0L);
    pushbuttonview_SetDataObject(bv2, b2);
    pushbutton_SetText(b2, "Switch User");
    pushbuttonview_AddRecipient(bv2, atom_Intern("buttonpushed"), self, ChangePushbuttonHit, 0L);
    lpair_SetUp(lp2, bv, bv2, 35, lpair_PERCENTAGE, lpair_VERTICAL, FALSE);
    lpair_SetUp(lp4, s3, s1, 50, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(lp3, lp4, s2, 33, lpair_PERCENTAGE, lpair_HORIZONTAL, TRUE);
    lpair_SetUp(lp, lp3, lp2, 25, lpair_BOTTOMFIXED, lpair_HORIZONTAL, FALSE);
    frame_SetView(myframe, lp);
    im_SetView(myim, myframe);
    frame_PostDefaultHandler(myframe, "message", frame_WantHandler(myframe, "message"));
    chlistview_WantInputFocus(uchv, uchv);

    if (!(self->entry = WPI_Lookup(self->EditUser, true))) {
      fprintf(stderr,"WPI Error: %s (%d).\n", WPI_error_msg, WPI_error_code);
      fflush(stderr);
      return(FALSE);
    }
#if 0				/* exit handler code */
    (void)exit_AddExitApprover(OnExit, self); /* won't need to remove handler */
#endif
    return(SetupList(self));
}
 
boolean wpeditapp__InitializeClass(c)
struct classheader *c;
{
    if (!(title_style = style_New())) return(FALSE);
    style_AddNewFontFace(title_style, fontdesc_Bold);
    style_SetJustification(title_style, style_Centered);

    if (!(field_style = style_New())) return(FALSE);
    style_SetFontSize(field_style, style_PreviousFontSize,-2);
    style_AddNewFontFace(field_style, fontdesc_Italic);

    if (!(value_style = style_New())) return(FALSE);
    style_AddNewFontFace(value_style, fontdesc_Plain);

    return(TRUE);
}

boolean wpeditapp__InitializeObject(c, self)
struct classheader *c;
struct wpeditapp *self;  
{
    char *p;

    wpeditapp_SetMajorVersion(self, 2);
    wpeditapp_SetMinorVersion(self, 3);

    self->user = self->admin = NULL;
    self->desc = NULL;
    self->adminflag = FALSE;
    self->entry = NULL;

    CheckServiceConfiguration();
    strcpy(self->reqdomain, ThisDomain);
    WPI_SetWorkingDomain(ThisDomain);

    p = WPI_Self();
    if (!p) {
      fprintf(stderr,"WPI Error: %s (%d).\n", WPI_error_msg, WPI_error_code);
      fflush(stderr);
      return(FALSE);
    }
    strcpy(self->EditUser, p);
    strcpy(self->requestor, p);

    return(TRUE);
}

void wpeditapp__FinalizeObject(c, self)
struct classheader *c;
struct wpeditapp *self;
{
    return; /* Not really necessary, but class complains at its absence */
}

boolean wpeditapp__ParseArgs(self, argc, argv)  
struct wpeditapp *self;
int argc;
char **argv;
{
    super_ParseArgs(self, argc, argv);
    ++argv;
    while (argv[0]) {
	if (argv[0][0] == '-') {
	  switch (argv[0][1]) {
	  case 'A':
	    self->adminflag = TRUE;
	    break;
	  case 'c':
	    if (argv[1]) {
	      WPI_SetWorkingDomain(argv[1]);
	      ++argv;
	    } else {
	      fprintf(stderr, "Must supply a cellname after '-c' switch.\n");
	      return(FALSE);
	    }
	    break;
	  case 'h':
	    fprintf(stderr, "Usage:  wpedit [-A] [-c cell] [username].\n");
	    break;
	  default:
	    fprintf(stderr, "Unrecognized switch: %s.\n", argv[0]);
	    return(FALSE);
	  }
	} else {
	  strcpy(self->EditUser, argv[0]);
	}
	++argv;
    }

    return(TRUE);
}

#if 0				/* exit handler code */
static boolean OnExit(self, status)
struct wpeditapp *self;
int status;
{
  if (status==exit_Normal) {
    int i, ct = 0;

    for (i=0; self->entry[i].fieldnum != -1; ++i) {
      if (self->entry[i].changed) ++ct;
    }
    if (ct > 0) {
      int result;
      static char *choices[] = 
	{"Yes, save changes", "No, ignore changes", "Don't exit", NULL};

      if (message_MultipleChoiceQuestion(NULL, 90, 
					 "Save WP changes before exiting?", 
					 0, &result, choices, NULL) < 0)
	return(FALSE);

      switch(result) {
      case 0:
	CommitPushbuttonHit(self, NULL, 0);
	return(TRUE);
      case 1:
	return(TRUE);
      case 2:
      dafault:
	return(FALSE);
      } /* switch(result) */
    } /* if (ct > 0) */
  } /* if (status...) */
  return(TRUE);
}
#endif
