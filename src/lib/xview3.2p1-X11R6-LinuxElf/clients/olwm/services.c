#ident	"@(#)services.c	26.53	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */


#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define XK_MISCELLANY
#include <X11/keysymdef.h>

#include "i18n.h"
#include "ollocale.h"
#include "events.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "notice.h"
#include "globals.h"
#include "group.h"
#include "mem.h"
#include "resources.h"
#include "error.h"
#include "dsdm.h"

#ifdef __linux
#define SYSV
#endif

extern	char		*getenv();
extern	unsigned int	FindModifierMask();
extern	void		ReInitUserMenu();
extern	void		*ClientKill();

/*
 * Externals
 */
extern Atom AtomProtocols;
extern Atom AtomSaveYourself;

extern Window	NoFocusWin;

extern Bool 	UpdInputFocusStyle();

extern Time	LastEventTime;

/*
 * Execute a command by handing it to /bin/sh.
 */
static int
execCommand(winInfo,cmd)
    WinGeneric *winInfo;
    char *cmd;
{
    char *args[4];
    int pid;
    char **env = winInfo->core.client->scrInfo->environment;

    args[0] = "/bin/sh";
    args[1] = "-c";
    args[2] = cmd;
    args[3] = NULL;

    pid = fork();
    if (pid == -1) {
	perror("olwm: fork");
	return 1;
    } else if (pid == 0) {
	/* child */
#ifdef SYSV
	setpgrp();
#else
	setpgrp(0, getpid());
#endif
	execve(args[0], args, env);
	perror("olwm: exec");
	exit(1);
    }
    return 0;
}


/***************************************************************************
* Exit from WM
****************************************************************************/

void
ExitCallback(dpy,button)
	Display		*dpy;
	int		button;
{
	/* If Exit button is selected, will return 0 */
	if (button == 0)
		Exit(dpy);
}

/*
 * ExitFunc - Exit window with a confirmation notice
 */
/*ARGSUSED*/
int
ExitFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	int		screen;
	Text		*buttons[2];
	Text		*msg;
	NoticeBox	noticeBox;

	buttons[0] = GetText("Exit");
	buttons[1] = GetText("Cancel");
	msg = GetText("Please confirm exit from window system");
	screen = winInfo->core.client->screen;

	/* set up noticeBox information */
	noticeBox.numButtons = NOTICE_BUTTON_COUNT(buttons);
	noticeBox.defaultButton = 1;		/* cancel is default */
	noticeBox.buttonText = buttons;
	noticeBox.msgText = msg;
	noticeBox.boxX = -1;
	noticeBox.boxY = -1;

	UseNoticeBoxSync(dpy,screen,&noticeBox,ExitCallback);

	FreeText(buttons[0]);
	FreeText(buttons[1]);
	FreeText(msg);
}

/*
 * ExitNoConfirmFunc - Exit window w/o a confirmation notice
 */
int
/*ARGSUSED*/
ExitNoConfirmFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Exit(dpy);
}

/***************************************************************************
* Command execution
****************************************************************************/

/*
 * AppMenuFunc -- called when a command is listed as the item selected on
 *      the olwm menu
 */
/*ARGSUSED*/
int
AppMenuFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	return execCommand(winInfo,
		menuInfo->menu->buttons[idx]->action.command);
}

/*
 * PshFunc -- called when the "POSTSCRIPT" keyword is present for the
 *      item selected in the olwm menu
 *
 */
/*ARGSUSED*/
int
PshFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	char    *commArgv[2];
	int	pshPipe[2];
	int     pid;
	char	*dir;
	char	pshPath[100];
	char	**env = winInfo->core.client->scrInfo->environment;

	if ( (dir = getenv( "OPENWINHOME" )) == NULL )
		commArgv[0] = "/usr/bin/psh";
	else
	{
		strcpy( pshPath, dir ); 
		strcat( pshPath, "/bin/psh" );
		commArgv[0] = pshPath;
	}

	commArgv[1] = NULL;

	if ( pipe( pshPipe ) == -1 )
	{
		perror( "olwm: pipe" );
		return( -1 );
	}

	pid = fork();
	if ( pid == -1 )
	{
		perror("olwm: fork");
		return( -1 );
	}
	else if ( pid == 0 )
	{
		/* child reads from pipe and writes to stdout/err */
		close( 0 );		/* close stdin */
		dup( pshPipe[0] );	/* make stdin the read end */
		close( pshPipe[0] ); 	/* don't need orig pipe fds */
		close( pshPipe[1] );
		close( 1 );		/* close stdout */
		dup( 2 );		/* make olwm stderr = psh stdout */
#ifdef SYSV
		setpgrp();
#else
		setpgrp(0, getpid());
#endif
		execve( commArgv[0], commArgv, env );
		fprintf( stderr, GetString("olwm: psh error: %d\n"), errno );
	}
	else
	{
		/* parent writes user menu postscript code down pipe */
		close( pshPipe[0] );	/* don't need to read pipe */
		write( pshPipe[1], 
		       (menuInfo->menu->buttons[idx]->action.command),
		       strlen((menuInfo->menu->buttons[idx]->action.command)));
		close( pshPipe[1] );
	}
	return 1;
}

/***************************************************************************
* Flip Drag
****************************************************************************/

/*ARGSUSED*/
int
FlipDragFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	GRV.DragWindow = !GRV.DragWindow;
	return 0;
}


/***************************************************************************
* Flip Focus
****************************************************************************/


/*ARGSUSED*/
int
FlipFocusFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	extern void	UpdFocusStyle();
	Bool		temp = !GRV.FocusFollowsMouse;

	UpdFocusStyle(dpy, NULL, &GRV.FocusFollowsMouse, &temp);
	return 0;
}

/***************************************************************************
* No-Operation
****************************************************************************/

/*
 * NopFunc - a no-operation function, used as a placeholder for
 *      the NOP service
 */
/*ARGSUSED*/
int
NopFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
}

/***************************************************************************
* Clipboard
****************************************************************************/

/*ARGSUSED*/
int
ClipboardFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	NoticeBox	noticeBox;
	Text		*buttons[1];
	Text		*msg;

	buttons[0] = GetText("Ok");
	msg = GetText("Sorry, the clipboard is not yet implemented.");

	/* set up noticeBox information */
	noticeBox.numButtons = NOTICE_BUTTON_COUNT(buttons);
	noticeBox.defaultButton = 0;
	noticeBox.buttonText = buttons;
	noticeBox.msgText = msg;
	noticeBox.boxX = -1;
	noticeBox.boxY = -1;

	(void) UseNoticeBox(dpy, winInfo->core.client->screen, &noticeBox);

	FreeText(buttons[0]);
	FreeText(msg);
}

/***************************************************************************
* Print Screen
****************************************************************************/

/*ARGSUSED*/
int
PrintScreenFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	NoticeBox	noticeBox;
	Text		*buttons[1];
	Text		*msg;

	buttons[0] = GetText("Ok");
	msg = GetText("Sorry, Print Screen is not yet implemented.");

	/* set up noticeBox information */
	noticeBox.numButtons = NOTICE_BUTTON_COUNT(buttons);
	noticeBox.defaultButton = 0;
	noticeBox.buttonText = buttons;
	noticeBox.msgText = msg;
	noticeBox.boxX = -1;
	noticeBox.boxY = -1;

	(void) UseNoticeBox(dpy, winInfo->core.client->screen, &noticeBox);

	FreeText(buttons[0]);
	FreeText(msg);
}


/***************************************************************************
* Refresh screen
****************************************************************************/

/*
 * RecursiveRefresh
 * 
 * Recursively refresh an entire window tree, by walking the hierarchy and 
 * sending Expose events to each window (via XClearWindow).  Note that 
 * XClearArea will generate a BadMatch error if called on InputOnly windows; 
 * this error is suppressed in Error.c.
 */
void
RecursiveRefresh(dpy, win)
	Display *dpy;
	Window win;
{
	int i;
	unsigned int nchildren;
	Status s;
	Window root, parent;
	Window *childlist;

	XClearArea(dpy, win, 0, 0, 0, 0, True);
	s = XQueryTree(dpy, win, &root, &parent, &childlist, &nchildren);
	if (s == 0)
		return;
	for (i=0; i<nchildren; ++i) {
		RecursiveRefresh(dpy, childlist[i]);
	}
	if (nchildren > 0)
	XFree((char *)childlist);
}


/*
 * RefreshFunc -- called when the "Refresh Screen" item has been selected on
 *	the olwm menu
 */
/*ARGSUSED*/
int
RefreshFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	if (GRV.RefreshRecursively) {
		RecursiveRefresh(dpy, winInfo->core.client->scrInfo->rootid);
	} else {
		Window	w;
		XSetWindowAttributes xswa;
		int	screen = winInfo->core.client->screen;

		/* We create a window over the whole screen, map it,
		* then destroy it.
		*/
		xswa.override_redirect = True;
		w = ScreenCreateWindow(winInfo->core.client->scrInfo,
		    WinRootID(winInfo), 0, 0,
		    DisplayWidth(dpy,screen), DisplayHeight(dpy,screen),
		    CWOverrideRedirect, &xswa);

		XMapRaised(dpy, w);
		ScreenDestroyWindow(winInfo->core.client->scrInfo, w);
	}
}

/***************************************************************************
* Properties
****************************************************************************/

#define WORKSPACEPROPS "props"

/*
 * PropertiesFunc -- called when the "Properties ..." item has been selected 
 * on the root menu.  REMIND: this and AppMenuFunc should be merged.
 */
/*ARGSUSED*/
int
PropertiesFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	return execCommand(winInfo,WORKSPACEPROPS);
}

/***************************************************************************
* Save Workspace
****************************************************************************/

/*
 * SaveWorkspaceFunc - called when "Save Workspace" is selected
 * from the root menu.
 */
/*ARGSUSED*/
int
SaveWorkspaceFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Text		*buttons[1];
	Text		*msg;
	int		status;
	int		screen = winInfo->core.client->screen;
	NoticeBox	noticeBox;

	/* having either grab fail isn't fatal; issue warnings only */

	if (XGrabPointer(dpy, NoFocusWin, False, ButtonPressMask,
			 GrabModeAsync, GrabModeAsync, None,
			 GRV.BusyPointer, CurrentTime) != GrabSuccess)
	{
	    ErrorWarning(GetString("failed to grab pointer"));
	}

	if (XGrabKeyboard(dpy, NoFocusWin, False, GrabModeAsync,
			  GrabModeAsync, CurrentTime) != GrabSuccess)
	{
	    ErrorWarning(GetString("failed to grab keyboard"));
	}

	status = system(GRV.SaveWorkspaceCmd);

	XUngrabKeyboard(dpy,CurrentTime);
	XUngrabPointer(dpy,CurrentTime);

	/*
	 * owplaces was sucessful
	 */
	if (status == 0) {
		buttons[0] = GetText("Ok");
		msg = GetText("Save Workspace complete.");

		noticeBox.numButtons = NOTICE_BUTTON_COUNT(buttons);
		noticeBox.defaultButton = 0;
		noticeBox.buttonText = buttons;
		noticeBox.boxX = noticeBox.boxY = -1;
		noticeBox.msgText = msg;

		UseNoticeBox(dpy,screen,&noticeBox);

		FreeText(buttons[0]);
		FreeText(msg);

		return True;
	}

	/*
	 * owplaces failed with an error
	 */
	buttons[0] = GetText("Cancel");

	switch (status>>8) {
	case 4:
		msg = GetText("Save Workspace could not be performed, because\nthere was an error writing the .openwin-init file.");
		break;
	case 5:
		msg = GetText("Save Workspace could not be performed,\nbecause some applications did not respond.");
		break;
	default:
		msg = GetText("Save Workspace could not be performed,\nbecause the owplaces(1) command failed.");
		break;
		
	}

	noticeBox.numButtons = NOTICE_BUTTON_COUNT(buttons);
	noticeBox.defaultButton = 0;
	noticeBox.buttonText = buttons;
	noticeBox.boxX = noticeBox.boxY = -1;
	noticeBox.msgText = msg;

	UseNoticeBox(dpy,screen,&noticeBox);

	FreeText(buttons[0]);
	FreeText(msg);

	return False;
}

/***************************************************************************
* ReReadUserMenu
****************************************************************************/

/* 
 * ReReadUserMenuFunc
 */
/*ARGSUSED*/
int
ReReadUserMenuFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ReInitUserMenu(dpy,True);
}

/***************************************************************************
* Window Menu Action Procs
****************************************************************************/

/* 
 * WindowOpenCloseAction
 *	Toggles Open/Close.
 */
/*ARGSUSED*/
int
WindowOpenCloseAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientOpenCloseToggle(winInfo->core.client,LastEventTime);
}

/* 
 * WindowFullRestoreSizeAction
 *	Toggles Full/Restore Size.
 */
/*ARGSUSED*/
int
WindowFullRestoreSizeAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientFullRestoreSizeToggle(winInfo->core.client,LastEventTime);
}

/*
 * WindowMoveAction
 *	Moves the window with user interaction.
 */
/*ARGSUSED*/
int
WindowMoveAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientMove(winInfo->core.client,(XEvent *)NULL);
}

/*
 * WindowResizeAction
 *	Resizes the window with user interaction.
 */
/*ARGSUSED*/
int
WindowResizeAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientResize(winInfo->core.client, NULL, keyevent, NULL, NULL);
}

/* 
 * WindowPropsAction
 *
 *	This function is stubbed out because there is currently no definition 
 *	of what the WM is supposed to do when the "Props" item is hit.
 */
/*ARGSUSED*/
int
WindowPropsAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	return 0;
}

/* 
 * WindowBackAction
 *	Pushes a window back in the window hierarchy.
 */
/*ARGSUSED*/
int
WindowBackAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientBack(winInfo->core.client);
}

/* 
 * WindowRefreshAction
 *	Refreshes the window
 */
/*ARGSUSED*/
int
WindowRefreshAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientRefresh(winInfo->core.client);
}

/* 
 * WindowQuitAction
 *
 */
/*ARGSUSED*/
int
WindowQuitAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientKill(winInfo->core.client,True);
}

/* 
 * WindowFlashOwnerAction
 *
 */
/*ARGSUSED*/
int
WindowFlashOwnerAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientFlashOwner(winInfo->core.client);
}

/* 
 * WindowThisAction
 *	Dismiss this window.
 */
/*ARGSUSED*/
int
WindowDismissThisAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	ClientKill(winInfo->core.client, False);
}

/*
 * _dismissSiblingMenus - called from ViisitPinnedMenuclients to 
 * dismiss all pinned menus on a particular screen
 */
void
_dismissSiblingMenus(cli, winInfo)
    Client *cli;
    WinGeneric *winInfo;
{
    if (cli->screen == winInfo->core.client->screen)
	ClientKill(cli, False);
}

/* 
 * WindowDismissAllAction
 *	Dismiss all pop-ups in the group.
 */

/*ARGSUSED*/
int
WindowDismissAllAction(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Client	*cli = winInfo->core.client;

	if (cli->framewin && cli->framewin->fcore.panewin &&
	    cli->framewin->fcore.panewin->core.kind == WIN_PINMENU) {
	    VisitPinnedMenuClients(_dismissSiblingMenus, winInfo);
	} else {
	    /* dismiss all followers in this window's group */
	    GroupApply(cli->groupid,ClientKill,(void *)False,GROUP_DEPENDENT);

	    /*
	     * If this window is not a dependent follower, make sure to dismiss
	     * it too.
	     */
	    if (cli->groupmask != GROUP_DEPENDENT)
		ClientKill(winInfo->core.client, False);
	}
}

/***************************************************************************
* Window controls functions
****************************************************************************/

/*
 * Window Control Functions:
 *	Each function operates on the selected client list and
 *	performs the necessary action on each client (if any).
 */

/*
 * OpenCloseSelnFunc
 *	Toggles Open/Close on all selected clients
 */
/*ARGSUSED*/
int
OpenCloseSelnFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Client 	*cli = (Client *)NULL;

	while (cli = EnumSelections(cli)) {
		ClientOpenCloseToggle(cli,LastEventTime);
	}
}

/*
 * FullRestoreSizeSelnFunc
 *	Toggles Full/Restore Size on all selected clients
 */
/*ARGSUSED*/
int
FullRestoreSizeSelnFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Client 	*cli = (Client *)NULL;

	while (cli = EnumSelections(cli)) {
		ClientFullRestoreSizeToggle(cli,LastEventTime);
	}
}

/*
 * BackSelnFunc
 *	Lowers all selected clients/windows to that back of the 
 *	window hierarchy.
 */
/*ARGSUSED*/
int
BackSelnFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Client 	*cli = (Client *)NULL;

	while (cli = EnumSelections(cli)) {
		ClientBack(cli);
	}
}

/*
 * QuitSelnFunc
 *	Quit's all selected clients.
 */
/*ARGSUSED*/
int
QuitSelnFunc(dpy, winInfo, menuInfo, idx)
	Display 	*dpy;
	WinGeneric 	*winInfo;
	MenuInfo    	*menuInfo;
	int     	idx;
{
	Client 	*cli = (Client *)NULL;

	while (cli = EnumSelections(cli)) {
		ClientKill(cli,True);
	}
}


/***************************************************************************
* DSDM functions
****************************************************************************/

/*ARGSUSED*/
int
StartDSDMFunc(dpy, winInfo, menuInfo, idx)
    Display 	*dpy;
    WinGeneric 	*winInfo;
    MenuInfo	*menuInfo;
    int     	idx;
{
    DragDropStartDSDM(dpy);
    return 0;
}

/*ARGSUSED*/
int
StopDSDMFunc(dpy, winInfo, menuInfo, idx)
    Display 	*dpy;
    WinGeneric 	*winInfo;
    MenuInfo	*menuInfo;
    int     	idx;
{
    DragDropStopDSDM(dpy);
    return 0;
}
