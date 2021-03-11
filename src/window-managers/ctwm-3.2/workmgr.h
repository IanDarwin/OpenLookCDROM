/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */
#ifndef _WORKMGR_
#define _WORKMGR_

#define MAXWORKSPACE 32
#define MAPSTATE      0
#define BUTTONSSTATE  1

void CreateWorkSpaceManager ();
void GotoWorkSpaceByName ();
void GotoPrevWorkSpace ();
void GotoNextWorkSpace ();
void GotoWorkSpace ();
void AddWorkSpace ();
void SetupOccupation ();
void Occupy ();
void OccupyHandleButtonEvent ();
void OccupyAll ();
void AllocateOthersIconManagers ();
void ChangeOccupation ();
void WmgrRedoOccupation ();
void WMgrRemoveFromCurrentWosksace ();
void WMgrAddToCurrentWosksaceAndWarp ();
void WMgrHandleExposeEvent ();
void PaintWorkSpaceManager ();
void PaintOccupyWindow ();
void AddToClientsList ();
void WMapToggleState ();
void WMapSetMapState ();
void WMapSetButtonsState ();
void WMapAddWindow ();
void WMapDestroyWindow ();
void WMapMapWindow ();
void WMapSetupWindow ();
void WMapIconify ();
void WMapDeIconify ();
void WMapRaiseLower ();
void WMapLower ();
void WMapRaise ();
void WMapRestack ();
void WMapUpdateIconName ();
void WMgrHandleKeyReleaseEvent ();
void WMgrHandleKeyPressEvent ();
void WMgrHandleButtonEvent ();
void WMapRedrawName ();
void WMapCreateCurrentBackGround ();
void WMapCreateDefaultBackGround ();

typedef struct WorkSpaceList WorkSpaceList;

typedef struct winList {
    Window		w;
    int			x, y;
    int			width, height;
    TwmWindow		*twm_win;
    ColorPair		cp;
    MyFont		font;
    struct winList	*next;
} *WinList;

typedef struct mapSubwindow {
    Window		w;
    Window		blanket;
    int			x, y;
    WinList		wl;
} MapSubwindow;

typedef struct WorkSpaceWindow {
    Window		w;
    TwmWindow		*twm_win;
    char		*geometry;
    int			x, y;
    char		*name;
    char		*icon_name;
    int			state;
    int			lines, columns;
    int			noshowoccupyall;

    int			width, height;
    int			bwidth, bheight;
    int			hspace, vspace;
    ColorPair		cp;
    MyFont		buttonFont;

    int			wwidth, wheight;
    name_list		*windowBackgroundL;
    name_list		*windowForegroundL;
    ColorPair		windowcp;
    MyFont		windowFont;

    ColorPair		curColors;
    Image		*curImage;
    unsigned long	curBorderColor;

    ColorPair		defColors;
    Image		*defImage;
    unsigned long	defBorderColor;
} WorkSpaceWindow;

typedef struct OccupyWindow {
    Window		w;
    TwmWindow		*twm_win;
    char		*geometry;
    Window		OK, cancel, allworkspc;
    int			x, y;
    int			width, height;
    char		*name;
    char		*icon_name;
    int			lines, columns;
    int			hspace, vspace;
    int			bwidth, bheight;
    int			owidth, oheight;
    ColorPair		cp;
    MyFont		font;
    int			tmpOccupation;
} OccupyWindow;

struct WorkSpaceList {
    Window		buttonw;
    Window		obuttonw;
    int			number;
    char		*name;
    char		*label;
    ColorPair		cp;
    IconMgr		*iconmgr;
    ColorPair		backcp;
    Image		*image;
    name_list		*clientlist;
    MapSubwindow	mapSubwindow;
    struct WorkSpaceList *next;
};

typedef struct WorkSpaceMgr {
    WorkSpaceList	*workSpaceList;
    WorkSpaceList	*activeWSPC;
    WorkSpaceWindow	workspaceWindow;
    OccupyWindow	occupyWindow;
    int			visibility;
    int			count;
} WorkSpaceMgr;


#endif
