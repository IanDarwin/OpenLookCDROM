#ident	"@(#)globals.h	26.45	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_GLOBALS_H
#define _OLWM_GLOBALS_H

#include "list.h"

typedef struct {
	unsigned int	modmask;
	KeyCode		keycode;
} KeySpec;

typedef enum { BeepAlways, BeepNever, BeepNotices } BeepStatus;

typedef enum { KbdSunView, KbdBasic, KbdFull } MouselessMode;

typedef struct _globalResourceVariables {
	char		*WindowColor;
	char		*ForegroundColor;
	char		*BackgroundColor;
	char		*BorderColor;
	WorkspaceStyle	WorkspaceStyle;
	char		*WorkspaceColor;
	char		*WorkspaceBitmapFile;
	char		*WorkspaceBitmapFg;
	char		*WorkspaceBitmapBg;
	Bool		ReverseVideo;
	Bool		PaintWorkspace;
	Bool		PointerWorkspace;
	Bool		F3dUsed;
	Bool		F3dFrames;
	Bool		F3dResize;
#ifdef OW_I18N_L4
	XFontSetInfo 	TitleFontSetInfo;
	XFontSetInfo	TextFontSetInfo;
	XFontSetInfo 	ButtonFontSetInfo;
	XFontSetInfo	IconFontSetInfo;
#else
	XFontStruct    	*TitleFontInfo;
	XFontStruct	*TextFontInfo;
	XFontStruct    	*ButtonFontInfo;
	XFontStruct	*IconFontInfo;
#endif
	XFontStruct	*GlyphFontInfo;
	Cursor		BasicPointer;
	Cursor		MovePointer;
	Cursor		BusyPointer;
	Cursor		IconPointer;
	Cursor		ResizePointer;
	Cursor		MenuPointer;
	Cursor		QuestionPointer;
	Cursor		TargetPointer;
	Cursor		PanPointer;
	Bool		FocusFollowsMouse;
	Text		*DefaultWinName;
	int		SaveWorkspaceTimeout;
	char		*SaveWorkspaceCmd;
	int		FlashTime;
	Bool		FShowMenuButtons;		/* XXX */
	Bool		FShowPinnedMenuButtons;		/* XXX */
	IconPreference	IconPlacement;
	Bool		FSnapToGrid;
	Bool		FocusLenience;
	Bool		DragWindow;
	Bool		AutoRaise;
	int		AutoRaiseDelay;
	Bool		PopupJumpCursor;
	Bool		ColorLocked;
	Bool		PPositionCompat;
	Bool		RefreshRecursively;
	BeepStatus	Beep;
	int		EdgeThreshold;
	int		DragRightDistance;
	int		MoveThreshold;
	int		ClickMoveThreshold;
	int		DoubleClickTime;
	int		RubberBandThickness;
	KeySpec		FrontKey;
	KeySpec		HelpKey;
	KeySpec		OpenKey;
	KeySpec		ConfirmKey;
	KeySpec		CancelKey;
	KeySpec		ColorLockKey;
	KeySpec		ColorUnlockKey;
	List		*Minimals;
	Bool		MouseChordMenu;
	int		MouseChordTimeout;
	Bool		SingleScreen;
	Bool		AutoReReadMenuFile;
	Bool		KeepTransientsAbove;
	Bool		TransientsSaveUnder;
	Bool		TransientsTitled;
	Bool		SelectWindows;
	Bool		ShowMoveGeometry;
	Bool		ShowResizeGeometry;
	Bool		InvertFocusHighlighting;
	Bool		RunSlaveProcess;
	Bool		SelectToggleStacking;
	int		FlashCount;
	char		*DefaultIconImage;
	char		*DefaultIconMask;
	Bool		ServerGrabs;
	int		IconFlashCount;
	Bool		SelectDisplaysMenu;
	int		SelectionFuzz;
	Bool		AutoInputFocus;
	Bool		AutoColorFocus;
	Bool		ColorTracksInputFocus;
	int		IconFlashOnTime;
	int		IconFlashOffTime;
	MouselessMode	Mouseless;
	Bool		RaiseOnActivate;
	Bool		RestackWhenWithdraw;
	Bool		BoldFontEmulation;
	Bool		RaiseOnMove;
	Bool		RaiseOnResize;
	Bool		StartDSDM;
	int		WindowCacheSize;
	Bool		MenuAccelerators;
	Bool		WindowMenuAccelerators;
#ifdef OW_I18N_L3
	OLLCItem	LC[OLLC_LC_MAX];
	char		*CharacterSet;
#endif
	Bool		PrintOrphans;
	Bool		PrintAll;
	Bool		Synchronize;
	Bool		PrintWarnings;
} GlobalResourceVariables;

extern GlobalResourceVariables	GRV;


#ifdef OW_I18N_L3

/* shortcuts for getting at locale category items */
#define lc_basic		LC[OLLC_LC_BASIC_LOCALE]
#define lc_dlang		LC[OLLC_LC_DISPLAY_LANG]
#define lc_ilang		LC[OLLC_LC_INPUT_LANG]
#define lc_numeric		LC[OLLC_LC_NUMERIC]
#define lc_datefmt		LC[OLLC_LC_DATE_FORMAT]

#endif


#endif /* _OLWM_GLOBALS_H */

