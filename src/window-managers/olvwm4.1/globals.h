#ifndef _OLWM_GLOBALS_H
#define _OLWM_GLOBALS_H
/*
 *      (c) Copyright 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)globals.h	1.6 olvwm version 07 Jan 1994"
#endif

/*
 * Based on
#ident	"@(#)globals.h	26.45	93/06/28 SMI"
 *
 */

#include "list.h"

typedef struct {
	unsigned int	modmask;
	KeyCode		keycode;
} KeySpec;

typedef enum { BeepAlways, BeepNever, BeepNotices } BeepStatus;

typedef enum { KbdSunView, KbdBasic, KbdFull } MouselessMode;

typedef enum { GridNone, GridInvisible, GridVisible } VirtualGridType;

typedef enum { UseNone, UseVDM, UseAll } ImageType;

typedef enum { SortYounger, SortAlpha, SortYoungerAll, SortAlphaAll } SortType;

typedef struct _globalResourceVariables {
	char		*WindowColor;
	char		*ForegroundColor;
	char		*BackgroundColor;
	char		*BorderColor;
	WorkspaceStyle	WorkspaceStyle;
	char            *WorkspaceColor;
	char            *WorkspaceBitmapFile;
	char            *WorkspaceBitmapFg;
	char            *WorkspaceBitmapBg;
	Bool		ReverseVideo;
	Bool		PaintWorkspace;
	Bool		PointerWorkspace;
	Bool		F3dUsed;
	Bool		F3dFrames;
	Bool		F3dResize;
#ifdef OW_I18N_L4
	XFontSetInfo    TitleFontSetInfo;
	XFontSetInfo    TextFontSetInfo;
	XFontSetInfo    ButtonFontSetInfo;
	XFontSetInfo    IconFontSetInfo;
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
	Bool            BoldFontEmulation;
	Bool            RaiseOnMove;
	Bool            RaiseOnResize;
	Bool            StartDSDM;
	int             WindowCacheSize;
	Bool            MenuAccelerators;
	Bool            WindowMenuAccelerators;
#ifdef OW_I18N_L3
	OLLCItem	LC[OLLC_LC_MAX];
	char		*CharacterSet;
#endif /* OW_I18N_L3 */
/* Following are three entries are strictly for debugging purposes and 
 * are not mentioned in the usage message or doc.
 * Orphaned events are events that are associated with a window or frame 
 * has no entry in the frame hash table, or events that are not handled by the
 * various event handlers.
 * 'PrintAll' is useful for when verification of an events existance is needed.
 */
	Bool		PrintOrphans;
	Bool		PrintAll;
	Bool		Synchronize;
	Bool		PrintWarnings;
/*
 * Following are entries for the Virtual Desktop.
 */
	char		*VirtualDesktop;
	int		VDMScale;
	Bool		AllowMoveIntoDesktop;
	Bool		ArrowInRoot;
	char		*VirtualGeometry;
	char		*VirtualFontName;
	char		*VirtualBackgroundMap;
	char		*VirtualBackgroundColor;
	int		MaxMapColors;
	char		*VirtualPixmapColor;
	char		*VirtualIconGeometry;
	char		*VirtualForegroundColor;
	char		*VirtualFontColor;
	Bool		VirtualIconic;
	List		*StickyList;
	Bool		UseRelativePosition;
	Bool		GrabVirtualKeys;
	VirtualGridType	VirtualGrid;
	char		*VirtualGridColor;
	Bool		VirtualRaiseVDM;
	Bool		StickyIcons;
	Bool		StickyIconScreen;
	Bool		VirtualMoveGroups;
	Bool		VirtualReRead;
	Bool		AllowSyntheticEvents;
	Bool		SyntheticEvents;
	List		*NoVirtualKey;
	List		*NoVirtualLKey;
	List		*NoVirtualFKey;
	List		*NoVirtualRKey;
	Bool		VirtualDrawSticky;
	Bool		ParentScreenPopup;
/*
 * Following are entries added with olvwm but which aren't strictly
 * dependent on the virtual nature of olvwm
 */
	Bool		AutoShowRootMenu;
	int		AutoRootMenuX;
	int		AutoRootMenuY;
	char		*InputFocusColor;
	Bool		FullSizeZoomX;
	List		*NoDecors;
	char		*ResizePosition;
	ImageType	UseImageMenu;
	SortType	VirtualMenuSort;
	SortType	VirtualDirSort;
	Bool		FreeIconSlots;
	int		IconGridHeight;
	int		IconGridWidth;
	Bool		UniqueIconSlots;

     /* Special resize cursors */
	char            *SpecialResizePointerData;
	Bool            SpecialResizePointers;
	Cursor          CornerPointers[4];
	Cursor          ResizePointers[4];
     /* Other Cursors */
	Cursor          CloseUpPointer;
	Cursor          CloseDownPointer;
	char            *BasicPointerData;
	char            *MovePointerData;
	char            *BusyPointerData;
	char            *IconPointerData;
	char            *ResizePointerData;
	char            *MenuPointerData;
	char            *QuestionPointerData;
	char            *TargetPointerData;
	char            *PanPointerData;
	char            *CloseUpPointerData;
	char            *CloseDownPointerData;
} GlobalResourceVariables;

extern GlobalResourceVariables	GRV;

#ifdef OW_I18N_L3
 
/* shortcuts for getting at locale category items */
#define lc_basic                LC[OLLC_LC_BASIC_LOCALE]
#define lc_dlang                LC[OLLC_LC_DISPLAY_LANG]
#define lc_ilang                LC[OLLC_LC_INPUT_LANG]
#define lc_numeric              LC[OLLC_LC_NUMERIC]
#define lc_datefmt              LC[OLLC_LC_DATE_FORMAT]

#endif

#endif	/* _OLWM_GLOBALS_H */
