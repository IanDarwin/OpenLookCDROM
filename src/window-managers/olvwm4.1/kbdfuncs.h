/*
 *      (c) Copyright 1989, 1990 Sun Microsystems, Inc. Sun design patents
 *      pending in the U.S. and foreign countries. See LEGAL_NOTICE
 *      file for terms of the license.
 */

#ifndef _OLWM_KBDFUNCS_H
#define _OLWM_KBDFUNCS_H

#ifdef IDENT
#ident	"@(#)kbdfuncs.h	1.5 olvwm version 07 Jan 1994"
#endif

/*
 * Based on
#ident	"@(#)kbdfuncs.h	1.8	93/06/28 SMI"
 *
 */

extern void KeyBackFocus();
extern void KeyBeep();
extern void KeyFocusToPointer();
extern void KeyRaiseLowerPointer();
extern void KeyFrontFocus();
extern void KeyFullRestore();
extern void KeyLockColormap();
extern void KeyMove();
extern void KeyNextApp();
extern void KeyNextWindow();
extern void KeyOpenClosePointer();
extern void KeyOpenCloseFocus();
extern void KeyOwner();
extern void KeyPrevApp();
extern void KeyPrevWindow();
extern void KeyProperties();
extern void KeyQuit();
extern void KeyRefresh();
extern void KeyResize();
extern void KeyToggleInput();
extern void KeyTogglePin();
extern void KeyUnlockColormap();
extern void KeyWindowMenu();
extern void KeyWorkspaceMenu();
extern void KeyMakeInvisiblePointer();
extern void KeyMakeInvisibleFocus();
extern void KeyMakeVisibleAll();
extern void KeyToggleFullSizeZoomX();  /* Toggle resource FullSizeZoomX. */
extern void KeyToggleDragWindow();     /* Toggle resource DragWindow. */
extern void KeyToggleMoveGroups();     /* Toggle resource VirtualMoveGroups. */
extern void KeyToggleSticky();         /* Toggle Sticky State */
extern void KeySaveWorkspace();

extern void KeyMoveVDM();
extern void AddKeyBinding();

#endif /* _OLWM_KBDFUNCS_H */
