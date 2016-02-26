#ifndef _MriMotifP_h_
#define _MriMotifP_h_
#include <X11/Xmp/COPY>

/*
* SCCS_data: @(#) XmpMotifP.h	1.3 92/10/28 07:50:39
*
*	This module includes all of the private headers for all Motif
*	widgets and Xmp widgets (public widgets based on Motif).  The
*	files get included in the appropriate order (top down in the
*	widget class heirarchy).
*
*******************************************************************************
*/

/* Core, Object, RectObj, WindowObj, 
** XmGadget, XmPrimitive, and XmComposite, 
** Shell, OverrideShell, WMShell, VendorShell, TopLevelShell, ApplicationShell, 
** Constraint, XmManager.
*/
#include <Xm/XmP.h>

/* XmGadget Subclasses
*/
#include <Xm/ArrowBGP.h>
#include <Xm/SeparatoGP.h>
#include <Xm/LabelGP.h>
#include <Xm/CascadeBGP.h>
#include <Xm/PushBGP.h>
#include <Xm/ToggleBGP.h>

/* XmPrimitive Subclasses
*/
#include <Xm/ArrowBP.h>
#include <Xm/ListP.h>
#include <Xm/ScrollBarP.h>
#include <Xm/SeparatorP.h>
#include <Xm/TextP.h>

/* TextField: Motif 1.0 doesn't have them, the TextFP.h collides with TextP.h,
 * and they are useless anyway.  Nevertheless, at least get TextF.h if there.
 */
#ifndef _OLD_MOTIF
#ifdef FIXED_TextFP_h
#include <Xm/TextFP.h>
#else
#include <Xm/TextF.h>
#endif
#endif

#include <Xm/LabelP.h>
#include <Xm/CascadeBP.h>
#include <Xm/DrawnBP.h>
#include <Xm/PushBP.h>
#include <Xm/ToggleBP.h>

/* XmManager Subclasses
*/
#include <Xm/DrawingAP.h>
#include <Xm/FrameP.h>
#include <Xm/PanedWP.h>
#include <Xm/RowColumnP.h>
#include <Xm/ScaleP.h>
#include <Xm/ScrolledWP.h>
#include <Xm/MainWP.h>
#include <Xm/BulletinBP.h>
#include <Xm/FormP.h>
#include <Xm/MessageBP.h>
#include <Xm/SelectioBP.h>
#include <Xm/CommandP.h>
#ifndef XtSpecificationRelease
#include <Xm/FileSB.h>
#endif
#include <Xm/FileSBP.h>

/* Shell Subclasses
*/
#include <ShellP.h>
#include <VendorP.h>
#include <Xm/MenuShellP.h>
#include <Xm/DialogSP.h>

/* Apparently Obsolete 
*/
#include <Xm/SashP.h>

/* Public widgets derived from Motif
*/
#include <X11/Xmp/TableP.h>

#endif /* _MriMotifP_h_ */
