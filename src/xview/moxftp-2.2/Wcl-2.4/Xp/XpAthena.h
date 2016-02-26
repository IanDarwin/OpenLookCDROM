#ifndef _XpAthena_h_
#define _XpAthena_h_
#include <X11/Xp/COPY>

/* SCCS_data: @(#) XpAthena.h	1.3 92/10/28 07:53:57
*/

/* Core, Object, RectObj, WindowObj, 
** Shell, OverrideShell, WMShell, VendorShell, TopLevelShell, ApplicationShell, 
** Constraint.
*/
#include <X11/Intrinsic.h>

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
#define R5
#endif

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 5 
#define R6
#endif



/* include all the *.h files in heirarchical order */

#if defined(XAW3D)

#include <X11/Xaw3d/Simple.h>
#include <X11/Vendor.h>

#ifndef R6
/* Core */
#include <X11/Xaw3d/Clock.h>
#include <X11/Xaw3d/Logo.h>
#include <X11/Xaw3d/Mailbox.h>
#endif

/* Simple */
#include <X11/Xaw3d/Grip.h>
#include <X11/Xaw3d/Label.h>
#include <X11/Xaw3d/List.h>
#include <X11/Xaw3d/Scrollbar.h>
#include <X11/Xaw3d/StripChart.h>
#include <X11/Xaw3d/Text.h>
#ifdef R5
#include <X11/Xaw3d/Mailbox.h>
#include <X11/Xaw3d/Panner.h>
#endif /*R5*/

/* Label */
#include <X11/Xaw3d/Command.h>
#include <X11/Xaw3d/MenuButton.h>
#include <X11/Xaw3d/Toggle.h>

/* Command */
#ifdef R5
#include <X11/Xaw3d/Repeater.h>
#endif

/* Sme */
#include <X11/Xaw3d/Sme.h>
#include <X11/Xaw3d/SimpleMenu.h>
#include <X11/Xaw3d/SmeBSB.h>
#include <X11/Xaw3d/SmeLine.h>


/* Text */
#include <X11/Xaw3d/AsciiText.h>
#include <X11/Xaw3d/TextSrc.h>
#include <X11/Xaw3d/AsciiSrc.h>
#include <X11/Xaw3d/TextSink.h>
#include <X11/Xaw3d/AsciiSink.h>

/* Composite and Constraint */
#include <X11/Xaw3d/Box.h>
#include <X11/Xaw3d/Form.h>
#include <X11/Xaw3d/Paned.h>
#ifdef R5
#include <X11/Xaw3d/Porthole.h>
#include <X11/Xaw3d/Tree.h>
#endif /*R5*/
#include <X11/Xp/Table.h>

/* Form */
#include <X11/Xaw3d/Dialog.h>
#include <X11/Xaw3d/Viewport.h>
#else /* XAW3D */

#include <X11/Xaw/Simple.h>
#include <X11/Vendor.h>

#ifndef R6
/* Core */
#include <X11/Xaw/Clock.h>
#include <X11/Xaw/Logo.h>
#include <X11/Xaw/Mailbox.h>
#endif

/* Simple */
#include <X11/Xaw/Grip.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Text.h>
#ifdef R5
#ifndef R6
#include <X11/Xaw/Mailbox.h>
#endif /* !R6 */
#include <X11/Xaw/Panner.h>
#endif /*R5*/

/* Label */
#include <X11/Xaw/Command.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Toggle.h>

/* Command */
#ifdef R5
#include <X11/Xaw/Repeater.h>
#endif

/* Sme */
#include <X11/Xaw/Sme.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>


/* Text */
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/TextSrc.h>
#include <X11/Xaw/AsciiSrc.h>
#include <X11/Xaw/TextSink.h>
#include <X11/Xaw/AsciiSink.h>

/* Composite and Constraint */
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Paned.h>
#ifdef R5
#include <X11/Xaw/Porthole.h>
#include <X11/Xaw/Tree.h>
#endif /*R5*/
#include <X11/Xp/Table.h>

/* Form */
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Viewport.h>

#undef R5
#endif /* XAW3D */

#endif /* _XpAthena_h_ */
