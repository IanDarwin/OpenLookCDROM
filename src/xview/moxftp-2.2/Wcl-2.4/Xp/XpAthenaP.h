#ifndef _XpAthenaP_h_
#define _XpAthenaP_h_
#include <X11/Xp/COPY>

/* SCCS_data: @(#) XpAthenaP.h	1.3 92/10/28 07:53:58
*/

/* Core, Object, RectObj, WindowObj, 
** Shell, OverrideShell, WMShell, VendorShell, TopLevelShell, ApplicationShell, 
** Constraint.
*/
#include <X11/IntrinsicP.h>

#if defined(XtSpecificationRelease) && XtSpecificationRelease > 4
#define R5
#endif

/* include all the *P.h files in heirarchical order */

#include <X11/CoreP.h>
#include <X11/ObjectP.h>
#include <X11/CompositeP.h>
#include <X11/ConstrainP.h>

#if defined(XAW3D)
/* Core */
#include <X11/Xaw3d/SimpleP.h>
#include <X11/Xaw3d/LogoP.h>

/* Core with lotsa system dependencies: using these P.h files requires a
** very well configured Imake configuration due to the many OS dependencies,
** and - hey - we really only do this for kicks anyway!
*/ 
#include <X11/Xaw3d/Clock.h>
#include <X11/Xaw3d/Mailbox.h>

/* Simple */
#include <X11/Xaw3d/GripP.h>
#include <X11/Xaw3d/LabelP.h>
#include <X11/Xaw3d/ListP.h>
#include <X11/Xaw3d/ScrollbarP.h>
#include <X11/Xaw3d/StripCharP.h>
#include <X11/Xaw3d/TextP.h>
#ifdef R5
#include <X11/Xaw3d/MailboxP.h>
#include <X11/Xaw3d/PannerP.h>
#endif /*R5*/

/* Label */
#include <X11/Xaw3d/CommandP.h>
#include <X11/Xaw3d/MenuButtoP.h>
#include <X11/Xaw3d/ToggleP.h>

/* Command */
#ifdef R5
#include <X11/Xaw3d/RepeaterP.h>
#endif

/* Sme */
#include <X11/Xaw3d/SmeP.h>
#include <X11/Xaw3d/SimpleMenP.h>
#include <X11/Xaw3d/SmeBSBP.h>
#include <X11/Xaw3d/SmeLineP.h>


/* Text */
#include <X11/Xaw3d/AsciiTextP.h>
#include <X11/Xaw3d/TextSrcP.h>
#include <X11/Xaw3d/AsciiSrcP.h>
#include <X11/Xaw3d/TextSinkP.h>
#include <X11/Xaw3d/AsciiSinkP.h>

/* Composite and Constraint */
#include <X11/Xaw3d/BoxP.h>
#include <X11/Xaw3d/FormP.h>
#include <X11/Xaw3d/PanedP.h>
#ifdef R5
#include <X11/Xaw3d/PortholeP.h>
#include <X11/Xaw3d/TreeP.h>
#endif /*R5*/
#include <X11/Xp/TableP.h>

/* Form */
#include <X11/Xaw3d/DialogP.h>
#include <X11/Xaw3d/ViewportP.h>

#else /* XAW3D */

/* Core */
#include <X11/Xaw/SimpleP.h>
#include <X11/Xaw/LogoP.h>

/* Core with lotsa system dependencies: using these P.h files requires a
** very well configured Imake configuration due to the many OS dependencies,
** and - hey - we really only do this for kicks anyway!
*/ 
#include <X11/Xaw/Clock.h>
#include <X11/Xaw/Mailbox.h>

/* Simple */
#include <X11/Xaw/GripP.h>
#include <X11/Xaw/LabelP.h>
#include <X11/Xaw/ListP.h>
#include <X11/Xaw/ScrollbarP.h>
#include <X11/Xaw/StripCharP.h>
#include <X11/Xaw/TextP.h>
#ifdef R5
#include <X11/Xaw/MailboxP.h>
#include <X11/Xaw/PannerP.h>
#endif /*R5*/

/* Label */
#include <X11/Xaw/CommandP.h>
#include <X11/Xaw/MenuButtoP.h>
#include <X11/Xaw/ToggleP.h>

/* Command */
#ifdef R5
#include <X11/Xaw/RepeaterP.h>
#endif

/* Sme */
#include <X11/Xaw/SmeP.h>
#include <X11/Xaw/SimpleMenP.h>
#include <X11/Xaw/SmeBSBP.h>
#include <X11/Xaw/SmeLineP.h>


/* Text */
#include <X11/Xaw/AsciiTextP.h>
#include <X11/Xaw/TextSrcP.h>
#include <X11/Xaw/AsciiSrcP.h>
#include <X11/Xaw/TextSinkP.h>
#include <X11/Xaw/AsciiSinkP.h>

/* Composite and Constraint */
#include <X11/Xaw/BoxP.h>
#include <X11/Xaw/FormP.h>
#include <X11/Xaw/PanedP.h>
#ifdef R5
#include <X11/Xaw/PortholeP.h>
#include <X11/Xaw/TreeP.h>
#endif /*R5*/
#include <X11/Xp/TableP.h>

/* Form */
#include <X11/Xaw/DialogP.h>
#include <X11/Xaw/ViewportP.h>

#undef R5
#endif /* XAW3D */

#endif /* _XpAthenaP_h_ */
