#ifndef _Xmp_h_
#define _Xmp_h_
#include <X11/Xmp/COPY>

/*
* SCCS_data: @(#) Xmp.h 1.7 92/10/28 07:50:37
*
*	This module contains declarations useful to clients of the
*	Xmp library.
*
*******************************************************************************
*/

#include <X11/Wc/WcCreate.h>	/* for _() macro */

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/* XmpRegisterMotif registers all Motif and Xmp widgets.
 * XmpRegisterAll and MriRegisterMotif are aliases for XmpRegisterMotif
 * for backward compatibility.
 */
void XmpRegisterMotif _(( XtAppContext ));
void XmpRegisterAll   _(( XtAppContext ));
void MriRegisterMotif _(( XtAppContext ));

void XmpAddMwmCloseCallback _(( Widget, XtCallbackProc, XtPointer ));

void XmpChangeNavigationType _(( Widget ));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _Xmp_h_ */
