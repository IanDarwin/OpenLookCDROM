#ifndef _Xp_h_
#define _Xp_h_
#include <X11/Xp/COPY>

/*
* SCCS_data: @(#) Xp.h	1.3 92/10/28 07:53:57
*
*	This module contains declarations useful to clients of the
*	Xp library.
*******************************************************************************
*/

#include <X11/Wc/WcCreate.h>	/* for _() macro */

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

/* These are identical: multiple names for backward compatibility
*/
void XpRegisterAthena	_(( XtAppContext ));
void XpRegisterAll	_(( XtAppContext ));
void AriRegisterAthena	_(( XtAppContext ));

/* These are identical: multiple names for backward compatibility
*/
Widget XpCreateSimpleMenu _(( Widget, String, Arg*, Cardinal ));
Widget WcCreateSimpleMenu _(( Widget, String, Arg*, Cardinal ));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _Xp_h_ */
