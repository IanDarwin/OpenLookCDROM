#ifndef _Xop_h_
#define _Xop_h_
#include <X11/Xop/COPY>

/* SCCS_data: @(#) Xop.h	1.3 92/10/28 08:02:19
*/

#include <X11/Wc/WcCreate.h>	/* for _() macro */

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

void XopRegisterOpenLook _(( XtAppContext ));
void XopRegisterAll	 _(( XtAppContext ));
void OriRegisterOpenLook _(( XtAppContext ));

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif
