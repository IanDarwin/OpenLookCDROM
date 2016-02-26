#ifndef _XmpP_h_
#define _XmpP_h_
#include <X11/Xmp/COPY>

/*
* SCCS_data: @(#) XmpP.h	1.5 92/10/28 07:50:40
*
*	This module contains declarations private to the implementation
*	of the Xmp library.
*
*******************************************************************************
*/

#include <X11/Wc/WcCreateP.h>
#include <X11/Xmp/Xmp.h>

#ifdef __cplusplus	/* for C++ V2.0 */
extern "C" {
#endif

void XmpCvtStringToXmString	( CONVERTER(NULL) );
void XmpCvtStringToMenuWidget	( CONVERTER(wcWidgetCvtArgs) );

void XmpPopupACT ( ACTION( menu ) );
void XmpFixTranslationsACT ( ACTION( text ) );
void XmpFixTranslationsCB ( CALLBACK( text ) );
void XmpAddMwmCloseCallbackACT ( ACTION( shell_cbList ) );
void XmpAddMwmCloseCallbackCB ( CALLBACK( shell_cbList ) );
void XmpAddTabGroupCB ( CALLBACK( tabGroupWidgetNameOpt ) );
void XmpAddTabGroupACT ( ACTION( tabGroupWidgetNameOpt ) );
void XmpTableChildConfigCB ( CALLBACK( child_col_row_hSpan_vSpan ) );
void XmpTableChildConfigACT ( ACTION( child_col_row_hSpan_vSpan ) );
void XmpTableChildPositionCB( CALLBACK( child_col_row ) );
void XmpTableChildPositionACT ( ACTION( child_col_row ) );
void XmpTableChildResizeCB( CALLBACK( child_hSpan_vSpan ) );
void XmpTableChildResizeACT ( ACTION( child_hSpan_vSpan ) );
void XmpTableChildOptionsCB( CALLBACK( child_opts ) );
void XmpTableChildOptionsACT ( ACTION( child_opts ) );

#ifdef __cplusplus	/* for C++ V2.0 */
}
#endif

#endif /* _XmpP_h_ */
