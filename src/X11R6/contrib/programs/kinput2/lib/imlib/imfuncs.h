/* $Id: imfuncs.h,v 1.8 1994/05/17 10:54:02 ishisone Exp $ */
/*
 * Copyright (c) 1994  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 */

#ifndef _imfuncs_h
#define _imfuncs_h

#include "imprtype.h"

/*
 * Functions to get/put data (defined in imdata.c)
 */
extern int IMGetC8 _Pt_((IMConnection *conn, int offset));
extern unsigned int IMGetC16 _Pt_((IMConnection *conn, int offset));
extern int IMGetI16 _Pt_((IMConnection *conn, int offset));
extern unsigned long IMGetC32 _Pt_((IMConnection *conn, int offset));
extern void IMGetString _Pt_((IMConnection *conn,
			       int offset, char *buf, int len));

extern void IMPutC8 _Pt_((IMConnection *conn, int x));
extern void IMPutC16 _Pt_((IMConnection *conn, unsigned int x));
extern void IMPutC32 _Pt_((IMConnection *conn, unsigned long x));
extern void IMPutI16 _Pt_((IMConnection *conn, int x));
extern void IMPutString _Pt_((IMConnection *conn, char *s, int len));
extern void IMPutPad _Pt_((IMConnection *conn));
extern void IMRewriteC16 _Pt_((IMConnection *conn, int pos, unsigned int x));

extern int IMWritePos _Pt_((IMConnection *conn));

extern int IMPutHeader _Pt_((IMConnection *conn,
			     int major, int minor, int arglen));
extern void IMFinishRequest _Pt_((IMConnection *conn, int offset));
extern void IMCancelRequest _Pt_((IMConnection *conn, int offset));
extern void IMSendSimpleRequest _Pt_((IMConnection *conn,
				      int major, int minor));
extern void IMSendRequestWithIC _Pt_((IMConnection *conn,
				      int major, int minor, IMIC *icp));

extern void IMSendError _Pt_((IMConnection *conn, int code,
			      unsigned int imid, unsigned int icid,
			      char *msg));
extern void IMSendBadProtocol _Pt_((IMConnection *conn, char *msg));
extern void IMSendBadLength _Pt_((IMConnection *conn,
				  unsigned int imid, unsigned int icid));

/*
 * Transport layer functions (defined in imxport.c)
 */
extern int IMCreateTCPService _Pt_((int *portp));
extern IMConnection *IMTCPConnection _Pt_((Widget protocol, int socket));
extern int IMCreateUnixService _Pt_((char *path));
extern IMConnection *IMUnixConnection _Pt_((Widget protocol, int socket));
extern IMConnection *IMXConnection _Pt_((Widget protocol, XEvent *ev));
extern int IMFlush _Pt_((IMConnection *conn));
extern void IMShutdown _Pt_((IMConnection *conn));
extern void IMCloseConnection _Pt_((IMConnection *conn));

/*
 * Request dispatch functions (defined in imdispatch.c)
 */
extern void IMSetInitialDispatcher _Pt_((IMConnection *conn));
extern void IMDispatch _Pt_((IMConnection *conn, int cond));
extern void IMSchedule _Pt_((IMConnection *conn, int type));
extern void IMProcessQueue _Pt_((Widget w));

/*
 * Function to compile request dispatch table (defined in imrequest.c)
 */
extern void IMCompileReq _Pt_((void));

/*
 * Functions handling IM/IC creation/destruction/lookup (defined in imic.c)
 */
extern IMIM *IMGetIM _Pt_((IMConnection *conn, int arglen));
extern IMIC *IMGetIC _Pt_((IMConnection *conn, int arglen));
extern IMIM *IMCreateIM _Pt_((IMConnection *conn, IMConverter *converter));
extern IMIC *IMCreateIC _Pt_((IMIM *imp));
extern void IMDestroyIM _Pt_((IMIM *imp));
extern void IMDestroyIC _Pt_((IMIC *imp));

/*
 * Functions handling conversion start/stop/etc. (defined in imconv.c)
 */
extern int IMStartConversion _Pt_((IMIC *icp));
extern void IMStopConversion _Pt_((IMIC *icp));
extern int IMResetIC _Pt_((IMIC *icp, char **preedit_strp));
extern void IMForwardEvent _Pt_((IMIC *icp, XEvent *ev));
extern void IMSetFocus _Pt_((IMIC *icp));
extern void IMUnsetFocus _Pt_((IMIC *icp));

/*
 * Functions dealing with IM/IC attributes (defined in imattr.c)
 */
extern void IMPutIMAttrList _Pt_((IMIM *imp));
extern void IMPutICAttrList _Pt_((IMIM *imp));
extern int IMSetIMValues _Pt_((IMIM *imp, char *data, int len, int major));
extern int IMGetIMValues _Pt_((IMIM *imp, char *data, int len, int offset));
extern int IMSetICValues _Pt_((IMIC *icp, char *data, int len, int major));
extern int IMGetICValues _Pt_((IMIC *icp, char *data, int len, int offset));
extern void IMFillDefault _Pt_((IMIC *icp,
				unsigned long common_mask,
				unsigned long preedit_mask,
				unsigned long status_mask));
extern int IMValidateWindow _Pt_((Display *dpy, Window win,
				  IMWindowProfile *profilep));
extern int IMValidateICAttributes _Pt_((IMIC *icp, int checkonly));
extern void IMFreeICAttributes _Pt_((IMIC *icp));
extern unsigned long IMMakeConvAttributes _Pt_((IMIC *icp,
						ConversionAttributes *attr));
extern void IMMoveLocation _Pt_((IMIC *icp, int x, int y));

/*
 * Functions interfacing imlib and IMProtocol widget (defined in improto.c)
 */
extern void IMRegisterConnection _Pt_((IMConnection *conn));
extern void IMUnregisterConnection _Pt_((IMConnection *conn));
extern IMConnection *IMConnectionList _Pt_((Widget w));
extern void IMPushQueue _Pt_((IMConnection *conn));
extern IMConnection *IMPopQueue _Pt_((Widget w));
extern int IMQueueEmpty _Pt_((Widget w));
extern Pixel IMDefaultForeground _Pt_((Widget w));
extern Pixel IMDefaultBackground _Pt_((Widget w));
extern char *IMDefaultFontSet _Pt_((IMIM *imp));
extern FontBank IMFontBank _Pt_((IMIM *imp));
extern int IMStatusWidth _Pt_((Widget w));
extern void IMInitHash _Pt_((Widget w));
extern IMIM **IMIMHash _Pt_((Widget w));
extern IMIC **IMICHash _Pt_((Widget w));
extern unsigned int IMNextIMID _Pt_((Widget w));
extern unsigned int IMNextICID _Pt_((Widget w));
extern Atom IMCtextAtom _Pt_((Widget w));
extern Atom IMKi2CommAtom _Pt_((Widget w));
extern Atom IMXConnectAtom _Pt_((Widget w));
extern Atom IMProtocolAtom _Pt_((Widget w));
extern Atom IMMoreDataAtom _Pt_((Widget w));
extern IMTriggerKey *IMTriggerKeys _Pt_((IMIM *imp, int *num_triggersp));
extern IMConverter *IMGetConverter _Pt_((Widget w, char *locale));

#endif /* _imfuncs_h */
