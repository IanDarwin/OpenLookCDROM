/* $XimpImplementGroup: XimInt.c, v 1.1 94/05/31 21:16:02 $ */
/* $XConsortium: imInt.c,v 1.3 94/03/26 17:00:26 rws Exp $ */
/******************************************************************

           Copyright 1992, 1993, 1994 by FUJITSU LIMITED

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of FUJITSU LIMITED
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.
FUJITSU LIMITED makes no representations about the suitability of
this software for any purpose. 
It is provided "as is" without express or implied warranty.

FUJITSU LIMITED DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
EVENT SHALL FUJITSU LIMITED BE LIABLE FOR ANY SPECIAL, INDIRECT OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF
USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR
OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

  Author: Takashi Fujiwara     FUJITSU LIMITED 
                               fujiwara@a80.tech.yk.fujitsu.co.jp

******************************************************************/
/*

Copyright (c) 1992 - 1994  FUJITSU LIMITED

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE FUJITSU LIMITED BE LIABLE FOR ANY CLAIM, DAMAGES
OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the FUJITSU LIMITED.

  Author : Takashi Fujiwara     FUJITSU LIMITED 

*/

#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xmd.h>
#include "Xlibint.h"
#include "Xlcint.h"
#include "Ximint.h"
#include "XimImSw.h"

Private Xim 		*_XimCurrentIMlist  = (Xim *)NULL;
Private int		 _XimCurrentIMcount = 0;

Private Bool
_XimSetIMStructureList(im)
    Xim		  im;
{
    register int  i;
    Xim		 *xim;

    if(!(_XimCurrentIMlist)) {
	if(!(_XimCurrentIMlist = (Xim *)Xmalloc(sizeof(Xim))))
	    return False;
	_XimCurrentIMlist[0] = im;
	_XimCurrentIMcount   = 1;
    }
    else {
	for(i = 0; i < _XimCurrentIMcount; i++) {
	    if(!( _XimCurrentIMlist[i])) {
		_XimCurrentIMlist[i] = im;
		break;
	    }
	}
	if(i >= _XimCurrentIMcount) {
	    if(!(xim = (Xim *)Xrealloc(_XimCurrentIMlist, 
					 ((i + 1) * sizeof(Xim)))))
		return False;
	    _XimCurrentIMlist			  = xim;
	    _XimCurrentIMlist[_XimCurrentIMcount] = im;
	    _XimCurrentIMcount++;
	}
    }
    return True;
}

Public void
_XimDestroyIMStructureList(im)
    Xim		  im;
{
    register int  i;

    for(i = 0; i < _XimCurrentIMcount; i++) {
	if(_XimCurrentIMlist[i] == im) {
	    _XimCurrentIMlist[i] = NULL;
	    break;
	}
    }
    return;
}

Public void
_XimServerDestroy()
{
    register int  i;
    Xim		  im;
    XIC		  ic;

    for(i = 0; i < _XimCurrentIMcount; i++) {
	if(!(im = _XimCurrentIMlist[i]))
	    continue;

	if (im->core.destroy_callback.callback)
	    (*im->core.destroy_callback.callback)(im,
			im->core.destroy_callback.client_data, NULL);
	for (ic = im->core.ic_chain; ic; ic = ic->core.next) {
	    if (ic->core.destroy_callback.callback) {
		(*ic->core.destroy_callback.callback)(ic,
			ic->core.destroy_callback.client_data, NULL);
	    }
	}
	_XimResetIMInstantiateCallback( im );
	(void)im->methods->close((XIM)im);
	Xfree(im);
    }
    Xfree(_XimCurrentIMlist);
    _XimCurrentIMlist  = (Xim *)NULL;
    return;
}

#ifdef XIM_CONNECTABLE
Public void
_XimServerReconectableDestroy()
{
    register int  i;
    Xim		  im;
    XIC		  ic;

    for(i = 0; i < _XimCurrentIMcount; i++) {
	if(!(im = _XimCurrentIMlist[i]))
	    continue;

	if (im->core.destroy_callback.callback)
	    (*im->core.destroy_callback.callback)(im,
			im->core.destroy_callback.client_data, NULL);
	for (ic = im->core.ic_chain; ic; ic = ic->core.next) {
	    if (ic->core.destroy_callback.callback) {
		(*ic->core.destroy_callback.callback)(ic,
			ic->core.destroy_callback.client_data, NULL);
	    }
	}
	_XimResetIMInstantiateCallback( im );
	(void)im->methods->close((XIM)im);
    }
    return;
}
#endif /* XIM_CONNECTABLE */

Private char	*
_XimStrstr(src, dest)
    register char	*src, *dest;
{
    int			 len;
    
    len = strlen(dest);
    while(src = strchr(src, *dest)) {
	if(!strncmp(src, dest, len))
	    return src;
	src++;
    }
    return NULL;
}

Private char *
_XimMakeImName(lcd)
    XLCd	   lcd;
{
    char	   buf[BUFSIZE];
    register char *mod;
    register int   i = 0;
    char	  *ret;

    buf[0] = '\0';
    if(lcd->core->modifiers != (char *)NULL && *lcd->core->modifiers != '\0') {
	mod = _XimStrstr(lcd->core->modifiers, XIMMODIFIER);
	if(mod) {
	    mod += strlen(XIMMODIFIER);
	    while (*mod && *mod != '@')
		buf[i++] = *mod++;
	    buf[i] = '\0';
	}
    }
    if(!(ret = Xmalloc(i + 1)))
	return NULL;
    (void)strcpy(ret, buf);
    return ret;
}

Public XIM
_XimOpenIM(lcd, dpy, rdb, res_name, res_class)
    XLCd		 lcd;
    Display		*dpy;
    XrmDatabase		 rdb;
    char		*res_name, *res_class;
{
    Xim			 im;
    register int	 i;
    
    if (!(im = (Xim)Xmalloc(sizeof(XimRec))))
	return (XIM)NULL;
    bzero(im, sizeof(XimRec));

    im->core.lcd       = lcd;
    im->core.ic_chain  = (XIC)NULL;
    im->core.display   = dpy;
    im->core.rdb       = rdb;
    im->core.res_name  = NULL;
    im->core.res_class = NULL;
    if((res_name != NULL) && (*res_name != '\0')){
	if(!(im->core.res_name  = (char *)Xmalloc(strlen(res_name)+1)))
	    goto Error1;
	strcpy(im->core.res_name,res_name);
    }
    if((res_class != NULL) && (*res_class != '\0')){
	if(!(im->core.res_class = (char *)Xmalloc(strlen(res_class)+1)))
	    goto Error2;
	strcpy(im->core.res_class,res_class);
    }
    if(!(im->core.im_name = _XimMakeImName(lcd)))
	goto Error3;

    for(i= 0; ; i++) {
	if(_XimImSportRec[i].checkprocessing(im)) {
	    if(!(_XimImSportRec[i].im_open(im)))
		goto Error4;
	    if(!_XimSetIMStructureList(im))
		goto Error4;
	    return (XIM)im;
	}
    }

Error4 :
    _XimImSportRec[i].im_free(im);
    Xfree(im);
    return NULL;
Error3 :
    if(im->core.im_name)
	Xfree(im->core.im_name);
Error2:
    if(im->core.res_class)
	Xfree(im->core.res_class);
Error1:
    if(im->core.res_name)
	Xfree(im->core.res_name);
    Xfree(im);
    return NULL;
}

#ifdef XIM_CONNECTABLE
Public XIM
_XimDelayOpenIM(lcd, dpy, rdb, res_name, res_class)
    XLCd		 lcd;
    Display		*dpy;
    XrmDatabase		 rdb;
    char		*res_name, *res_class;
{
    Xim			 im;
    register int	 i;
    
    if (!(im = (Xim)Xmalloc(sizeof(XimRec))))
	return (XIM)NULL;
    bzero(im, sizeof(XimRec));

    im->core.lcd       = lcd;
    im->core.ic_chain  = (XIC)NULL;
    im->core.display   = dpy;
    im->core.rdb       = rdb;
    im->core.res_name  = NULL;
    im->core.res_class = NULL;
    if((res_name != NULL) && (*res_name != '\0')){
	if(!(im->core.res_name  = (char *)Xmalloc(strlen(res_name)+1)))
	    goto Error1;
	strcpy(im->core.res_name,res_name);
    }
    if((res_class != NULL) && (*res_class != '\0')){
	if(!(im->core.res_class = (char *)Xmalloc(strlen(res_class)+1)))
	    goto Error2;
	strcpy(im->core.res_class,res_class);
    }
    if(!(im->core.im_name = _XimMakeImName(lcd)))
	goto Error3;

    if(!_XimProtoDelayOpenIM(im))
	goto Error4;
    if(!_XimSetIMStructureList(im))
	goto Error4;
    return (XIM)im;

Error4 :
    _XimProtoAllIMFree(im);
    Xfree(im);
    return NULL;
Error3 :
    if(im->core.im_name)
	Xfree(im->core.im_name);
Error2:
    if(im->core.res_class)
	Xfree(im->core.res_class);
Error1:
    if(im->core.res_name)
	Xfree(im->core.res_name);
    Xfree(im);
    return NULL;
}
#endif /* XIM_CONNECTABLE */

Private XIM
_XimSWOpenIM(lcd, dpy, rdb, res_name, res_class)
    XLCd		 lcd;
    Display		*dpy;
    XrmDatabase		 rdb;
    char		*res_name, *res_class;
{
    XIM			 im;
    register int	 i;

    for(i= 0; _XimImSportProtocolRec[i].im_open ; i++) {
	if(im = _XimImSportProtocolRec[i].im_open(lcd,
					dpy, rdb, res_name, res_class)) {
	    return im;
	}
    }
    return (XIM)NULL;
}

Public Bool
_XInitIM(lcd)
    XLCd	 lcd;
{
    if(lcd == (XLCd)NULL)
	return False;
    lcd->methods->open_im = _XimSWOpenIM;
    lcd->methods->register_callback = _XimRegisterIMInstantiateCallback;
    lcd->methods->unregister_callback = _XimUnRegisterIMInstantiateCallback;
    return True;
}
