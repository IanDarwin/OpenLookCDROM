/* $XimpImplementGroup: XimpICG.c, v 1.1 94/05/31 21:16:05 $ */
/* $XConsortium: XimpICG.c,v 1.10 92/10/19 19:24:35 rws Exp $ */
/******************************************************************

    Copyright 1991, 1992 by FUJITSU LIMITED.
    Copyright 1991, 1992 by Sun Microsystems, Inc.
    Copyright 1991, 1992 by Sony Corporation

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose is hereby granted without fee,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the name of FUJITSU LIMITED, Sun
Microsystems, Inc. and Sony Corporation  not be used in advertising
or publicity pertaining to distribution of the software without
specific, written prior permission.
FUJITSU LIMITED , Sun Microsystems, Inc. and Sony Corporation make no
representations about the suitability of this software for any
purpose.  It is provided "as is" without express or implied warranty.

FUJITSU LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION DISCLAIM
ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL FUJITSU
LIMITED, SUN MICROSYSTEMS, INC. AND SONY CORPORATION BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Author: Takashi Fujiwara     FUJITSU LIMITED
        Hideki Hiura         Sun Microsystems, Inc.
        Makoto Wakamatsu     Sony Corporation

******************************************************************/
/*

Copyright (c) 1991 - 1994  FUJITSU LIMITED
Copyright (c) 1991 - 1994  Sony Corporation

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
IN NO EVENT SHALL THE FUJITSU LIMITED AND SONY CORPORATION BE LIABLE FOR
ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the FUJITSU LIMITED and
Sony Corporation shall not be used in advertising or otherwise to promote
the sale, use or other dealings in this Software without prior written
authorization from the FUJITSU LIMITED and Sony Corporation.

  Author:  Takashi Fujiwara     FUJITSU LIMITED
           Makoto Wakamatsu     Sony Corporation

*/

#define NEED_EVENTS
#include "Xlibint.h"
#include <X11/Xatom.h>

#include "XimpIm.h"

Public XPointer
_Ximp_GetRequestIM(ic, mask, get_atom_id, atom_id)
    Ximp_XIC		 ic;
    unsigned long	 mask;
    Atom		 get_atom_id, atom_id;
{
    XEvent			event;
    Atom			actual_type_ret;
    int				actual_format_ret;
    unsigned long		nitems_ret;
    unsigned long		bytes_after_ret;
    unsigned char		*data;
    XimpCMPredicateArgRec	Arg;

    if(!IS_IC_CONNECTED(ic))
	return(NULL);
    if(!(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN))
	return(NULL);

    _Ximp_IM_SendMessage(ic, XIMP_GETVALUE(ic), mask, NULL, NULL);
    Arg.type = ((Ximp_XIM)ic->core.im)->ximp_impart->improtocol_id;
    Arg.owner = ((Ximp_XIM)ic->core.im)->ximp_impart->fe_window;
    Arg.protocol = XIMP_GETVALUE_RETURN(ic);
    Arg.icid = ic->ximp_icpart->icid;
    if( !_XimpIfEvent( ic, &event, _Ximp_CMPredicate32, (XPointer)&Arg ) )
	return( NULL );

    XGetWindowProperty(ic->core.im->core.display,
		       ic->core.client_window,
		       get_atom_id, 0L, 1000000L, True, atom_id,
		       &actual_type_ret, &actual_format_ret, &nitems_ret,
		       &bytes_after_ret, &data);

    if(actual_format_ret == 0 || nitems_ret == 0)
	return(NULL);
    return((XPointer)data);
}

Private Bool
_Ximp_PreGetAttributes(ic, vl, return_name)
    Ximp_XIC		 ic;
    XIMArg		*vl;
    char		**return_name;
{
    XIMArg			*p;
    XRectangle			*p_rect;
    XPoint			*p_point;
    unsigned long		 mask;
    int				 im_preedit_flag = 0;
    Ximp_PreeditPropRec4	*preedit_data = NULL;
    Ximp_PreeditPropRec3	*preedit_data_bc = NULL;
    XIMCallback 		*p_callback;

    if(IS_IC_CONNECTED(ic)){
	for(mask = 0, p = vl; p->name != NULL; p++) {
	    if(strcmp(p->name, XNArea)==0)
		mask |= XIMP_PRE_AREA_MASK(ic);
	    else if(strcmp(p->name, XNAreaNeeded)==0)
		mask |= XIMP_PRE_AREANEED_MASK(ic);
	    else if(strcmp(p->name, XNSpotLocation)==0) {
		if(!(IS_BEING_PREEDITED(ic) && _XimpIsNeedMoveProtoMode(ic)))
		    _Ximp_IM_SendMessage(ic, XIMP_MOVE(ic),
			    ic->ximp_icpart->preedit_attr.SpotLocation.x,
			    ic->ximp_icpart->preedit_attr.SpotLocation.y,
			    NULL);
		mask |= XIMP_PRE_SPOTL_MASK(ic);
	    }
	    else if(strcmp(p->name, XNColormap)==0)
		mask |= XIMP_PRE_COLORMAP_MASK(ic);
	    else if(strcmp(p->name, XNStdColormap)==0)
		mask |= XIMP_PRE_STD_COLORMAP_MASK(ic);
	    else if(strcmp(p->name, XNBackground)==0)
		mask |= XIMP_PRE_BG_MASK(ic);
	    else if(strcmp(p->name, XNForeground)==0)
		mask |= XIMP_PRE_FG_MASK(ic);
	    else if(strcmp(p->name, XNBackgroundPixmap)==0)
		mask |= XIMP_PRE_BGPIXMAP_MASK(ic);
	    else if(strcmp(p->name, XNLineSpace)==0)
		mask |= XIMP_PRE_LINESP_MASK(ic);
	    else if(strcmp(p->name, XNCursor)==0)
		mask |= XIMP_PRE_CURSOR_MASK(ic);
	}
	if(mask) {
	    preedit_data = (Ximp_PreeditPropRec4 *)_Ximp_GetRequestIM(ic,
			mask,
			((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id,
			((Ximp_XIM)ic->core.im)->ximp_impart->preedit_atr_id);
	    if(preedit_data != (Ximp_PreeditPropRec4 *)NULL) {
		im_preedit_flag = 1;
		if( !ISXimp4(ic) ) {
		    preedit_data_bc = (Ximp_PreeditPropRec3 *)preedit_data;
		    if( (preedit_data = (Ximp_PreeditPropRec4 *)Xmalloc(sizeof(Ximp_PreeditPropRec4))) == NULL ) {
			im_preedit_flag = 0;
		    }
		    else {
			preedit_data->Area = preedit_data_bc->Area;
			preedit_data->AreaNeeded = preedit_data_bc->AreaNeeded;
			preedit_data->SpotLocation = preedit_data_bc->SpotLocation;
			preedit_data->Colormap = preedit_data_bc->Colormap;
			preedit_data->StdColormap = preedit_data_bc->Colormap;
			preedit_data->Foreground = preedit_data_bc->Foreground;
			preedit_data->Background = preedit_data_bc->Background;
			preedit_data->Bg_Pixmap = preedit_data_bc->Bg_Pixmap;
			preedit_data->LineSpacing = preedit_data_bc->LineSpacing;
			preedit_data->Cursor = preedit_data_bc->Cursor;
		    }
		    XFree( preedit_data_bc );
		}
	    }
	}
    }

    for(p = vl; p->name != NULL; p++) {
	if(strcmp(p->name, XNArea)==0) {
	    if(im_preedit_flag) {
		if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_rect->x       = preedit_data->Area.x;
		p_rect->y       = preedit_data->Area.y;
		p_rect->width   = preedit_data->Area.width;
		p_rect->height  = preedit_data->Area.height;
	    } else {
		if(XIMP_CHK_PREAREAMASK(ic)) {
		    if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
			*return_name = p->name;
			return(False);
		    }
		    p_rect->x       = ic->core.preedit_attr.area.x;
		    p_rect->y       = ic->core.preedit_attr.area.y;
		    p_rect->width   = ic->core.preedit_attr.area.width;
		    p_rect->height  = ic->core.preedit_attr.area.height;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	    *((XRectangle **)(p->value)) = p_rect;
	} else if(strcmp(p->name, XNAreaNeeded)==0) {
	    if(im_preedit_flag) {
		if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_rect->x  = p_rect->y  = 0;
		p_rect->width   = preedit_data->AreaNeeded.width;
		p_rect->height  = preedit_data->AreaNeeded.height;
	    } else {
		if(XIMP_CHK_PREAREANEEDMASK(ic)) {
		    if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
			*return_name = p->name;
			return(False);
		    }
		    p_rect->x  = p_rect->y  = 0;
		    p_rect->width   = ic->core.preedit_attr.area_needed.width;
		    p_rect->height  = ic->core.preedit_attr.area_needed.height;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	    *((XRectangle **)(p->value)) = p_rect;
	} else if(strcmp(p->name, XNSpotLocation)==0) {
	    if(im_preedit_flag) {
		if((p_point = (XPoint *)Xmalloc(sizeof(XPoint))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_point->x = preedit_data->SpotLocation.x;
		p_point->y = preedit_data->SpotLocation.y;
	    } else {
		if(XIMP_CHK_PRESPOTLMASK(ic)) {
		    if((p_point = (XPoint *)Xmalloc(sizeof(XPoint))) == NULL) {
			*return_name = p->name;
			return(False);
		    }
		    p_point->x = ic->core.preedit_attr.spot_location.x;
		    p_point->y = ic->core.preedit_attr.spot_location.y;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	    *((XPoint **)(p->value)) = p_point;
	} else if(strcmp(p->name, XNColormap)==0) {
	    if(im_preedit_flag) {
		 *((Colormap *)(p->value)) = preedit_data->Colormap;
	    } else {
		if(XIMP_CHK_PRECOLORMAPMASK(ic)) {
		     *((Colormap *)(p->value)) = ic->core.preedit_attr.colormap;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNStdColormap)==0) {
	    if(im_preedit_flag) {
		 *((Atom *)(p->value)) = preedit_data->StdColormap;
	    } else {
		if(XIMP_CHK_PRESTDCOLORMAPMASK(ic))
		     *((Atom *)(p->value)) = ic->core.preedit_attr.std_colormap;
		else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNBackground)==0) {
	    if(im_preedit_flag) {
		 *((unsigned long *)(p->value)) = preedit_data->Background;
	    } else {
		if(XIMP_CHK_PREBGMASK(ic)) {
		     *((unsigned long *)(p->value)) = ic->core.preedit_attr.background;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNForeground)==0) {
	    if(im_preedit_flag) {
		 *((unsigned long *)(p->value)) = preedit_data->Foreground;
	    } else {
		if(XIMP_CHK_PREFGMASK(ic)) {
		     *((unsigned long *)(p->value)) = ic->core.preedit_attr.foreground;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNBackgroundPixmap)==0) {
	    if(im_preedit_flag) {
		 *((Pixmap *)(p->value)) = preedit_data->Bg_Pixmap;
	    } else {
		if(XIMP_CHK_PREBGPIXMAPMASK(ic)) {
		     *((Pixmap *)(p->value)) = ic->core.preedit_attr.background_pixmap;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNFontSet)==0) {
	    if(XIMP_CHK_PREFONTMASK(ic)) {
		 *((XFontSet *)(p->value)) = ic->core.preedit_attr.fontset;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNLineSpace)==0) {
	    if(im_preedit_flag) {
		 *((int *)(p->value)) = preedit_data->LineSpacing;
	    } else {
		if(XIMP_CHK_PRELINESPMASK(ic)) {
		     *((int *)(p->value)) = ic->core.preedit_attr.line_spacing;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNCursor)==0) {
	    if(im_preedit_flag) {
		 *((Cursor *)(p->value)) = preedit_data->Cursor;
	    } else {
		if(XIMP_CHK_PRECURSORMASK(ic)) {
		     *((Cursor *)(p->value)) = ic->core.preedit_attr.cursor;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNPreeditStartCallback)==0) {
	    if((int)ic->core.preedit_attr.start_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.preedit_attr.start_callback.client_data;
		p_callback->callback =
			ic->core.preedit_attr.start_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNPreeditDrawCallback)==0) {
	    if((int)ic->core.preedit_attr.draw_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.preedit_attr.draw_callback.client_data;
		p_callback->callback =
			ic->core.preedit_attr.draw_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNPreeditDoneCallback)==0) {
	    if((int)ic->core.preedit_attr.done_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.preedit_attr.done_callback.client_data;
		p_callback->callback =
			ic->core.preedit_attr.done_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNPreeditCaretCallback)==0) {
	    if((int)ic->core.preedit_attr.caret_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.preedit_attr.caret_callback.client_data;
		p_callback->callback =
			ic->core.preedit_attr.caret_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	}
    }
    if( preedit_data )
	XFree(preedit_data);
    return(True);
}

Private Bool
_Ximp_StatusGetAttributes(ic, vl, return_name)
    Ximp_XIC		 ic;
    XIMArg	 	*vl;
    char		**return_name;
{
    XIMArg			*p;
    XRectangle			*p_rect;
    unsigned long		 mask;
    int				 im_status_flag = 0;
    Ximp_StatusPropRec4		*status_data = NULL;
    Ximp_StatusPropRec3		*status_data_bc = NULL;
    XIMCallback 		*p_callback;

    if(IS_IC_CONNECTED(ic)){
	for(mask = 0, p = vl; p->name != NULL; p++) {
	    if(strcmp(p->name, XNArea)==0)
		mask |= XIMP_STS_AREA_MASK(ic);
	    else if(strcmp(p->name, XNAreaNeeded)==0)
		mask |= XIMP_STS_AREANEED_MASK(ic);
	    else if(strcmp(p->name, XNColormap)==0)
		mask |= XIMP_STS_COLORMAP_MASK(ic);
	    else if(strcmp(p->name, XNStdColormap)==0)
		mask |= XIMP_STS_STD_COLORMAP_MASK(ic);
	    else if(strcmp(p->name, XNBackground)==0)
		mask |= XIMP_STS_BG_MASK(ic);
	    else if(strcmp(p->name, XNForeground)==0)
		mask |= XIMP_STS_FG_MASK(ic);
	    else if(strcmp(p->name, XNBackgroundPixmap)==0)
		mask |= XIMP_STS_BGPIXMAP_MASK(ic);
	    else if(strcmp(p->name, XNLineSpace)==0)
		mask |= XIMP_STS_LINESP_MASK(ic);
	    else if(strcmp(p->name, XNCursor)==0)
		mask |= XIMP_STS_CURSOR_MASK(ic);
	}
	if(mask) {
	    status_data = (Ximp_StatusPropRec4 *)_Ximp_GetRequestIM(ic, mask,
			((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id,
			((Ximp_XIM)ic->core.im)->ximp_impart->status_atr_id);
	    if(status_data != (Ximp_StatusPropRec4 *)NULL) {
		im_status_flag = 1;
		if( !ISXimp4(ic) ) {
		    status_data_bc = (Ximp_StatusPropRec3 *)status_data;
		    if( (status_data = (Ximp_StatusPropRec4 *)Xmalloc(sizeof(Ximp_StatusPropRec4))) == NULL ) {
			im_status_flag = 0;
		    }
		    else {
			status_data->Area = status_data_bc->Area;
			status_data->AreaNeeded = status_data_bc->AreaNeeded;
			status_data->Colormap = status_data_bc->Colormap;
			status_data->StdColormap = status_data_bc->Colormap;
			status_data->Foreground = status_data_bc->Foreground;
			status_data->Background = status_data_bc->Background;
			status_data->Bg_Pixmap = status_data_bc->Bg_Pixmap;
			status_data->LineSpacing = status_data_bc->LineSpacing;
			status_data->Cursor = status_data_bc->Cursor;
			status_data->window = status_data_bc->window;
		    }
		    XFree( status_data_bc );
		}
	    }
	}
    }

    for(p = vl; p->name != NULL; p++) {
	if(strcmp(p->name, XNArea)==0) {
	    if(im_status_flag) {
		if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_rect->x       = status_data->Area.x;
		p_rect->y       = status_data->Area.y;
		p_rect->width   = status_data->Area.width;
		p_rect->height  = status_data->Area.height;
	    } else {
		if(XIMP_CHK_STSAREAMASK(ic)) {
		    if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
			*return_name = p->name;
			return(False);
		    }
		    p_rect->x       = ic->core.status_attr.area.x;
		    p_rect->y       = ic->core.status_attr.area.y;
		    p_rect->width   = ic->core.status_attr.area.width;
		    p_rect->height  = ic->core.status_attr.area.height;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	    *((XRectangle **)(p->value)) = p_rect;
	} else if(strcmp(p->name, XNAreaNeeded)==0) {
	    if(im_status_flag) {
		if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_rect->x  = p_rect->y  = 0;
		p_rect->width   = status_data->AreaNeeded.width;
		p_rect->height  = status_data->AreaNeeded.height;
	    } else {
		if(XIMP_CHK_STSAREANEEDMASK(ic)) {
		    if((p_rect = (XRectangle *)Xmalloc(sizeof(XRectangle))) == NULL) {
			*return_name = p->name;
			return(False);
		    }
		    p_rect->x  = p_rect->y  = 0;
		    p_rect->width   = ic->core.status_attr.area_needed.width;
		    p_rect->height  = ic->core.status_attr.area_needed.height;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	    *((XRectangle **)(p->value)) = p_rect;
	} else if(strcmp(p->name, XNColormap)==0) {
	    if(im_status_flag) {
		 *((Colormap *)(p->value)) = status_data->Colormap;
	    } else {
		if(XIMP_CHK_STSCOLORMAPMASK(ic)) {
		     *((Colormap *)(p->value)) = ic->core.status_attr.colormap;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNStdColormap)==0) {
	    if(im_status_flag) {
		 *((Atom *)(p->value)) = status_data->StdColormap;
	    } else {
		if(XIMP_STS_STD_COLORMAP_MASK(ic)) {
		     *((Atom *)(p->value)) = ic->core.status_attr.std_colormap;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNBackground)==0) {
	    if(im_status_flag) {
		 *((unsigned long *)(p->value)) = status_data->Background;
	    } else {
		if(XIMP_CHK_STSBGMASK(ic)) {
		     *((unsigned long *)(p->value)) = ic->core.status_attr.background;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNForeground)==0) {
	    if(im_status_flag) {
		 *((unsigned long *)(p->value)) = status_data->Foreground;
	    } else {
		if(XIMP_CHK_STSFGMASK(ic)) {
		     *((unsigned long *)(p->value)) = ic->core.status_attr.foreground;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNBackgroundPixmap)==0) {
	    if(im_status_flag) {
		 *((Pixmap *)(p->value)) = status_data->Bg_Pixmap;
	    } else {
		if(XIMP_CHK_STSBGPIXMAPMASK(ic)) {
		     *((Pixmap *)(p->value)) = ic->core.status_attr.background_pixmap;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNFontSet)==0) {
	    if(XIMP_CHK_STSFONTMASK(ic)) {
		 *((XFontSet *)(p->value)) = ic->core.status_attr.fontset;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNLineSpace)==0) {
	    if(im_status_flag) {
		 *((int *)(p->value)) = status_data->LineSpacing;
	    } else {
		if(XIMP_CHK_STSLINESPMASK(ic)) {
		     *((int *)(p->value)) = ic->core.status_attr.line_spacing;
		} else {
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNCursor)==0) {
	    if(im_status_flag) {
		 *((Cursor *)(p->value)) = status_data->Cursor;
	    } else {
		if(XIMP_CHK_STSCURSORMASK(ic)) {
		     *((Cursor *)(p->value)) = ic->core.status_attr.cursor;
		} else { 
		    *return_name = p->name;
		    return(False);
		}
	    }
	} else if(strcmp(p->name, XNStatusStartCallback)==0) {
	    if((int)ic->core.status_attr.start_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.status_attr.start_callback.client_data;
		p_callback->callback =
			ic->core.status_attr.start_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNStatusDrawCallback)==0) {
	    if((int)ic->core.status_attr.draw_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.status_attr.draw_callback.client_data;
		p_callback->callback =
			ic->core.status_attr.draw_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	} else if(strcmp(p->name, XNStatusDoneCallback)==0) {
	    if((int)ic->core.status_attr.done_callback.callback) {
		if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) == NULL) {
		    *return_name = p->name;
		    return(False);
		}
		p_callback->client_data =
			ic->core.status_attr.done_callback.client_data;
		p_callback->callback =
			ic->core.status_attr.done_callback.callback;
		*((XIMCallback **)(p->value)) = p_callback;
	    } else {
		*return_name = p->name;
		return(False);
	    }
	}
    }
    if( status_data )
	XFree(status_data);
    return(True);
}

Public char *
_Ximp_GetICValues(xic, values)
    XIC		 xic;
    XIMArg	*values;
{
    Ximp_XIC	 ic = (Ximp_XIC)xic;
    XIMArg	*p;
    char	*p_char;
    char	*return_name = NULL;
    int		 len;

    if(!IS_SERVER_CONNECTED(ic->core.im) && IS_RECONNECTABLE(ic->core.im))
	_Ximp_ConnectServer( (Ximp_XIM)ic->core.im );
    if( IS_SERVER_CONNECTED(ic->core.im)  &&  !IS_IC_CONNECTED(ic) )
	if( _Ximp_ConnectIC( ic, XIMP_START_IC ) )
	    if(IS_RESTARTABLE(ic->core.im))
		_Ximp_CallRestartCallbackExtensionHook( ic );

    for(p = values; p->name != NULL; p++) {
	if(strcmp(p->name, XNInputStyle) == 0) {
	    if(ic->ximp_icpart->value_mask & XIMP_INPUT_STYLE) {
		*((XIMStyle *)(p->value)) = ic->core.input_style;
	    } else {			    
		return_name = p->name;
		break;
	    }
	} else if(strcmp(p->name, XNClientWindow)==0) {
	    if(ic->ximp_icpart->value_mask & XIMP_CLIENT_WIN) {
		*((Window *)(p->value)) = ic->core.client_window;
	    } else {
		return_name = p->name;
		break;
	    }
	} else if(strcmp(p->name, XNFocusWindow)==0) {
	    if(XIMP_CHK_FOCUSWINMASK(ic)) {
		*((Window *)(p->value)) = ic->core.focus_window;
	    } else if(IS_IC_CONNECTED(ic)) {
		XPointer tmp = _Ximp_GetRequestIM(ic,
			    XIMP_FOCUS_WIN_MASK(ic),
			    ((Ximp_XIM)ic->core.im)->ximp_impart->focus_win_id,
			    XA_WINDOW);
		*((Window *)(p->value)) = *(Window *)tmp ;
		Xfree(tmp) ;
	    } else {
		return_name = p->name;
		break;
	    }
	} else if(strcmp(p->name, XNResourceName)==0) {
            if(ic->ximp_icpart->res_name != (char *)NULL) {
                len = strlen(ic->ximp_icpart->res_name);
                if((p_char = Xmalloc(len+1)) == NULL) {
                    return_name = p->name;
                    break;
                }
                strcpy(p_char, ic->ximp_icpart->res_name);
                *((char **)(p->value)) = p_char;
            } else {
                return_name = p->name;
                break;
            }
        } else if(strcmp(p->name, XNResourceClass)==0) {
            if(ic->ximp_icpart->res_class != (char *)NULL) {
                len = strlen(ic->ximp_icpart->res_class);
                if((p_char = Xmalloc(len+1)) == NULL) {
                    return_name = p->name;
                    break;
                }
                strcpy(p_char, ic->ximp_icpart->res_class);
                *((char **)(p->value)) = p_char;
            } else {
                return_name = p->name;
                break;
            }
	} else if(strcmp(p->name, XNGeometryCallback)==0) {
	    if(ic->ximp_icpart->value_mask & XIMP_GEOMETRY_CB) {
		XIMCallback 	*p_callback;
                if((p_callback = (XIMCallback *)Xmalloc(sizeof(XIMCallback))) ==
 NULL) {
                    return_name = p->name;
                    break;
                }
                p_callback->client_data =
                        ic->core.geometry_callback.client_data;
                p_callback->callback =
                        ic->core.geometry_callback.callback;
                *((XIMCallback **)(p->value)) = p_callback;
	    } else {
		return_name = p->name;
		break;
	    }
	} else if(strcmp(p->name, XNFilterEvents)==0) {
	    *((unsigned long *)(p->value)) = ic->core.filter_events;
	} else if(strcmp(p->name, XNPreeditAttributes)==0) {
	    if( _Ximp_PreGetAttributes(ic, p->value, &return_name) == False)
		break;
	} else if(strcmp(p->name, XNStatusAttributes)==0) {
	    if( _Ximp_StatusGetAttributes(ic, p->value, &return_name) == False)
		break;
	} else {
	    if( _Ximp_GetICExtensionHook(ic, p->name, (long)p->value) == False) {
		return_name = p->name;
		break;
	    }
	}
    }
    return(return_name);
}
