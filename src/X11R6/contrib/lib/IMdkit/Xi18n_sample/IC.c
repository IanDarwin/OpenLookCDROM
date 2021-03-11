/******************************************************************

         Copyright 1993, 1994 by Hewlett-Packard Company

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Hewlett-Packard not
be used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.
Hewlett-Packard Company makes no representations about the suitability
of this software for any purpose.
It is provided "as is" without express or implied warranty.

HEWLETT-PACKARD COMPANY DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
IN NO EVENT SHALL HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY SPECIAL,
INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE.

Author:
    Hidetoshi Tajima	Hewlett-Packard Company.
			(tajima@kobe.hp.com)
******************************************************************/

#include <X11/Xlib.h>
#include <X11/Ximd/IMdkit.h>
#include <X11/Ximd/Xi18n.h>
#include "IC.h"

static IC *ic_list = (IC *)NULL;
static IC *free_list = (IC *)NULL;

static IC
*NewIC()
{
    static CARD16 icid = 0;
    IC *rec;

    if (free_list != NULL) {
	rec = free_list;
	free_list = free_list->next;
    } else {
	rec = (IC *)malloc(sizeof(IC));
    }
    memset(rec, 0, sizeof(IC));
    rec->id = ++icid;

    rec->next = ic_list;
    ic_list = rec;
    return rec;
}

static void
StoreIC(rec, call_data)
IC *rec;
IMChangeICStruct *call_data;
{
    XICAttribute *ic_attr = call_data->ic_attr;
    XICAttribute *pre_attr = call_data->preedit_attr;
    XICAttribute *sts_attr = call_data->status_attr;
    register int i;

    for (i = 0; i < (int)call_data->ic_attr_num; i++, ic_attr++) {
	if (!strcmp(XNInputStyle, ic_attr->name))
	  rec->input_style = *(INT32*)ic_attr->value;
	else if (!strcmp(XNClientWindow, ic_attr->name))
	  rec->client_win = *(Window*)ic_attr->value;
	else if (!strcmp(XNFocusWindow, ic_attr->name))
	  rec->focus_win = *(Window*)ic_attr->value;
    }
    for (i = 0; i < (int)call_data->preedit_attr_num; i++, pre_attr++) {
	if (!strcmp(XNArea, pre_attr->name))
	  rec->pre_attr.area = *(XRectangle*)pre_attr->value;
	else if (!strcmp(XNAreaNeeded, pre_attr->name))
	  rec->pre_attr.area_needed = *(XRectangle*)pre_attr->value;
	else if (!strcmp(XNSpotLocation, pre_attr->name))
	  rec->pre_attr.spot_location = *(XPoint*)pre_attr->value;
	else if (!strcmp(XNColormap, pre_attr->name))
	  rec->pre_attr.cmap = *(Colormap*)pre_attr->value;
	else if (!strcmp(XNStdColormap, pre_attr->name))
	  rec->pre_attr.cmap = *(Colormap*)pre_attr->value;
	else if (!strcmp(XNForeground, pre_attr->name))
	  rec->pre_attr.foreground = *(CARD32*)pre_attr->value;
	else if (!strcmp(XNBackground, pre_attr->name))
	  rec->pre_attr.background = *(CARD32*)pre_attr->value;
	else if (!strcmp(XNBackgroundPixmap, pre_attr->name))
	  rec->pre_attr.bg_pixmap = *(Pixmap*)pre_attr->value;
	else if (!strcmp(XNFontSet, pre_attr->name)) {
	    int str_length = strlen(pre_attr->value);
	    if (rec->pre_attr.base_font != NULL) {
		if (strcmp(rec->pre_attr.base_font, pre_attr->value)) {
		    XFree(rec->pre_attr.base_font);
		} else {
		    continue;
		}
	    }
	    rec->pre_attr.base_font = malloc(str_length + 1);
	    strcpy(rec->pre_attr.base_font, pre_attr->value);
	} else if (!strcmp(XNLineSpace, pre_attr->name))
	  rec->pre_attr.line_space = *(CARD32*)pre_attr->value;
	else if (!strcmp(XNCursor, pre_attr->name))
	  rec->pre_attr.cursor = *(Cursor*)pre_attr->value;
    }
    for (i = 0; i < (int)call_data->status_attr_num; i++, sts_attr++) {
	if (!strcmp(XNArea, sts_attr->name))
	  rec->sts_attr.area = *(XRectangle*)sts_attr->value;
	else if (!strcmp(XNAreaNeeded, sts_attr->name))
	  rec->sts_attr.area_needed = *(XRectangle*)sts_attr->value;
	else if (!strcmp(XNColormap, sts_attr->name))
	  rec->sts_attr.cmap = *(Colormap*)sts_attr->value;
	else if (!strcmp(XNStdColormap, sts_attr->name))
	  rec->sts_attr.cmap = *(Colormap*)sts_attr->value;
	else if (!strcmp(XNForeground, sts_attr->name))
	  rec->sts_attr.foreground = *(CARD32*)sts_attr->value;
	else if (!strcmp(XNBackground, sts_attr->name))
	  rec->sts_attr.background = *(CARD32*)sts_attr->value;
	else if (!strcmp(XNBackgroundPixmap, sts_attr->name))
	  rec->sts_attr.bg_pixmap = *(Pixmap*)sts_attr->value;
	else if (!strcmp(XNFontSet, sts_attr->name)) {
	    int str_length = strlen(sts_attr->value);
	    if (rec->sts_attr.base_font != NULL) {
		if (strcmp(rec->sts_attr.base_font, sts_attr->value)) {
		    XFree(rec->sts_attr.base_font);
		} else {
		    continue;
		}
	    }
	    rec->sts_attr.base_font = malloc(str_length + 1);
	    strcpy(rec->sts_attr.base_font, sts_attr->value);
	} else if (!strcmp(XNLineSpace, sts_attr->name))
	  rec->sts_attr.line_space= *(CARD32*)sts_attr->value;
	else if (!strcmp(XNCursor, sts_attr->name))
	  rec->sts_attr.cursor = *(Cursor*)sts_attr->value;
    }

}

IC
*FindIC(icid)
CARD16 icid;
{
    IC *rec = ic_list;

    while (rec != NULL) {
	if (rec->id == icid)
	  return rec;
	rec = rec->next;
    }

    return NULL;
}

void
CreateIC(call_data)
IMChangeICStruct *call_data;
{
    IC *rec;

    rec = NewIC();
    if (rec == NULL)
      return;
    StoreIC(rec, call_data);
    call_data->icid = rec->id;
    return;
}

void
SetIC(call_data)
IMChangeICStruct *call_data;
{
    IC *rec = FindIC(call_data->icid);

    if (rec == NULL)
      return;
    StoreIC(rec, call_data);
    return;
}

void
GetIC(call_data)
IMChangeICStruct *call_data;
{
    XICAttribute *ic_attr = call_data->ic_attr;
    XICAttribute *pre_attr = call_data->preedit_attr;
    XICAttribute *sts_attr = call_data->status_attr;
    register int i;
    IC *rec = FindIC(call_data->icid);

    if (rec == NULL)
      return;
    for (i = 0; i < (int)call_data->ic_attr_num; i++, ic_attr++) {
	if (!strcmp(XNFilterEvents, ic_attr->name)) {
	    ic_attr->value = (void *)malloc(sizeof(CARD32));
	    *(CARD32*)ic_attr->value = KeyPressMask|KeyReleaseMask;
	    ic_attr->value_length = sizeof(CARD32);
	}
    }

    /* preedit attributes */
    for (i = 0; i < (int)call_data->preedit_attr_num; i++, pre_attr++) {
	if (!strcmp(XNArea, pre_attr->name)) {
	    pre_attr->value = (void *)malloc(sizeof(XRectangle));
	    *(XRectangle*)pre_attr->value = rec->pre_attr.area;
	    pre_attr->value_length = sizeof(XRectangle);
	} else if (!strcmp(XNAreaNeeded, pre_attr->name)) {
	    pre_attr->value = (void *)malloc(sizeof(XRectangle));
	    *(XRectangle*)pre_attr->value = rec->pre_attr.area_needed;
	    pre_attr->value_length = sizeof(XRectangle);
	} else if (!strcmp(XNSpotLocation, pre_attr->name)) {
	    pre_attr->value = (void *)malloc(sizeof(XPoint));
	    *(XPoint*)pre_attr->value = rec->pre_attr.spot_location;
	    pre_attr->value_length = sizeof(XPoint);
	} else if (!strcmp(XNFontSet, pre_attr->name)) {
	    CARD16 base_len = (CARD16)strlen(rec->pre_attr.base_font);
	    int total_len = sizeof(CARD16) + (CARD16)base_len;
	    char *p;

	    pre_attr->value = (void *)malloc(total_len);
	    p = (char *)pre_attr->value;
	    memmove(p, &base_len, sizeof(CARD16));
	    p += sizeof(CARD16);
	    strncpy(p, rec->pre_attr.base_font, base_len);
	    pre_attr->value_length = total_len;
	} else if (!strcmp(XNForeground, pre_attr->name)) {
	    pre_attr->value = (void *)malloc(sizeof(long));
	    *(long*)pre_attr->value = rec->pre_attr.foreground;
	    pre_attr->value_length = sizeof(long);
	} else if (!strcmp(XNBackground, pre_attr->name)) {
	    pre_attr->value = (void *)malloc(sizeof(long));
	    *(long*)pre_attr->value = rec->pre_attr.background;
	    pre_attr->value_length = sizeof(long);
	} else if (!strcmp(XNLineSpace, pre_attr->name)) {
	    pre_attr->value = (void *)malloc(sizeof(long));
#if 0
	    *(long*)pre_attr->value = rec->pre_attr.line_space;
#endif
	    *(long*)pre_attr->value = 18;
	    pre_attr->value_length = sizeof(long);
	}
    }

    /* status attributes */
    for (i = 0; i < (int)call_data->status_attr_num; i++, sts_attr++) {
	if (!strcmp(XNArea, sts_attr->name)) {
	    sts_attr->value = (void *)malloc(sizeof(XRectangle));
	    *(XRectangle*)sts_attr->value = rec->sts_attr.area;
	    sts_attr->value_length = sizeof(XRectangle);
	} else if (!strcmp(XNAreaNeeded, sts_attr->name)) {
	    sts_attr->value = (void *)malloc(sizeof(XRectangle));
	    *(XRectangle*)sts_attr->value = rec->sts_attr.area_needed;
	    sts_attr->value_length = sizeof(XRectangle);
	} else if (!strcmp(XNFontSet, sts_attr->name)) {
	    CARD16 base_len = (CARD16)strlen(rec->sts_attr.base_font);
	    int total_len = sizeof(CARD16) + (CARD16)base_len;
	    char *p;

	    sts_attr->value = (void *)malloc(total_len);
	    p = (char *)sts_attr->value;
	    memmove(p, &base_len, sizeof(CARD16));
	    p += sizeof(CARD16);
	    strncpy(p, rec->sts_attr.base_font, base_len);
	    sts_attr->value_length = total_len;
	} else if (!strcmp(XNForeground, sts_attr->name)) {
	    sts_attr->value = (void *)malloc(sizeof(long));
	    *(long*)sts_attr->value = rec->sts_attr.foreground;
	    sts_attr->value_length = sizeof(long);
	} else if (!strcmp(XNBackground, sts_attr->name)) {
	    sts_attr->value = (void *)malloc(sizeof(long));
	    *(long*)sts_attr->value = rec->sts_attr.background;
	    sts_attr->value_length = sizeof(long);
	} else if (!strcmp(XNLineSpace, sts_attr->name)) {
	    sts_attr->value = (void *)malloc(sizeof(long));
#if 0
	    *(long*)sts_attr->value = rec->sts_attr.line_space;
#endif
	    *(long*)sts_attr->value = 18;
	    sts_attr->value_length = sizeof(long);
	}
    }
}
