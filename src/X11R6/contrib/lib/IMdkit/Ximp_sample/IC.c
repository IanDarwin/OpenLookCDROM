/******************************************************************

         Copyright 1993, 1994 by Hewlett-Packard Company
         Copyright 1993, 1994 by Sun Microsystems, Inc.

Permission to use, copy, modify, distribute, and sell this software
and its documentation for any purpose without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Hewlett-Packard and
Sun Microsystems, Inc. not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.
Hewlett-Packard Company and Sun Microsystems, Inc. makes no
representations about the suitability of this software for any purpose.
It is provided "as is" without express or implied warranty.

HEWLETT-PACKARD COMPANY AND SUN MICROSYSTEMS, INC. DISCLAIM ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
HEWLETT-PACKARD COMPANY AND SUN MICROSYSTEMS, INC. BE LIABLE FOR ANY
SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR
IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

Author:
    Hidetoshi Tajima	Hewlett-Packard Company.
			(tajima@kobe.hp.com)
    Hiromu Inukai	Sun Microsystems, Inc.
			(Hiromu.Inukai@Japan.Sun.COM)
******************************************************************/
#include <X11/Xlib.h>
#include "IMdkit.h"
#include "XimpData.h"
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
StoreIC(rec, call_data, is_create)
IC *rec;
XIMPICValuesStruct *call_data;
Bool is_create;
{
    long mask = call_data->attr_mask;
    Ximp_PreeditPropRec4 *pre_values = call_data->pre_values;
    Ximp_StatusPropRec4 *sts_values = call_data->sts_values;
    register int i;

    if(is_create) {
	rec->input_style = call_data->input_style;
	rec->client_win = call_data->client_win;
    }
    if (mask & XIMP_FOCUS_WIN_MASK4)
	  rec->focus_win = call_data->focus_win;
    if (mask & XIMP_PRE_AREA_MASK4) {
	  rec->pre_attr.area.x = pre_values->Area.x;
	  rec->pre_attr.area.y = pre_values->Area.y;
	  rec->pre_attr.area.width = pre_values->Area.width;
	  rec->pre_attr.area.height = pre_values->Area.height;
    }
    if (mask & XIMP_PRE_AREANEED_MASK4) {
	  rec->pre_attr.area_needed.x = 0;
	  rec->pre_attr.area_needed.y = 0;
	  rec->pre_attr.area_needed.width = pre_values->AreaNeeded.width;
	  rec->pre_attr.area_needed.height = pre_values->AreaNeeded.height;
    }
    if (mask & XIMP_PRE_COLORMAP_MASK4)
	  rec->pre_attr.cmap = pre_values->Colormap;
    if (mask & XIMP_PRE_STD_COLORMAP_MASK4)
/*	  rec->pre_attr.cmap = pre_values->Colormap; */
	;
    if (mask & XIMP_PRE_FG_MASK4)
	  rec->pre_attr.foreground = pre_values->Foreground;
    if (mask & XIMP_PRE_BG_MASK4)
	  rec->pre_attr.background = pre_values->Background;
    if (mask & XIMP_PRE_BGPIXMAP_MASK4)
	  rec->pre_attr.bg_pixmap = pre_values->Bg_Pixmap;
    if (mask & XIMP_PRE_LINESP_MASK4)
	  rec->pre_attr.line_space = pre_values->LineSpacing;
    if (mask & XIMP_PRE_CURSOR_MASK4)
	  rec->pre_attr.cursor = pre_values->Cursor;
    if (mask & XIMP_PRE_FONT_MASK4) {
	    int str_length = strlen(call_data->pre_font);
	    if (rec->pre_attr.base_font != NULL) {
		if (strcmp(rec->pre_attr.base_font, call_data->pre_font)) {
		    XFree(rec->pre_attr.base_font);
		}
	    }
	    rec->pre_attr.base_font = (char *)malloc(str_length + 1);
	    strcpy(rec->pre_attr.base_font, call_data->pre_font);
    }
    if (mask & XIMP_PRE_SPOTL_MASK4) {
	  rec->pre_attr.spot_location.x = pre_values->SpotLocation.x;
	  rec->pre_attr.spot_location.y = pre_values->SpotLocation.y;
    }
    if (mask & XIMP_STS_AREA_MASK4) {
	  rec->sts_attr.area.x = sts_values->Area.x;
	  rec->sts_attr.area.y = sts_values->Area.y;
	  rec->sts_attr.area.width = sts_values->Area.width;
	  rec->sts_attr.area.height = sts_values->Area.height;
    }
    if (mask & XIMP_STS_AREANEED_MASK4) {
	  rec->sts_attr.area_needed.x = 0;
	  rec->sts_attr.area_needed.y = 0;
	  rec->sts_attr.area_needed.width = sts_values->AreaNeeded.width;
	  rec->sts_attr.area_needed.height = sts_values->AreaNeeded.height;
    }
    if (mask & XIMP_STS_COLORMAP_MASK4)
	  rec->sts_attr.cmap = sts_values->Colormap;
    if (mask & XIMP_STS_STD_COLORMAP_MASK4)
/*	  rec->sts_attr.cmap = sts_values->Colormap; */
	;
    if (mask & XIMP_STS_FG_MASK4)
	  rec->sts_attr.foreground = sts_values->Foreground;
    if (mask & XIMP_STS_BG_MASK4)
	  rec->sts_attr.background = sts_values->Background;
    if (mask & XIMP_STS_BGPIXMAP_MASK4)
	  rec->sts_attr.bg_pixmap = sts_values->Bg_Pixmap;
    if (mask & XIMP_STS_LINESP_MASK4)
	  rec->sts_attr.line_space= sts_values->LineSpacing;
    if (mask & XIMP_STS_CURSOR_MASK4)
	  rec->sts_attr.cursor = sts_values->Cursor;
    if (mask & XIMP_STS_FONT_MASK4) {
	    int str_length = strlen(call_data->sts_font);
	    if (rec->sts_attr.base_font != NULL) {
		if (strcmp(rec->sts_attr.base_font, call_data->sts_font)) {
		    XFree(rec->sts_attr.base_font);
		}
	    }
	    rec->sts_attr.base_font = (char *)malloc(str_length + 1);
	    strcpy(rec->sts_attr.base_font, call_data->sts_font);
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
XIMPICValuesStruct *call_data;
{
    IC *rec;

    rec = NewIC();
    if (rec == NULL)
      return;
    StoreIC(rec, call_data, True);
    call_data->icid = rec->id;
    return;
}

void
SetIC(call_data)
XIMPICValuesStruct *call_data;
{
    IC *rec = FindIC(call_data->icid);

    if (rec == NULL)
    StoreIC(rec, call_data, False);
      return;
}

static void
#if NeedFunctionPrototypes
_get_prevalues(long mask, IC *rec, Ximp_PreeditPropRec4 **pre_values, char **pre_font)
#else
_get_prevalues(mask, rec, pre_values, pre_font)
long mask;
IC *rec;
Ximp_PreeditPropRec4 **pre_values;
char **pre_font;
#endif
{
    if (mask & XIMP_PRE_AREA_MASK4) {
#ifdef DEBUG	/* Skelton IMS returns temp. value. */
	  rec->pre_attr.area.x = 0;
	  rec->pre_attr.area.y = 0;
	  rec->pre_attr.area.width = 200;
	  rec->pre_attr.area.height = 100;
#endif
	  (*pre_values)->Area.x = rec->pre_attr.area.x;
	  (*pre_values)->Area.y = rec->pre_attr.area.y;
	  (*pre_values)->Area.width = rec->pre_attr.area.width;
	  (*pre_values)->Area.height = rec->pre_attr.area.height;
    }
    if (mask & XIMP_PRE_AREANEED_MASK4) {
#ifdef DEBUG	/* Skelton IMS returns temp. value. */
	  rec->pre_attr.area_needed.width = 200;
	  rec->pre_attr.area_needed.height = 100;
#endif
	  (*pre_values)->AreaNeeded.width = rec->pre_attr.area_needed.width;
	  (*pre_values)->AreaNeeded.height = rec->pre_attr.area_needed.height;
    }
    if (mask & XIMP_PRE_COLORMAP_MASK4) {
	  (*pre_values)->Colormap = rec->pre_attr.cmap;
    }
    if (mask & XIMP_PRE_STD_COLORMAP_MASK4)
/*	  (*pre_values)->Colormap = rec->pre_attr.cmap; */
    if (mask & XIMP_PRE_FG_MASK4) {
	  (*pre_values)->Foreground = rec->pre_attr.foreground;
    }
    if (mask & XIMP_PRE_BG_MASK4) {
	  (*pre_values)->Background = rec->pre_attr.background;
    }
    if (mask & XIMP_PRE_BGPIXMAP_MASK4) {
	  (*pre_values)->Bg_Pixmap = rec->pre_attr.bg_pixmap;
    }
    if (mask & XIMP_PRE_LINESP_MASK4) {
	  (*pre_values)->LineSpacing = rec->pre_attr.line_space;
    }
    if (mask & XIMP_PRE_CURSOR_MASK4) {
	  (*pre_values)->Cursor = rec->pre_attr.cursor;
    }
    if (mask & XIMP_PRE_FONT_MASK4) {
	    int str_length = strlen(rec->pre_attr.base_font);
	    *pre_font = (char*)malloc(str_length+1);
	    strncpy(*pre_font, rec->pre_attr.base_font, str_length);
    }
    if (mask & XIMP_PRE_SPOTL_MASK4) {
	  (*pre_values)->SpotLocation.x = rec->pre_attr.spot_location.x;
	  (*pre_values)->SpotLocation.y = rec->pre_attr.spot_location.y;
    }
}

static void
#if NeedFunctionPrototypes
_get_stsvalues(long mask, IC *rec, Ximp_StatusPropRec4 **sts_values, char **sts_font)
#else
_get_stsvalues(mask, rec, sts_values, sts_font)
long mask;
IC *rec;
Ximp_StatusPropRec4 **sts_values;
char **sts_font;
#endif
{
    if (mask & XIMP_STS_AREA_MASK4) {
#ifdef DEBUG	/* Skelton IMS returns temp. value. */
	  rec->sts_attr.area.x = 0;
	  rec->sts_attr.area.y = 0;
	  rec->sts_attr.area.width = 20;
	  rec->sts_attr.area.height = 10;
#endif
	  (*sts_values)->Area.x = rec->sts_attr.area.x;
	  (*sts_values)->Area.y = rec->sts_attr.area.y;
	  (*sts_values)->Area.width = rec->sts_attr.area.width;
	  (*sts_values)->Area.height = rec->sts_attr.area.height;
    }
    if (mask & XIMP_STS_AREANEED_MASK4) {
#ifdef DEBUG	/* Skelton IMS returns temp. value. */
	  rec->sts_attr.area_needed.x = 0;
	  rec->sts_attr.area_needed.y = 0;
	  rec->sts_attr.area_needed.width = 20;
	  rec->sts_attr.area_needed.height = 10;
#endif
	  (*sts_values)->Area.x = rec->sts_attr.area_needed.x;
	  (*sts_values)->Area.y = rec->sts_attr.area_needed.y;
	  (*sts_values)->Area.width = rec->sts_attr.area_needed.width;
	  (*sts_values)->Area.height = rec->sts_attr.area_needed.height;
    }
    if (mask & XIMP_STS_COLORMAP_MASK4) {
	  (*sts_values)->Colormap = rec->sts_attr.cmap;
    }
    if (mask & XIMP_STS_STD_COLORMAP_MASK4) {
/*	  (*sts_values)->Colormap = rec->sts_attr.cmap; */
    }
    if (mask & XIMP_STS_FG_MASK4) {
	  (*sts_values)->Foreground = rec->sts_attr.foreground;
    }
    if (mask & XIMP_STS_BG_MASK4) {
	  (*sts_values)->Background = rec->sts_attr.background;
    }
    if (mask & XIMP_STS_BGPIXMAP_MASK4) {
	  (*sts_values)->Bg_Pixmap = rec->sts_attr.bg_pixmap;
    }
    if (mask & XIMP_STS_LINESP_MASK4) {
	  (*sts_values)->LineSpacing = rec->sts_attr.line_space;
    }
    if (mask & XIMP_STS_CURSOR_MASK4) {
	  (*sts_values)->Cursor = rec->sts_attr.cursor;
    }
    if (mask & XIMP_STS_FONT_MASK4) {
	    int str_length = strlen(rec->sts_attr.base_font);
	    *sts_font = (char*)malloc(str_length + 1);
	    strncpy(*sts_font, rec->sts_attr.base_font, str_length);
    }
}

void
GetIC(call_data)
XIMPICValuesStruct *call_data;
{
    long mask = call_data->attr_mask;
    Ximp_PreeditPropRec4 *pre_values;
    Ximp_StatusPropRec4 *sts_values;
    register int i;

    IC *rec = FindIC(call_data->icid);

    call_data->input_style = rec->input_style;
    call_data->client_win = rec->client_win;
    if (mask & XIMP_FOCUS_WIN_MASK4) {
	  call_data->focus_win = rec->focus_win;
    }
    if(mask &
       (XIMP_PRE_AREA_MASK4 |
       XIMP_PRE_AREANEED_MASK4 |
       XIMP_PRE_COLORMAP_MASK4 |
       XIMP_PRE_STD_COLORMAP_MASK4 |
       XIMP_PRE_FG_MASK4 |
       XIMP_PRE_BG_MASK4 |
       XIMP_PRE_BGPIXMAP_MASK4 |
       XIMP_PRE_LINESP_MASK4 |
       XIMP_PRE_CURSOR_MASK4 |
       XIMP_PRE_SPOTL_MASK4)) {
	call_data->pre_values = (Ximp_PreeditPropRec4*)malloc(sizeof(Ximp_PreeditPropRec4));
	pre_values = call_data->pre_values;
	_get_prevalues(mask, rec, &pre_values, &(call_data->pre_font));
    }
    if(mask &
       (XIMP_STS_AREA_MASK4 |
       XIMP_STS_AREANEED_MASK4 |
       XIMP_STS_COLORMAP_MASK4 |
       XIMP_STS_STD_COLORMAP_MASK4 |
       XIMP_STS_FG_MASK4 |
       XIMP_STS_BG_MASK4 |
       XIMP_STS_BGPIXMAP_MASK4 |
       XIMP_STS_LINESP_MASK4 |
       XIMP_STS_CURSOR_MASK4 |
       XIMP_STS_WINDOW_MASK4)) {
	call_data->sts_values = (Ximp_StatusPropRec4*)malloc(sizeof(Ximp_StatusPropRec4));
	sts_values = call_data->sts_values;
	_get_stsvalues(mask, rec, &sts_values, &(call_data->sts_font));
    }
}
