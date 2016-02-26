#include <stdio.h>
#include <memory.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xresource.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>

#include "i18n.h"

#include "olcursor.h"
#include "ollocale.h"
#include "mem.h"
#include "olwm.h"
#include "win.h"
#include "menu.h"
#include "globals.h"
#include "screen.h"
#include "cursors.h"
#include "st.h"

#ifdef IDENT
#ident "@(#)cursors.c	1.4 olvwm version 07 Jan 1994"
#endif

static st_table	*cursorTable;
static st_table *fontTable;

struct _cursor_data {
    char *name;
    int num;
};

static struct _cursor_data cursor_names[] = {
    { "XC_num_glyphs", XC_num_glyphs },
    { "XC_X_cursor", XC_X_cursor },
    { "XC_arrow", XC_arrow },
    { "XC_based_arrow_down", XC_based_arrow_down },
    { "XC_based_arrow_up", XC_based_arrow_up },
    { "XC_boat", XC_boat },
    { "XC_bogosity", XC_bogosity },
    { "XC_bottom_left_corner", XC_bottom_left_corner },
    { "XC_bottom_right_corner", XC_bottom_right_corner },
    { "XC_bottom_side", XC_bottom_side },
    { "XC_bottom_tee", XC_bottom_tee },
    { "XC_box_spiral", XC_box_spiral },
    { "XC_center_ptr", XC_center_ptr },
    { "XC_circle", XC_circle },
    { "XC_clock", XC_clock },
    { "XC_coffee_mug", XC_coffee_mug },
    { "XC_cross", XC_cross },
    { "XC_cross_reverse", XC_cross_reverse },
    { "XC_crosshair", XC_crosshair },
    { "XC_diamond_cross", XC_diamond_cross },
    { "XC_dot", XC_dot },
    { "XC_dotbox", XC_dotbox },
    { "XC_double_arrow", XC_double_arrow },
    { "XC_draft_large", XC_draft_large },
    { "XC_draft_small", XC_draft_small },
    { "XC_draped_box", XC_draped_box },
    { "XC_exchange", XC_exchange },
    { "XC_fleur", XC_fleur },
    { "XC_gobbler", XC_gobbler },
    { "XC_gumby", XC_gumby },
    { "XC_hand1", XC_hand1 },
    { "XC_hand2", XC_hand2 },
    { "XC_heart", XC_heart },
    { "XC_icon", XC_icon },
    { "XC_iron_cross", XC_iron_cross },
    { "XC_left_ptr", XC_left_ptr },
    { "XC_left_side", XC_left_side },
    { "XC_left_tee", XC_left_tee },
    { "XC_leftbutton", XC_leftbutton },
    { "XC_ll_angle", XC_ll_angle },
    { "XC_lr_angle", XC_lr_angle },
    { "XC_man", XC_man },
    { "XC_middlebutton", XC_middlebutton },
    { "XC_mouse", XC_mouse },
    { "XC_pencil", XC_pencil },
    { "XC_pirate", XC_pirate },
    { "XC_plus", XC_plus },
    { "XC_question_arrow", XC_question_arrow },
    { "XC_right_ptr", XC_right_ptr },
    { "XC_right_side", XC_right_side },
    { "XC_right_tee", XC_right_tee },
    { "XC_rightbutton", XC_rightbutton },
    { "XC_rtl_logo", XC_rtl_logo },
    { "XC_sailboat", XC_sailboat },
    { "XC_sb_down_arrow", XC_sb_down_arrow },
    { "XC_sb_h_double_arrow", XC_sb_h_double_arrow },
    { "XC_sb_left_arrow", XC_sb_left_arrow },
    { "XC_sb_right_arrow", XC_sb_right_arrow },
    { "XC_sb_up_arrow", XC_sb_up_arrow },
    { "XC_sb_v_double_arrow", XC_sb_v_double_arrow },
    { "XC_shuttle", XC_shuttle },
    { "XC_sizing", XC_sizing },
    { "XC_spider", XC_spider },
    { "XC_spraycan", XC_spraycan },
    { "XC_star", XC_star },
    { "XC_target", XC_target },
    { "XC_tcross", XC_tcross },
    { "XC_top_left_arrow", XC_top_left_arrow },
    { "XC_top_left_corner", XC_top_left_corner },
    { "XC_top_right_corner", XC_top_right_corner },
    { "XC_top_side", XC_top_side },
    { "XC_top_tee", XC_top_tee },
    { "XC_trek", XC_trek },
    { "XC_ul_angle", XC_ul_angle },
    { "XC_umbrella", XC_umbrella },
    { "XC_ur_angle", XC_ur_angle },
    { "XC_watch", XC_watch },
    { "XC_xterm", XC_xterm },
    { "OLC_basic", OLC_basic },
    { "OLC_move", OLC_move },
    { "OLC_copy", OLC_copy },
    { "OLC_busy", OLC_busy },
    { "OLC_stop", OLC_stop },
    { "OLC_panning", OLC_panning },
    { "OLC_target", OLC_target },
    { "OLC_nouse", OLC_nouse },
    { "OLC_ptr", OLC_ptr },
    { "OLC_beye", OLC_beye },
    { "OLC_rtarr", OLC_rtarr },
    { "OLC_xhair", OLC_xhair },
    { "OLC_xcurs", OLC_xcurs },
    { "OLC_hourg", OLC_hourg },
    { NULL, -1 }
};

static int
cursorHash(a, modulus)
    register char	*a;
    register int	modulus;
{
int	sum = 0;

    while (*a)
	sum += *a++;
    return sum % modulus;
}

#define DEFAULT_CURSOR	XC_left_ptr

static Font
resLoadFont(dpy, file)
    Display *dpy;
    char *file;
{
    Font fid;
    XFontStruct	*p;

    if (!fontTable)
	fontTable = st_init_table(strcmp, cursorHash);
    
    if (!st_lookup(fontTable, file, &fid)) {
	if ((p = XLoadQueryFont(dpy, file)) == NULL) {
	    ErrorWarning(gettext("An invalid font file was named for a cursor font"));
	    fid = (Font) 0;
	}
	else {
	    fid = p->fid;
	    st_insert(fontTable, file, (char *) fid);
	}
    }
    return fid;
}

static Bool
findNextColor(dpy, ptr, xcolor, cmap)
    Display *dpy;
    char **ptr;
    XColor *xcolor;
    Colormap cmap;
{
char *color, tmp;
Bool return_flag = True;

    for ( ; **ptr && isspace(**ptr); ++(*ptr))
	;	/* read to next space */

    if (**ptr == '\0')
	return_flag = False;
    else {
	color = *ptr;

	for ( ; **ptr && !isspace( **ptr ); ++(*ptr) )
	    ;	/* read to next space */
	tmp = **ptr;
	**ptr = '\0';

	if (!XParseColor(dpy, cmap, color, xcolor) ||
	    !XAllocColor(dpy, cmap, xcolor))
	    return_flag = False;
	**ptr = tmp;
    }
    return return_flag;
}

static void
createCursor(dpy, cmap, pointer, cursor_id, font_file, no_colors, colors_p)
    Display *dpy;
    Colormap cmap;
    Cursor *pointer;
    int cursor_id;
    char *font_file;
    Bool no_colors;
    char *colors_p;
{
    static XColor def_bg, def_fg;
    static Bool def_cols_set = False;
    XColor fg, bg;
    Font fid;
    Bool use_default = False;

    if (!def_cols_set) {
	def_fg.pixel = BlackPixel(dpy, DefaultScreen(dpy));
	XQueryColor(dpy, cmap, &def_fg);
	def_bg.pixel = WhitePixel(dpy, DefaultScreen(dpy));
	XQueryColor(dpy, cmap, &def_bg);
	def_cols_set = True;
    }

    if (no_colors) {
	fg = def_fg;
	bg = def_bg;
    }
    else {
	char *p = colors_p;

	if (findNextColor(dpy, (++p, &p), &fg, cmap)) {
	    if (!*p || !findNextColor(dpy, (++p, &p), &bg, cmap))
		bg = def_bg;
	}
	else {
	    fg = def_fg;
	    bg = def_bg;
	}
    }

    if (font_file == NULL || (fid = resLoadFont(dpy, font_file)) == 0)
	use_default = True;
    else {
	if ((*pointer = XCreateGlyphCursor(dpy, fid, fid, cursor_id,
	  				   cursor_id+1, &fg, &bg )) == NULL)
	    use_default = True;
    }

    if (use_default) {
	*pointer = XCreateFontCursor(dpy, DEFAULT_CURSOR);
	XRecolorCursor(dpy, *pointer, &def_fg, &def_bg);
    }
}

static void
initPointer(dpy, cmap, data, pointer)
    Display *dpy;
    Colormap cmap;
    char *data;
    Cursor *pointer;
{
    char *our_copy, *ptr;
    Bool end = False;
    int cursor_id;
    char *font_file = NULL;
    char *cursor_id_p;

    if (data) {
	our_copy = MemNewString(data);

	for (ptr = our_copy; *ptr && !isspace( *ptr ); ++ptr)
		;
	if ( *ptr == '\0' )
	    end = True;
	else *ptr = '\0';
	    
	if (st_lookup(cursorTable, our_copy, &cursor_id)) {
	    if (our_copy[0] == 'X')
		font_file = "cursor";
	    else if (our_copy[0] == 'O')
		font_file = "olcursor";
	    createCursor(dpy, cmap, pointer, cursor_id, font_file, end, ptr);
	}
	else {
	    if (end)
		createCursor(dpy, cmap, pointer, 0, (char *) NULL, end, ptr);
	    
	    font_file = our_copy;
	    for (++ptr; *ptr && isspace( *ptr ); ++ptr)
		;
	    cursor_id_p = ptr;
	    for (; *ptr && !isspace( *ptr ); ++ptr )
		;
	    if ( *ptr == '\0' )
		end = True;
	    else *ptr = '\0';
	    
	    cursor_id = atoi(cursor_id_p);
	    createCursor(dpy, cmap, pointer, cursor_id, font_file, end, ptr);
	}
	MemFree( our_copy );
    }
}

static void
initOtherPointers(dpy, cmap)
    Display *dpy;
    Colormap cmap;
{
    initPointer(dpy, cmap, GRV.BasicPointerData, &GRV.BasicPointer);
    initPointer(dpy, cmap, GRV.MovePointerData, &GRV.MovePointer);
    initPointer(dpy, cmap, GRV.BusyPointerData, &GRV.BusyPointer);
    initPointer(dpy, cmap, GRV.IconPointerData, &GRV.IconPointer);
    initPointer(dpy, cmap, GRV.ResizePointerData, &GRV.ResizePointer);
    initPointer(dpy, cmap, GRV.MenuPointerData, &GRV.MenuPointer);
    initPointer(dpy, cmap, GRV.QuestionPointerData, &GRV.QuestionPointer);
    initPointer(dpy, cmap, GRV.TargetPointerData, &GRV.TargetPointer);
    initPointer(dpy, cmap, GRV.PanPointerData, &GRV.PanPointer);
    initPointer(dpy, cmap, GRV.CloseUpPointerData, &GRV.CloseUpPointer);
    initPointer(dpy, cmap, GRV.CloseDownPointerData, &GRV.CloseDownPointer);
}

static Bool
initResizePointers(dpy, cmap)
    Display *dpy;
    Colormap cmap;
{
char *ptr;
Bool colorsIncluded = False;
char *our_copy;
XColor colFGC, colBGC, colFGM, colBGM;
int i;

    if (GRV.SpecialResizePointerData) {
	our_copy = MemNewString( GRV.SpecialResizePointerData );
	for (ptr = our_copy; *ptr && !isspace(*ptr); ++ptr)
		;	/* get the first space in ptr */

	if (*ptr) {
	    colorsIncluded = True;
	    *ptr = '\0';
	}

	if (matchBool(our_copy, &GRV.SpecialResizePointers) == True ) {
	    /* Turn on cursors */

	    /* Arrays are in order of enum WhichResize, defined in win.h */
	    GRV.CornerPointers[0] = XCreateFontCursor(dpy, XC_ul_angle);
	    GRV.CornerPointers[1] = XCreateFontCursor(dpy, XC_ur_angle);
	    GRV.CornerPointers[2] = XCreateFontCursor(dpy, XC_ll_angle);
	    GRV.CornerPointers[3] = XCreateFontCursor(dpy, XC_lr_angle);

	    GRV.ResizePointers[0] =
	      		XCreateFontCursor(dpy, XC_top_left_corner);
	    GRV.ResizePointers[1] =
	      		XCreateFontCursor(dpy, XC_top_right_corner);
	    GRV.ResizePointers[2] =
	      		XCreateFontCursor(dpy, XC_bottom_left_corner);
	    GRV.ResizePointers[3] =
	      		XCreateFontCursor(dpy, XC_bottom_right_corner);
	    
	    if (colorsIncluded) {
		if (!findNextColor(dpy, (++ptr, &ptr), &colFGC, cmap) ||
		   !*ptr || !findNextColor(dpy, (++ptr, &ptr), &colBGC, cmap) ||
		   !*ptr || !findNextColor(dpy, (++ptr, &ptr), &colFGM, cmap) ||
		   !*ptr || !findNextColor(dpy, (++ptr, &ptr), &colBGM, cmap)) {
			ErrorWarning(gettext("Bad color specification for special resize cursor"));
			colorsIncluded = False;
		}
	    }

	    if (!colorsIncluded) {
		/* if not included, get default colors */
		colFGC.pixel = BlackPixel(dpy, DefaultScreen(dpy));
		XQueryColor(dpy, cmap, &colFGC);

		colBGC.pixel = WhitePixel(dpy, DefaultScreen(dpy));
		XQueryColor(dpy, cmap, &colBGC);

		colFGM = colFGC;
		colBGM = colBGC;
	    }

	    for (i = 0; i < 4; ++i) {
		XRecolorCursor(dpy, GRV.CornerPointers[i], &colFGC, &colBGC);
		XRecolorCursor(dpy, GRV.ResizePointers[i], &colFGM, &colBGM);
	    }
	}

	MemFree(our_copy);
    }
    else return False;
    return True;
}

/*
 *
 * ============================================================================
 * Entry Points
 */

void
InitCursors(dpy, scrInfo)
    Display *dpy;
    ScreenInfo *scrInfo;
{
Colormap cmap;
struct _cursor_data	*p;

    cmap = scrInfo->colormap;

    cursorTable = st_init_table(strcmp, cursorHash);

    for (p = cursor_names; p->name; p++)
	st_insert(cursorTable, (int) p->name, (char *) p->num);

    if (!initResizePointers(dpy, cmap))
	GRV.SpecialResizePointers = False;

    initOtherPointers(dpy, cmap);
}
