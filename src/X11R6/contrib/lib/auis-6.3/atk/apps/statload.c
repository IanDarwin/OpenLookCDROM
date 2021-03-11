/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/apps/RCS/statload.c,v 2.25 1993/12/08 23:11:50 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#define STATICLOADTYPESCRIPT 1
#define class_StaticEntriesOnly
#ifdef WM_ENV
#include <wfontd.ih>
#include <wgraphic.ih>
#include <wim.ih>
#include <wcursor.ih>
#include <wws.ih>
#include <mrl.ih>
#endif /* WM_ENV */
#ifdef X11_ENV
#include <X11/X.h>
#include <X11/Xlib.h>
#include <cmenu.h>
#include <xcolor.ih>
#include <xcmap.ih>
#include <xfontd.ih>
#include <xgraphic.ih>
#include <xim.ih>
#include <xcursor.ih>
#include <xws.ih>
#include <region.ih>
#endif
#include <updlist.ih>
#include <keyrec.ih>
#include <proctbl.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <menulist.ih>
#include <fontdesc.ih>
#include <im.ih>
#include <traced.ih>
#include <owatch.ih>
#include <image.ih>
#include <color.ih>
#include <cmap.ih>
#include <graphic.ih>
#include <buffer.ih>
#include <bufferlist.ih>
#include <mark.ih>
#include <nstdmark.ih>
#include <rectlist.ih>
#include <text.ih>
#include <tabs.ih>
#include <envrment.ih>
#include <tree23.ih>
#include <stylesht.ih>
#include <style.ih>
#include <textv.ih>
#include <framev.ih>
#include <msghndlr.ih>
#include <message.ih>
#include <framemsg.ih>
#include <cursor.ih>
#include <filetype.ih>
#include <frame.ih>
#include <framecmd.ih>
#include <scroll.ih>
#include <event.ih>
#include <search.ih>
#include <init.ih>
#include <environ.ih>
#include <dict.ih>
#include <viewref.ih>
#include <bind.ih>
#include <messitem.ih>
#include <complete.ih>
#include <atom.ih>
#include <atomlist.ih>
#include <namespc.ih>
#include <rm.ih>
#include <app.ih>
#include <eza.ih>
#include <helptxtv.ih>
#include <matte.ih>
#include <lpair.ih>
#include <bpair.ih>
#include <label.ih>
#include <labelv.ih>
#include <strinput.ih>
#include <path.ih>

#ifdef STATICLOADTYPESCRIPT
#include <tscripta.ih>
#include <tscript.ih>
#include <typetext.ih>
#endif /* STATICLOADTYPESCRIPT */
#include <sbutton.ih>
#include <sbuttonv.ih>
#include <sbttnav.ih>
#include <dialog.ih>
#include <dialogv.ih>

#undef class_StaticEntriesOnly

doStaticLoads()
{
#ifdef WM_ENV
    wmfontdesc_StaticEntry;
    wmgraphic_StaticEntry;
    wmim_StaticEntry;
    wmcursor_StaticEntry;
    wmws_StaticEntry;
    mrl_StaticEntry;
#endif /* WM_ENV */
#ifdef X11_ENV
    xcolor_StaticEntry;
    xcolormap_StaticEntry;
    xfontdesc_StaticEntry;
    xgraphic_StaticEntry;
    xim_StaticEntry;
    xcursor_StaticEntry;
    xws_StaticEntry;
    region_StaticEntry;
#endif /* X11_ENV */
    traced_StaticEntry;
    owatch_StaticEntry;
    image_StaticEntry;
    color_StaticEntry;
    colormap_StaticEntry;
    graphic_StaticEntry;
    updatelist_StaticEntry;
    keyrec_StaticEntry;
    proctable_StaticEntry;
    keystate_StaticEntry;
    keymap_StaticEntry;
    menulist_StaticEntry;
    fontdesc_StaticEntry;
    im_StaticEntry;
    buffer_StaticEntry;
    bufferlist_StaticEntry;
    mark_StaticEntry;
    nestedmark_StaticEntry;
    rectlist_StaticEntry;
    text_StaticEntry;
    tabs_StaticEntry;
    environment_StaticEntry;
    tree23int_StaticEntry;
    stylesheet_StaticEntry;
    style_StaticEntry;
    textview_StaticEntry;
    frameview_StaticEntry;
    msghandler_StaticEntry;
    message_StaticEntry;
    framemessage_StaticEntry;
    cursor_StaticEntry;
    filetype_StaticEntry;
    frame_StaticEntry;
    framecmds_StaticEntry;
    scroll_StaticEntry;
    event_StaticEntry;
    search_StaticEntry;
    init_StaticEntry;
    environ_StaticEntry;
    dictionary_StaticEntry;
    viewref_StaticEntry;
    bind_StaticEntry;
    messitem_StaticEntry;
    completion_StaticEntry;
    atom_StaticEntry;
    atomlist_StaticEntry;
    namespace_StaticEntry;
    rm_StaticEntry;
    helptextview_StaticEntry;
    matte_StaticEntry;
    lpair_StaticEntry;
    bpair_StaticEntry;
    label_StaticEntry;
    labelview_StaticEntry;
    strinput_StaticEntry;
#ifdef STATICLOADTYPESCRIPT
    typescriptapp_StaticEntry;
    typescript_StaticEntry;
    typetext_StaticEntry;
#endif /* STATICLOADTYPESCRIPT */

    application_StaticEntry;
    ezapp_StaticEntry;
    path_StaticEntry;
    sbutton_StaticEntry;
    sbuttonv_StaticEntry;
    sbttnav_StaticEntry;
    dialog_StaticEntry;
    dialogv_StaticEntry;

}
