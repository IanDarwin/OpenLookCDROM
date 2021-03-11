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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/basinit.c,v 2.8 1992/12/15 21:27:21 rr2b R6tape $";
#endif


 

#include <class.h>
#define class_StaticEntriesOnly

#include <im.ih>
#include <view.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <menulist.ih>
#include <keyrec.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <updlist.ih>
#include <filetype.ih>
#include <dataobj.ih>
#include <msghndlr.ih>
#include <message.ih>
#include <cursor.ih>
#include <event.ih>
#include <init.ih>
#include <environ.ih>
#include <bind.ih>
#include <atom.ih>
#include <atomlist.ih>
#include <namespc.ih>
#include <rm.ih>
#include <winsys.ih>

int basics_Initialize()
{
    im_StaticEntry;
    windowsystem_StaticEntry;
    view_StaticEntry;
    fontdesc_StaticEntry;
    graphic_StaticEntry;
    menulist_StaticEntry;
    keyrec_StaticEntry;
    keystate_StaticEntry;
    keymap_StaticEntry;
    observable_StaticEntry;
    proctable_StaticEntry;
    updatelist_StaticEntry;
    filetype_StaticEntry;
    dataobject_StaticEntry;
    msghandler_StaticEntry;
    message_StaticEntry;
    cursor_StaticEntry;
    event_StaticEntry;
    init_StaticEntry;
    environ_StaticEntry;
    bind_StaticEntry;
    atom_StaticEntry;
    atomlist_StaticEntry;
    namespace_StaticEntry;
    rm_StaticEntry;
    return 1;
}
