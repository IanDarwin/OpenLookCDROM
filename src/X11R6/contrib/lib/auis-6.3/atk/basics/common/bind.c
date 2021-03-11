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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/bind.c,v 2.14 1993/12/08 16:41:26 rr2b Exp $";
#endif


 

/* bind.c
 */

/*  */

#include <class.h>
#include <keymap.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.eh>

void bind__BindList(classID, bl, km, ml, type)
struct classheader *classID;
struct bind_Description *bl;
struct keymap *km;
struct menulist *ml;
struct classinfo *type;
{
    while(bl && (bl->procName || bl->keyVector || bl->menuEntry)) {
	struct proctable_Entry *pe;

	if(bl->procName)
	    pe = proctable_DefineProc(bl->procName, (procedure) bl->proc, type, bl->module, bl->doc);
	else
	    pe = NULL;

	if(km && bl->keyVector)
	    keymap_BindToKey(km, bl->keyVector, pe, bl->keyRock);
	if(ml && bl->menuEntry)
	    menulist_AddToML(ml, bl->menuEntry, pe, bl->menuRock, bl->menuMask);

	bl++;
    }
}
