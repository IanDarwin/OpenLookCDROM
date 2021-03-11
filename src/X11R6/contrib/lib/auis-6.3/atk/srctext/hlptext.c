/* File hlptext.c created by R S Kemmetmueller
  
   hlptext, a Text subclass with dogtags for ATK. */
/* Copyright 1988, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsHeader[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/hlptext.c,v 1.12 1994/02/22 20:14:18 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>

#include "dogtags.h"
#include "hlptext.eh"

boolean hlptext__InitializeObject(classID, self)
struct classheader *classID;
struct hlptext *self;
{
    hlptext_SetCopyAsText(self,TRUE);
    return TRUE;
}

long hlptext__Read(self, file, id)
struct hlptext *self;
FILE *file;
long id;
{
    long tmpRetValue;

    tmpRetValue = super_Read(self, file, id);
    if (tmpRetValue == dataobject_NOREADERROR) {
	dogtags_substitute(self);
    }
    return tmpRetValue;
}
