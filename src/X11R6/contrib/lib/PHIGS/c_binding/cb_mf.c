/* $XConsortium: cb_mf.c,v 5.2 94/04/17 20:40:50 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/


#include "phg.h"
#include "cp.h"
#include "cb_priv.h"

void
pwrite_item(ws, type, length, record)
Pint		ws;		/* workstation identifier	*/
Pint		type;		/* item type	*/
Pint		length;		/* item data record length	*/
Pitem_data	*record;	/* item data record	*/
{
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_write_item))
	/* NOT IMPLEMENTED */
	ERR_REPORT( phg_cur_cph->erh, ERRN152);
}

void
pget_item_type(ws, type, length)
Pint	ws;		/* workstation identifier	*/
Pint	*type;		/* OUT item type	*/
Pint	*length;	/* OUT item data record length	*/
{
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_get_item_type))
	/* NOT IMPLEMENTED */
	ERR_REPORT( phg_cur_cph->erh, ERRN152);
}

void
pread_item(ws, max_length, record)
Pint		ws;		/* workstation identifier	*/
Pint		max_length;	/* max item data record length	*/
Pitem_data	*record;	/* OUT item data record	*/
{
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR3, Pfn_read_item))
	/* NOT IMPLEMENTED */
	ERR_REPORT( phg_cur_cph->erh, ERRN152);
}

void
pinterpret_item    (type, length, record)
Pint		type;		/* item type	*/
Pint		length;		/* item data record length	*/
Pitem_data	*record;	/* item data record	*/
{
    if (CB_ENTRY_CHECK( phg_cur_cph, ERR2, Pfn_interpret_item))
	/* NOT IMPLEMENTED */
	ERR_REPORT( phg_cur_cph->erh, ERRN152);
}
