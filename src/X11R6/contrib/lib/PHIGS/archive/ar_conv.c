/* $XConsortium: ar_conv.c,v 5.5 94/04/17 20:40:36 hersh Exp $ */

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

#include <stdio.h>
#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "ar.h"
#include "PEXprotost.h"
#include "PEXproto.h"
#include "PEX.h"
#include "pexSwap.h"


/* For in place conversions which happen in this file */
#define CONVERT_CARD32(swp, val) \
    if ((swp)->ConvertCARD32)  ((*(swp)->ConvertCARD32)(&(val))) 

#define CONVERT_CARD16(swp, val) \
    if ((swp)->ConvertCARD16)  ((*(swp)->ConvertCARD16)(&(val))) 
    
    
static pexSwap   clientSwapStructure;	
static pexSwap  *swp = &clientSwapStructure;

static int fromFormat;
static int toFormat;

extern void	ConvertIEEEToVax();
extern void	SwapIEEEToVax();
extern void	ConvertVaxToIEEE();
extern void	SwapVaxToIEEE();
extern void	SwapCARD16();
extern void	SwapCARD32();

typedef struct {
	void (*s)();		/* Function to convert a short */
	void (*l)();		/* Function to convert a long */
	void  (*f)();		/* Function to convert a float */
} ThreeFuncs;

static ThreeFuncs ConversionFunction[4][4] = {
	{   /* From Big Endian Ieee */
	    { 0, 0, 0 },
	    { SwapCARD16, SwapCARD32, (void (*)())SwapCARD32 },
	    { 0, 0, ConvertIEEEToVax },
	    { SwapCARD16, SwapCARD32, SwapIEEEToVax }
	},
	{
	    /* From Big Endian DecF */
	    { SwapCARD16, SwapCARD32, (void (*)())SwapCARD32 },
	    { 0, 0, 0 },
	    { SwapCARD16, SwapCARD32, SwapIEEEToVax },
	    { 0, 0, ConvertIEEEToVax }
	},
	{
	    /* From Little Endian Ieee */
	    { 0, 0, ConvertVaxToIEEE },
	    { SwapCARD16, SwapCARD32, SwapVaxToIEEE },
	    { 0, 0, 0 },
	    { SwapCARD16, SwapCARD32, (void (*)())SwapCARD32 }
	},
	{
	    /* From Little Endian DecF */
	    { SwapCARD16, SwapCARD32, SwapVaxToIEEE },
	    { 0, 0, ConvertVaxToIEEE },
	    { SwapCARD16, SwapCARD32, (void (*)())SwapCARD32 },
	    { 0, 0, 0 }
	}
};


void
phg_ar_set_conversion(from, to)
int from;
int to;
{
    toFormat		    = to;
    fromFormat		    = from;
    
    swp->ConvertCARD16	    = ConversionFunction[from][to].s;
    swp->ConvertCARD32	    = ConversionFunction[from][to].l;
    swp->ConvertFLOAT	    = ConversionFunction[from][to].f;
}


/* Convert Archive File Descriptor */
phg_ar_convert_afd(d)
Phg_ar_descriptor *d;
{
    d->opcode	= PHG_AR_AFD;
    CONVERT_CARD32(swp, d->phigs_version);
    CONVERT_CARD32(swp, d->version);
    CONVERT_CARD16(swp, d->length);
}


phg_ar_convert_bse(b)
Phg_ar_begin_struct *b;
{
    b->opcode	= PHG_AR_BSE;
    CONVERT_CARD32(swp, b->id);
    CONVERT_CARD32(swp, b->nelts);
    CONVERT_CARD32(swp, b->length);
}


phg_ar_convert_afs(f)
Phg_ar_free_space *f;
{
    f->opcode	= PHG_AR_AFS;
    CONVERT_CARD32(swp, f->length);
}


phg_ar_convert_afi(index)
Phg_ar_index *index;
{
    index->opcode   = PHG_AR_AFI;
    CONVERT_CARD16(swp, index->numUsed);
    CONVERT_CARD16(swp, index->numAvail);
    CONVERT_CARD32(swp, index->nextpos);
    CONVERT_CARD32(swp, index->length);
}


phg_ar_convert_afie(n, e)
int n;
Phg_ar_index_entry *e;
{
    int i;

    for (i = 0; i < n; i++) {
    	CONVERT_CARD32(swp, (e[i].length));
    	CONVERT_CARD32(swp, (e[i].position));
    	CONVERT_CARD32(swp, (e[i].str));
    	CONVERT_CARD32(swp, (e[i].nelts));
    }
}

void
phg_ar_convert_elements(nelts, buffer, direction)
int   			     nelts;
char			    *buffer;
Phg_ar_archiving_direction   direction;
{
    register char      *ptr = buffer;
    pexElementInfo     *head;
    register int	command;
    register CARD16	type;
    CARD16		length;
    void	      (*localswapshort)();
    extern OCFunction	cPEXOutputCmd[];
    extern OCFunction	uPEXOutputCmd[];

    /* if the formats are there are no conversions that will need to be
     * performed, then just return */
     
    if (!(swp->ConvertCARD16) && !(swp->ConvertCARD32) && !(swp->ConvertFLOAT))
	return;
	

    localswapshort = ConversionFunction[fromFormat][PHG_AR_HOST_BYTE_ORDER |
						  PHG_AR_HOST_FLOAT_FORMAT].s;

    /* For each element in the structure */
    for (command = 0; command < nelts; command++) {
    
	head = (pexElementInfo *)ptr;

	if (direction == PHG_AR_READING_ARCHIVE) {
	
	    /* we are reading fram an archive so we must 'decode' */
	    if (localswapshort != NULL) {
		(*localswapshort)(head->elementType);
		type = head->elementType;
		(*localswapshort)(head->length);
		length = head->length;
	    } else {
		type = head->elementType;
		length = head->length;
	    }
	    (*cPEXOutputCmd[type])(swp, ptr);
	    
	} else {
	
	    /* we are writing to an archive, so we must 'encode' */
	    type = head->elementType;
	    length = head->length;
	    (*uPEXOutputCmd[type])(swp, ptr);
	    
	}
	
	ptr += length * sizeof(CARD32);

    }
    
    return;

}

