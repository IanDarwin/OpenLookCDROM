/* $XConsortium: ar.h,v 5.7 94/04/17 20:41:37 hersh Exp $ */
/*

Copyright (c) 1988-1991  X Consortium

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
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.


Copyright (c) 1988-1991 by Sun Microsystems, Inc. 
All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems
not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.


Solbourne Computer Inc.
Copyright (c) 1988 Solbourne.  USA  All rights reserved.

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Solbourne not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

SOLBOURNE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
SOLBOURNE BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

*/

#ifndef PHG_AR_H_INCLUDED
#define PHG_AR_H_INCLUDED

#ifndef DEBUG
#define TOCSIZE	    256
#else
#define TOCSIZE	    4
#endif /* DEBUG */

#ifdef vax			/* probably others, too */
#define PHG_AR_HOST_FLOAT_FORMAT    0x2
#else /* !vax */
#define PHG_AR_HOST_FLOAT_FORMAT    0x0
#endif /* vax */

/***### CHANGE THE FOLLOWING DEFINITION BASED ON YOUR ARCHITECTURE ###***/
#define MSBFIRST
#ifdef LSBFIRST
#define PHG_AR_HOST_BYTE_ORDER	    0x0
#else /* MSBFIRST */
#define PHG_AR_HOST_BYTE_ORDER	    0x1
#endif

/* Archive Element Opcodes */
#define PHG_AR_BAF	    0x1010	/* Begin Archive File */
#define PHG_AR_AFD	    0x1111	/* Archive File Descriptor */
#define PHG_AR_BSE	    0x1212	/* Begin Structure Element */
#define PHG_AR_ESE	    0x1313	/* End Structure Element */
#define PHG_AR_EOA	    0x1414	/* End Of Archive */
#define PHG_AR_AFS	    0x1515	/* Archive Free Space */
#define PHG_AR_AFI	    0x1616	/* Archive File Index element */

#define PHG_AR_STRUCT	    0x1		/* block contains structure */
#define PHG_AR_FREE_SPACE   0x2		/* block is free space */

#define PHG_AR_FOR_ALL_TOC_ENTRIES(_arh, _e)			    \
    {								    \
	Phg_ar_toc *_t;						    \
	int	    _l;						    \
	for (_t = (_arh)->toc; _t; _t = _t->next) {		    \
	    _l = 0;						    \
	    while (_l < _t->head.numUsed) {			    \
		if (_t->entry[_l].type == PHG_AR_FREE_SPACE)	    \
		    _l++;					    \
		else {						    \
		    _e = &_t->entry[_l++];
		

#define PHG_AR_END_FOR_ALL_TOC_ENTRIES }}}}

/* size of memory block increments for curpath.ers and counts.integers */
#define PHG_AR_TMPMEM_BLOCKSIZE        20             /* 20 elements */

#define PHG_AR_CHECK_TMPMEM_BLOCKSIZE(blockptr, blocktype, els_used)    \
    if ( (els_used) && !( (els_used)%PHG_AR_TMPMEM_BLOCKSIZE) ) {    \
        /* get more space */					    \
        (blockptr) = (blocktype *) realloc((char *)(blockptr),	    \
         (int)((els_used + PHG_AR_TMPMEM_BLOCKSIZE) * sizeof(blocktype))); \
        if (!(blockptr))					    \
            return(FALSE);                      /* out of memory */ \
    }
 

typedef enum {
    PHG_AR_WRITING_ARCHIVE,
    PHG_AR_READING_ARCHIVE
} Phg_ar_archiving_direction;

/* The archive file element definitions */
typedef struct {
    CARD16	opcode;		/* Always BAF */
    CARD8	length;
    CARD8	pad;
    /* List of char(length) */
} Phg_ar_begin_archive;

typedef struct {
    CARD16	opcode;		/* Always EOA */
    CARD8	pad[2];
} Phg_ar_end_archive;

typedef struct {
    CARD16	opcode;		/* Always BSE */
    CARD8	pad[2];
    INT32	id;
    CARD32	nelts;
    INT32	length;
    /* List of structure elements(nelts) */
} Phg_ar_begin_struct;

typedef struct {
    CARD16	opcode;		/* Always AFD */
    CARD8	format;
    CARD8	pad1;
    INT32	phigs_version;
    INT32	version;
    CARD16	length;
    CARD8	pad2[2];
    /* List of char(length) */
}  Phg_ar_descriptor;

typedef struct {
    CARD16	opcode;		/* Always AFS */
    CARD8	pad[2];
    INT32	length;
} Phg_ar_free_space;

typedef struct {		
    CARD16	opcode;		/* Always AFI */
    CARD8	pad[2];
    CARD16	numUsed;
    CARD16	numAvail;
    CARD32	nextpos;
    CARD32	length;
    /* List of Phg_ar_index_entry(numAvail) */
} Phg_ar_index;

typedef struct {
    CARD8	type;		/* 0x1=Struct, 0x2=FreeSpace */
    CARD8	pad[3];		/* Padding */
    CARD32	length;		/* Size of this block */
    CARD32	position;	/* Distance from beginning of file in bytes */
    INT32	str;		/* Structure id if used */
    CARD32	nelts;		/* Number of structure elements if struct */
} Phg_ar_index_entry;

/* The table of contents is a linked list of above two types */
typedef struct _Phg_ar_toc {
    Phg_ar_index	 head;
    Phg_ar_index_entry	*entry;	/* Entries for this block */
    struct _Phg_ar_toc	*next;
} Phg_ar_toc;

typedef struct _Ar_struct {
    char	 fname[MAXNAMLEN+1];
    Pint	 arid;
    Pint	 fd;		    /* file descriptor */
    Phg_ar_toc	*toc;		    /* table of contents */
    CARD8	 format;	    /* binary format */
    CARD32	 afiOffset;	   /* pos of 1st archive file index element */
    struct _Ar_struct	*next;
} Ar_struct;

extern Ar_handle
    phg_ar_open();

extern int
    phg_ar_close();

extern void
    phg_ar_delete_structs(),
    phg_ar_get_hierarchy(),
    phg_ar_get_struct_names();

extern Phg_ar_index_entry 
   *phg_ar_get_entry_from_archive();
    

#endif
