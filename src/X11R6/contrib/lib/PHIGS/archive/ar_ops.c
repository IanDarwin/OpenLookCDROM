/* $XConsortium: ar_ops.c,v 5.7 94/04/17 20:40:39 hersh Exp $ */

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

#include <sys/types.h>
#include <sys/stat.h>
#include "phg.h"
#include "cp.h"
#include "ws.h"
#include "ar.h"
#ifdef SYSV
#include <fcntl.h>
#endif /* SYSV */

/* parameters for LSEEK */
#ifndef L_SET
#define  L_SET       0  /* absolute offset */
#endif
#ifndef L_INCR
#define  L_INCR      1  /* relative to current offset */
#endif
#ifndef L_XTND
#define  L_XTND      2  /* relative to end of file */
#endif

static CARD32 ar_int_pad = 0x55555555;

#define WRITE_PAD(fd, length)            \
    (write(fd, (char *)&ar_int_pad, (int)PADDING(length)) != PADDING(length))

#define READ_PAD(fd, length)             \
    (read(fd, (char *)&ar_int_pad, (int)PADDING(length)) != PADDING(length))


/*****************************************************************
 * Begin Archive File (BAF) functions
 *****************************************************************/
int
phg_ar_write_baf(arh)
Ar_handle   arh;
{
    Phg_ar_begin_archive    begar;
    
    begar.opcode = PHG_AR_BAF;
    begar.length = strlen(arh->fname) > 255 ? 255 : strlen(arh->fname);
    
    if (write(arh->fd, (char *)&begar, sizeof(begar)) != sizeof(begar))
	return(1);
    
    if (write(arh->fd, arh->fname, (int)begar.length) != begar.length)
	return(1);
    
    if (WRITE_PAD(arh->fd, begar.length))
	return(1);
    
    return(0);
}

/* no format conversion necessary */
int
phg_ar_read_baf(arh)
Ar_handle   arh;
{
    char name[256];
    Phg_ar_begin_archive    begar;
    
    if (read(arh->fd, (char *)&begar, sizeof(begar)) != sizeof(begar))
	return(1);
	
    if (read(arh->fd, name, (int)begar.length) != begar.length)
	return(1);

    if (READ_PAD(arh->fd, (int)begar.length))
	return(1);

    return(0);
}

/*****************************************************************
 * Archive File Descriptor (AFD) functions
 *****************************************************************/
int
phg_ar_write_afd(arh)
Ar_handle   arh;
{
    int txtlen;
    Phg_ar_descriptor d;
    static char text[] = "PEX-SI Phigs Archive Version 1";
 
    /* Install correct set of format conversion routines */
    phg_ar_set_conversion( PHG_AR_HOST_BYTE_ORDER | PHG_AR_HOST_FLOAT_FORMAT,
			   (int)arh->format);
 
    txtlen    = strlen(text);
 
    d.opcode  = PHG_AR_AFD;
    d.format  = arh->format;
    d.phigs_version = 2;	/* PHIGS 88 and PHIGS PLUS */
    d.version = 1;
    d.length  = txtlen;
 
    /* Change binary format */
    phg_ar_convert_afd(&d);
 
    /* Write the afd element */
    if (write(arh->fd, (char *)&d, sizeof(d)) != sizeof(d)) 
	return(1);
	
    if (write(arh->fd, text, txtlen) != txtlen) 
	return(1);
	
    if (WRITE_PAD(arh->fd, txtlen)) 
	return(1);
 
    return(0);
}

int 
phg_ar_read_afd(arh)
Ar_handle   arh;
{
    char *text;
    Phg_ar_descriptor d;

    if ((read(arh->fd, (char *)&d, sizeof(d)) != sizeof(d)) || 
			(d.opcode != PHG_AR_AFD))
	return(1);
 
    arh->format = d.format;
 
    /* Install correct set of format conversion routines */
    phg_ar_set_conversion((int)d.format, PHG_AR_HOST_BYTE_ORDER |
					 PHG_AR_HOST_FLOAT_FORMAT);
 
    /* Convert to host format */
    phg_ar_convert_afd(&d);
 
    if (d.length != 0) {
    
        text = (char *)malloc(d.length+1);
 
        if (!text)
	    return(1);
 
        if (read(arh->fd, text, (int)d.length) != d.length) {
            free(text);
            return(1);
        }
 
        text[d.length] = '\0';
        free(text);
 
        if (READ_PAD(arh->fd, (int)d.length)) 
	    return(1);
    }
       
    return(0);
 
}


/*****************************************************************
 * Table of Contents (TOC) functions
 *****************************************************************/
 
static Phg_ar_toc *
create_and_insert_toc_element(arh)
Ar_handle arh;
{
    Phg_ar_toc		*toc = (Phg_ar_toc *)malloc(sizeof(Phg_ar_toc));
    register Phg_ar_toc	*trav;
    
    if (!toc)
	return((Phg_ar_toc *)NULL);
	
    toc->next = NULL;
    if (arh->toc) {
	for (trav = arh->toc; trav->next; trav = trav->next);
	trav->next = toc;
    } else
	arh->toc = toc;

    return(toc);
}

Phg_ar_toc *
phg_ar_init_toc(arh)
Ar_handle   arh;
{
    /** Create a new AFI element and add to table of contents **/
    Phg_ar_toc	*toc = create_and_insert_toc_element(arh);
    
    if (!toc) 
	return(NULL);

    toc->head.opcode   = PHG_AR_AFI;
    toc->head.length   = TOCSIZE * sizeof(Phg_ar_index_entry);
    toc->head.numUsed  = 0;
    toc->head.numAvail = TOCSIZE;
    toc->head.nextpos  = 0;
    
    toc->entry = (Phg_ar_index_entry *)malloc((unsigned)toc->head.length);
 
    if( !toc->entry ) return((Phg_ar_toc *)NULL);
 
    return(toc);
}


void
phg_ar_free_toc(arh)
Ar_handle   arh;
{
    register Phg_ar_toc *curr, *next;
 
    curr = arh->toc;
    while (curr) {
	if (curr->entry)
	    free((char *)(curr->entry));
	next = curr->next;
	free((char *)curr);
	curr = next;
    }
}


int 
phg_ar_read_toc(arh)
Ar_handle   arh;
{
    int fd;
    Phg_ar_toc *toc, *tmp;

    fd = arh->fd;
 
    /* Install correct set of format conversion routines */
    phg_ar_set_conversion((int)arh->format, PHG_AR_HOST_BYTE_ORDER |
					    PHG_AR_HOST_FLOAT_FORMAT);
 
    /* Save position of first AFI block */
    arh->afiOffset =  lseek(fd, (off_t)0, L_INCR);
 
    /* Read the first AFI block */
    if (!(toc = create_and_insert_toc_element(arh)))
	return(1);

    if (read(fd, (char *)&toc->head, sizeof(Phg_ar_index)) 
		!= sizeof(Phg_ar_index)) {
        phg_ar_free_toc(arh);
			return(1);
    }
       
    /* Convert to host format */
    phg_ar_convert_afi(&toc->head);
 
    if (!(toc->entry =(Phg_ar_index_entry *) malloc((unsigned)toc->head.length))
		|| (read(fd,(char *)toc->entry, (int)toc->head.length) != toc->head.length))
		{
        phg_ar_free_toc(arh);
			return(1);
    }
       
    /* Convert to host format */
    phg_ar_convert_afie((int)toc->head.numUsed, toc->entry);
 
    /* Read remaining AFI elements, chain them together in memory */
    while( toc->head.nextpos != 0 ) {
 
        /* Seek to next AFI element */
        (void)lseek(fd, (long)toc->head.nextpos, L_SET);
 
        /* Get memory for this block */
	if (!(tmp = create_and_insert_toc_element(arh)) ||
	     (read(fd, (char *)tmp, sizeof(Phg_ar_index)) != sizeof(Phg_ar_index))) {
            phg_ar_free_toc(arh);          /* Free this archive */
	    return(1);
        }
 
        /* Convert to host format */
        phg_ar_convert_afi(&tmp->head);
 
        tmp->entry = (Phg_ar_index_entry *) malloc((unsigned)tmp->head.length);
 
        if (!tmp->entry ||
	    (read(fd,(char *)tmp->entry,(int)tmp->head.length) != tmp->head.length)) {
            phg_ar_free_toc(arh);
	    return(1);
        }
 
        /* Convert to host format */
        phg_ar_convert_afie((int)tmp->head.numUsed, tmp->entry);
 
        toc = tmp;
    }

    return(0);
}

int 
phg_ar_write_toc(arh)
Ar_handle   arh;
{
    Phg_ar_toc	 *toc;
    int		  convert;  /* True, archive file format conv required */
    Phg_ar_index  afi;
    Phg_ar_index *afiptr;
    Phg_ar_index_entry *entries;
 
    /* Set a flag indicating whether format conversion is necessary */
    convert = arh->format != (PHG_AR_HOST_BYTE_ORDER | 
			      PHG_AR_HOST_FLOAT_FORMAT);
 
    /* Install correct set of format conversion routines */
    if (convert)
        phg_ar_set_conversion(PHG_AR_HOST_BYTE_ORDER | 
			      PHG_AR_HOST_FLOAT_FORMAT, (int)arh->format);
 
    /* Seek to the first toc block */
    if (lseek(arh->fd, (long)arh->afiOffset, L_SET) != arh->afiOffset )
	return(1);
 
    /* Write each toc block */
    for (toc = arh->toc; toc != NULL; toc = toc->next) {
 
        if (convert) {
            afi = toc->head;
            afiptr = &afi;
            phg_ar_convert_afi(afiptr);
 
            entries = (Phg_ar_index_entry *)malloc((unsigned)toc->head.length);
	    
            if (!entries ) return(1);
	    
	    bcopy( (char *)(toc->entry), (char *)(entries),
		   (int)(toc->head.length));

            phg_ar_convert_afie((int)toc->head.numAvail, entries);
        } else {
            afiptr  = &toc->head;
            entries = toc->entry;
        }
 
        if (write(arh->fd, (char *)afiptr, sizeof(Phg_ar_index)) != 
		    sizeof(Phg_ar_index) )
            return(1);
 
        if (write(arh->fd, (char *)entries, (int)toc->head.length) 
				!= toc->head.length )
            return(1);
 
        /* Free any allocated space */
        if (convert)
            free((char *)entries);
 
        /* Seek to next afi element */
        if (toc->head.nextpos != 0 ) {
            if (lseek(arh->fd, (long)toc->head.nextpos, L_SET) !=
				     toc->head.nextpos)
                return(1);
        }
    }
       
    return(0);
 
}

/*****************************************************************
 * End of Archive (EOA) element functions
 *****************************************************************/
int
phg_ar_read_eoa(arh)
Ar_handle     arh;
{ 
    int nbytes;
    int fd = arh->fd;
    Phg_ar_end_archive    endar;
 
    /* Find EOA element position */
    nbytes = lseek(fd, (off_t)-4, L_XTND);
 
    /* Insure the last element is EOA */
    if (read(fd, (char *)&endar, sizeof(endar)) != sizeof(endar))
	return(1);

    if (endar.opcode != PHG_AR_EOA) 
	return(1);
 
    /* Remove the EOA element.  This is written by pclosearfile. */

#if defined(UTEKV) || (defined(SYSV) && defined(SYSV386))
    /* Losing machines running System V Release 3.2 don't define
       the function ftruncate(), although both BSD and System V Release 4
       systems do.  Thus we take a more twisted approach on such systems
     */
    {
#define CHUNKSIZE 1024
      
        char            tmp_file[80];
	int             new_fd, bytes_left, i;
	unsigned char   buffer[CHUNKSIZE];
	struct stat     stat_val;

	sprintf(tmp_file, "pex-si.archive.%d", getpid());
	(void) unlink(tmp_file);
	new_fd = open(tmp_file, O_RDWR | O_CREAT);
	lseek(fd, (off_t)0, L_SET);
	
	for (i = 0; i < nbytes / CHUNKSIZE; i++) {
	  
	  if (read(fd, buffer, CHUNKSIZE) != CHUNKSIZE)
	    return(1);
	  else if (write(new_fd, buffer, CHUNKSIZE) != CHUNKSIZE)
	    return(1);
	  
	}

	bytes_left = nbytes % CHUNKSIZE;
	if (read(fd, buffer, bytes_left) != bytes_left)
	  return (1);
	else if (write(new_fd, buffer, bytes_left) != bytes_left)
	  return (1);

	/* Get the mode on the old file and set the new file to be of that
	   mode */
	fstat(fd, &stat_val);
#ifdef UTEKV
	fchmod(new_fd, stat_val.st_mode);
#else
	chmod(tmp_file, stat_val.st_mode);
#endif

	close(new_fd);
	close(fd);

	(void)unlink(arh->fname); /* remove original */
	if (rename(tmp_file, arh->fname) != 0) /* rename new file to be orig */
	  return(1);

	arh->fd = open(arh->fname, O_RDWR); /* now reopen the file and... */
	lseek(fd, (off_t)0, L_XTND);	            /* ...goto the end of the file */
    }
       
#else /* !losing */

    /* This is the way it should be !! */
    if (ftruncate(fd, (long)nbytes)) 
	return(1);
    
#endif /* losing */

    return(0);
}

int
phg_ar_write_eoa(fd)
int fd;
{
    Phg_ar_end_archive    endar;
    
    /* Find end of file */
    (void) lseek(fd, (off_t)0, L_XTND);
 
    endar.opcode = PHG_AR_EOA;
    
    if (write(fd, (char *)&endar, sizeof(endar)) != sizeof(endar))
	return(1);
 
    return(0);
}

Phg_ar_index_entry *
phg_ar_get_entry_from_archive(arh, struct_id)
Ar_handle   arh;
Pint	    struct_id;
{
    Phg_ar_index_entry	*entry;
    
    PHG_AR_FOR_ALL_TOC_ENTRIES(arh, entry)
	if (entry->str == struct_id)
	    return (entry);
    PHG_AR_END_FOR_ALL_TOC_ENTRIES
    
    return (NULL);
}

int 
phg_ar_read_struct_from_archive(arh, entry, mem)
Ar_handle	    arh;
Phg_ar_index_entry *entry;
caddr_t		    mem;
{
    Phg_ar_begin_struct begstr;
 
    /* Install correct set of format conversion routines */
    phg_ar_set_conversion((int)arh->format, PHG_AR_HOST_FLOAT_FORMAT |
					   PHG_AR_HOST_BYTE_ORDER);
 
    /* Seek to BSE */
    if (lseek(arh->fd, (off_t)entry->position, L_SET) != entry->position )
        return(1);
 
    /* Read BSE */
    if (read(arh->fd, (char *)&begstr, sizeof(begstr)) != sizeof(begstr) )
        return(1);
 
    phg_ar_convert_bse(&begstr);
 
    /* read this structure */
    if (read(arh->fd, mem, (int)begstr.length) != begstr.length )
        return(1);
 
    /* Convert to host format */
    phg_ar_convert_elements((int)entry->nelts, mem, PHG_AR_READING_ARCHIVE);
 
    return(0);
}


/* Updates a free block on disk.  No error checking since this block
 * already exists. */ 
static void 
update_block(arh, entry)
Ar_handle	     arh;
Phg_ar_index_entry  *entry;
{
    int		      fd;
    Phg_ar_free_space f;
 
    fd = arh->fd;
    f.opcode = PHG_AR_AFS;
    f.length = entry->length - 8;   /* Number of bytes to next element */
 
    /* Convert to archive binary format */
    phg_ar_set_conversion(PHG_AR_HOST_BYTE_ORDER + PHG_AR_HOST_FLOAT_FORMAT, 
			  (int)arh->format);
    phg_ar_convert_afs(&f);
 
    /* Seek to this blocks position */
    (void) lseek(fd, (off_t)entry->position, L_SET);
 
    /* Update the block */
    (void) write(fd, (char *)&f, sizeof(f));
 
    return;
}

/* turns a structure entry into free space */
void
phg_ar_free_entry(arh, entry)
Ar_handle	    arh;
Phg_ar_index_entry *entry;
{
    entry->type = PHG_AR_FREE_SPACE;
 
    update_block(arh, entry);
}

/* Return a previously unused toc entry.  May have to add new AFI element. */
static Phg_ar_index_entry *
new_entry(arh)
Ar_handle arh;
{
    Phg_ar_toc *toc, *lastToc;
 
    /* Search through toc find an unused toc entry. */
    lastToc = NULL;
    for (toc = arh->toc; toc != NULL; toc = toc->next) {
 
        if( toc->head.numUsed < toc->head.numAvail ) {
            toc->head.numUsed++;
            return( &toc->entry[toc->head.numUsed-1] );
        }

        lastToc = toc;
    }
       
 
    /* This code handles adding an AFI element.
     * It writes the newly init'd element to insure there is space for it.
     * This element will be overwritten at file close with the correct
     * binary format.
     */
    toc = phg_ar_init_toc(arh);
    lastToc->head.nextpos = lseek(arh->fd, (off_t)0, L_XTND);   /* End of file */

    /* Ensure space for AFI element */
    if( write(arh->fd, (char *)&(toc->head), sizeof(Phg_ar_index)) != 
				     sizeof(Phg_ar_index))
        return(NULL);
 
    if( write(arh->fd, (char *)toc->entry, (int)toc->head.length) 
		!= toc->head.length)
        return(NULL);
	
    toc->head.numUsed = 1;
    return( &toc->entry[0] );
}


/* Returns an entry of size nbytes.  This may add a new entry, or
 * split an existing Free Space entry.  The returned entry has
 * a type of FreeSpace, its length is nbytes, and its position
 * has been set.
 */
static Phg_ar_index_entry *
get_entry(arh, nbytes)
Ar_handle arh;
Pint nbytes;
{
    int i;
    Phg_ar_toc	       *toc;
    Phg_ar_index_entry *entry;
    Phg_ar_index_entry *new;
 
    /* There are numAvail in the table of contents.
     * numUsed of these entries have Structures or Free Space stored in them.
     * We use the first Free Space entry which is at least nbytes long.
     * If we don't find one this size, we create a new entry of size nbytes.
     */
 
    /* Search through toc, if we find a suitable free block, return it. */
    for (toc = arh->toc; toc != NULL; toc = toc->next) {
 
        for (i = 0; i < toc->head.numUsed; i++) {
 
            /* See if we can use a FreeSpace element */
            if (toc->entry[i].type == PHG_AR_FREE_SPACE ) {
 
                /* If we're lucky enough to have the right size, return it */
                if (nbytes == toc->entry[i].length) {
                    return(&toc->entry[i]);
                }
 
                /* Attempt to split an existing Free Space Element */
                if (nbytes + sizeof(Phg_ar_free_space) <= toc->entry[i].length) {
                    if (!(new = new_entry(arh)))
			return(NULL);
                    new->type = PHG_AR_FREE_SPACE;
                    new->length = toc->entry[i].length - nbytes;
                    new->position = toc->entry[i].position + nbytes;
                    update_block(arh, new);
 
                    toc->entry[i].length = nbytes;
                    update_block(arh, &toc->entry[i]);
                    return(&toc->entry[i]);
                }
            }
        }
    }  
       
    /* Add a new entry to Table of Contents */
    if (!(entry = new_entry(arh)))
	return(NULL);
	
    entry->type      = PHG_AR_FREE_SPACE;
    entry->length    = nbytes;
    entry->position  = lseek(arh->fd, (off_t)0, L_XTND);    /* End of file */
    
    return(entry);
}
 
int 
phg_ar_write_struct_to_archive(arh, str, res_flag, nbytes, nelts, mem)
Ar_handle   arh;
Pint	    str;
Pconf_res    res_flag;
Pint	    nbytes;
Pint	    nelts;
caddr_t	    mem;
{
    int			 defsize;
    Phg_ar_index_entry	*entry;
    Phg_ar_begin_struct	 begstr;
    CARD32		 endstr = PHG_AR_ESE << 16;
 
    entry = phg_ar_get_entry_from_archive(arh, str);
 
    /* Calcualte total size of structure definition */
    defsize = nbytes + sizeof(Phg_ar_begin_struct) + sizeof(CARD32);
 
    /* Return if mode is maintain, PRES_ABANDON mode has already been checked */
    if (entry && res_flag == PRES_MAINTAIN ) {
        return(0);
    }
       
    /* write this structure */
    if (entry && defsize == entry->length ) 
	{   /* write to existing block */
        entry->nelts  = nelts;
    }
    else {					    /* write to new block */
        if (entry) phg_ar_free_entry(arh, entry);   /* Free old entry */
        entry = get_entry(arh, defsize);            /* Get new entry */
        if (!entry ) return(1);
        entry->type      = PHG_AR_STRUCT;
        entry->str       = str;
        entry->nelts     = nelts;
    }
       
    /* Install correct set of format conversion routines */
    phg_ar_set_conversion(PHG_AR_HOST_FLOAT_FORMAT | PHG_AR_HOST_BYTE_ORDER, 
				(int)arh->format);
     
    /* Seek to str block */
    (void) lseek(arh->fd, (off_t)entry->position, L_SET);
 
    begstr.opcode = PHG_AR_BSE;
    begstr.length = nbytes;
    begstr.id     = str;
    begstr.nelts  = nelts;
 
    phg_ar_convert_bse(&begstr);
    
    if (write(arh->fd, (char *)&begstr, sizeof(begstr)) != sizeof(begstr) )
        return(1);
 
    /* convert to archive format */
    phg_ar_convert_elements((int)entry->nelts, mem, PHG_AR_WRITING_ARCHIVE);

    if (write(arh->fd, mem, nbytes) != nbytes )
        return(1);
 
    if (write(arh->fd, (char *)&endstr, sizeof(endstr)) != sizeof(endstr) )
        return(1);
 
    return(0);
}
