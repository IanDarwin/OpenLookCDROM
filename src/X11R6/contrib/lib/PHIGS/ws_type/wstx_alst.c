/* $XConsortium: wstx_alst.c,v 5.5 94/04/17 20:42:36 rws Exp $ */

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


/* Code from the attr package: attr.c, etc. */
#include <stdio.h>
#include <sys/types.h>
#include "attr.h"
#include <X11/Xfuncs.h>
#if NeedVarargsPrototypes
# include <stdarg.h>
# define Va_start(a,b) va_start(a,b)
#else
# include <varargs.h>
# define Va_start(a,b) va_start(a)
#endif

/* NON_PORTABLE means that the var-args list is treated
 * as an avlist.  This is known to work for Sun1/2/3/4.
 * If you want to live dangerously, define NON_PORTABLE.
 */
/* #define	NON_PORTABLE */

/* size of an attribute */
#define	PHG_ATTR_SIZE	(sizeof(caddr_t))

#ifndef LINT_CAST
#ifdef lint
#define LINT_CAST(arg)  (arg ? 0 : 0)
#else
#define LINT_CAST(arg)  (arg)
#endif /* lint */
#endif /* LINT_CAST */

/* package private routines */
#ifdef NON_PORTABLE
extern Phg_attr_avlist	phg_attr_copy_avlist();
#else
extern Phg_attr_avlist	phg_attr_copy_valist();
#endif
extern void		phg_attr_check_pkg();
extern int		phg_attr_count_avlist();

/* Note that changes to the basic attribute-value traversal
 * should also be made in attr_portable.c for portable implmentations.
 */


/* phg_attr_create creates an avlist from the VARARGS passed
 * on the stack.
 */
#if NeedVarargsPrototypes
Phg_attr_avlist
phg_attr_create(caddr_t *listhead, int listlen, ...)
#else
/*VARARGS2*/
Phg_attr_avlist
phg_attr_create(listhead, listlen, va_alist)
caddr_t	*listhead; 
int	 listlen; 
va_dcl
#endif
{
   Phg_attr_avlist	avlist;
   va_list	valist;

   Va_start(valist,listlen);
   avlist = phg_attr_make(listhead, listlen, valist);
   va_end(valist);
   return avlist;
}


/* attr_make copies the attribute-value list pointed to by valist to
 * the storage pointed to by listhead, or some new storage if that is
 * null.  If listhead is not null, then the list must be less than or equal
 * to listlen PHG_ATTR_SIZE byte chunks; if not 0 is returned.
 * The count of the avlist is returned in *count_ptr, if count_ptr is
 * not NULL.
 */
Phg_attr_avlist
phg_attr_make_count(listhead, listlen, valist, count_ptr) 
Phg_attr_avlist	listhead; 
int	 	listlen; 
va_list		valist;
int		*count_ptr;
{
   unsigned int 	count;
   extern char	*malloc();
   
#ifdef NON_PORTABLE
   count = (unsigned int) (LINT_CAST(phg_attr_count((Phg_attr_avlist) valist)));
#else
   count = (unsigned int) (LINT_CAST(valist_count(valist)));
#endif
   /* if the user supplied storage space for attributes is not big enough
    * for the attributes in the attribute list, then exit!
    */
   if (listhead)  {
	if (count > listlen)  {
            printf("Number of attributes(%d) in the attr list exceeds\n",count);
	    printf("the maximum number(%d) specified.  Exit!\n",listlen);
	    exit(1);
	}
   }    else   {
      listhead = (Phg_attr_avlist) (LINT_CAST(malloc((PHG_ATTR_SIZE * count)+1)));
   }
#ifdef NON_PORTABLE
   (void) phg_attr_copy_avlist(listhead, (Phg_attr_avlist) (LINT_CAST(valist))); 
#else
   (void) phg_attr_copy_valist(listhead, valist); 
#endif

   if (count_ptr)
      *count_ptr = count;
   return(listhead);
}


#ifdef NON_PORTABLE

#define	avlist_get(avlist)	*(avlist)++

/* Copy count elements from list to dest.
 * Advance both list and dest to the next element after
 * the last one copied.
 */
#define	avlist_copy(avlist, dest, count)	\
    { \
        bcopy((char *) avlist, (char *) dest, (int)(count * PHG_ATTR_SIZE)); \
        avlist += count; \
        dest += count; \
    }


/* A macro to copy attribute values 
 * count is the number of caddr_t size chunks to copy.
 */
#define	avlist_copy_values(avlist, dest, count) \
    if (count == 1) \
        *dest++ = avlist_get(avlist); \
    else { \
	avlist_copy(avlist, dest, count); \
    }


/* phg_attr_copy_avlist copies the attribute-value list from 
 * avlist to dest.  Recursive lists are collapsed into dest.
 */
 
Phg_attr_avlist
phg_attr_copy_avlist(dest, avlist) 
register Phg_attr_avlist	dest;
register Phg_attr_avlist	avlist;
{
   register Phg_attr_attribute	attr;
   register unsigned int		cardinality;
   
   while (attr = (Phg_attr_attribute) avlist_get(avlist)) {
       cardinality = PHG_ATTR_CARDINALITY(attr);
       switch(PHG_ATTR_LIST_TYPE(attr)) {
           case PHG_ATTR_NONE:	/* not a list */
               *dest++ = (caddr_t) attr;
               avlist_copy_values(avlist, dest, cardinality);
               break;
               
           case PHG_ATTR_NULL:	/* null terminated list */
               *dest++ = (caddr_t) attr;
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE:
                       /* Note that this only checks the first four bytes
                        * for the null termination.
			* Copy each value element until we have copied the
			* null termination.
                        */
		       do {
		           avlist_copy_values(avlist, dest, cardinality);
		       } while (*(dest - 1));
                       break;
                       
                   case PHG_ATTR_LIST_IS_PTR:
                       *dest++ = avlist_get(avlist);
                       break;
               }
               break;
               
            case PHG_ATTR_COUNTED:		/* counted list */
               *dest++ = (caddr_t) attr;
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE: {
			   register unsigned int	count;

			   *dest = avlist_get(avlist);	/* copy the count */
			    count = ((unsigned int) *dest++) * cardinality;
			    avlist_copy_values(avlist, dest, count);
			}
                        break;
                        
                   case PHG_ATTR_LIST_IS_PTR:
                       *dest++ = avlist_get(avlist);
                       break;
               }
               break;
               
            case PHG_ATTR_RECURSIVE:	/* recursive attribute-value list */
                if (cardinality != 0)	/* don't strip it */
                    *dest++ = (caddr_t) attr;
                    
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE:
                       dest = phg_attr_copy_avlist(dest, avlist);
                       if (cardinality != 0)	/* don't strip it */
                           dest++;	/* move past the null terminator */
                       avlist = phg_attr_skip(attr, avlist);
                       break;
                       
                   case PHG_ATTR_LIST_IS_PTR:
                       if (cardinality != 0)	/* don't collapse inline */
                           *dest++ = avlist_get(avlist);
                       else {
			   Phg_attr_avlist new_avlist = (Phg_attr_avlist) 
			   	(LINT_CAST(avlist_get(avlist)));
			   if (new_avlist)
			       /* Copy the list inline -- don't 
				* move past the null termintor.
				* Here both the attribute and null
				* terminator will be stripped away.
				*/
			       dest = phg_attr_copy_avlist(dest, new_avlist);
		       }
	               break;
	       }
	       break;
	 }
    }
    *dest = 0; 
    return(dest);
}

/* phg_attr_count counts the number of slots in the av-list avlist.
 * Recursive lists are counted as being collapsed inline.
 */
int
phg_attr_count(avlist)
Phg_attr_avlist avlist; 
{
   /* count the null termination */
  return(phg_attr_count_avlist(avlist, 0) + 1);
}
#endif

static char *attr_names[PHG_ATTR_PKG_LAST-PHG_ATTR_PKG_FIRST+1] = {
    "Generic"
};


char *
phg_attr_sprint(s, attr)
char *s;
register Phg_attr_attribute attr;
{
    static char msgbuf[100];
    
    if (!s) s = msgbuf;
    if (((int) PHG_ATTR_PKG(attr)) >= ((int) PHG_ATTR_PKG_FIRST) && 
	((int) PHG_ATTR_PKG(attr)) <= ((int) PHG_ATTR_PKG_LAST)) {
	char *name = attr_names[((int) PHG_ATTR_PKG(attr)) - 
				((int)PHG_ATTR_PKG_FIRST)];

	if (!name) name = "Syspkg";
	(void) sprintf(s, "Attr pkg= %s, id= %d, cardinality= %d (0x%x)",
		name, PHG_ATTR_ORDINAL(attr), PHG_ATTR_CARDINALITY(attr), attr);
    } else {
	(void) sprintf(s, "Attr pkg= %s, id= %d, cardinality= %d (0x%x)",
	    PHG_ATTR_PKG(attr), PHG_ATTR_ORDINAL(attr),
		       PHG_ATTR_CARDINALITY(attr), attr);
    }
    return(s);
}

void
phg_attr_check_pkg(last_attr, attr)
    Phg_attr_attribute	last_attr, attr;
{
    if (!PHG_ATTR_VALID_PKG_ID(attr)) {
	char	expand_attr[100];

	(void) fprintf(stderr,
		"Malformed or non-terminated attribute-value list.\n");
	(void) fprintf(stderr, "Last valid attribute was %s.\n",
		phg_attr_sprint(expand_attr, last_attr));
	abort();
    }
}


int
phg_attr_count_avlist(avlist, last_attr) 
register Phg_attr_avlist	avlist; 
register Phg_attr_attribute	last_attr;
{
   register Phg_attr_attribute	attr;
   register unsigned int		count = 0;
   register unsigned int		num;
   register unsigned int		cardinality;
   
   while (attr = (Phg_attr_attribute) *avlist++) {
       count++;			/* count the attribute */
       cardinality = PHG_ATTR_CARDINALITY(attr);
       phg_attr_check_pkg(last_attr, attr);
       last_attr = attr;
       switch(PHG_ATTR_LIST_TYPE(attr)) {
           case PHG_ATTR_NONE:	/* not a list */
               count += cardinality;
               avlist += cardinality;
               break;
               
           case PHG_ATTR_NULL:	/* null terminated list */
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE:
                       /* Note that this only checks the first four bytes
                        * for the null termination.
                        */
                       while (*avlist++)
                           count++;
                       count++;			/* count the null terminator */
                       break;
               
                   case PHG_ATTR_LIST_IS_PTR:
                       count++;
                       avlist++;
                       break;
               }
               break;
               
            case PHG_ATTR_COUNTED:		/* counted list */
                switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                    case PHG_ATTR_LIST_IS_INLINE:
                       num = ((unsigned int)(*avlist)) * cardinality + 1;
                       count += num;
                       avlist += num;
                       break;
                    case PHG_ATTR_LIST_IS_PTR:
                        count++;
                        avlist++;
                        break;
                }
                break;      
               
            case PHG_ATTR_RECURSIVE:	/* recursive attribute-value list */
                if (cardinality == 0)	/* don't include the attribute */
                    count--;
                    
                switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                    case PHG_ATTR_LIST_IS_INLINE:
                        count += phg_attr_count_avlist(avlist, attr);
                        if (cardinality != 0)	/* count the null terminator */
                            count++;
                        avlist = phg_attr_skip(attr, avlist);
                        break;
                       
                    case PHG_ATTR_LIST_IS_PTR:
                        if (cardinality != 0) {	/* don't collapse inline */
                            count++;
                            avlist++;
                        } else if (*avlist)
                            /* Here we count the elements of the
                             * recursive list as being inline.
                             * Don't count the null terminator.
                             */
	                    count +=phg_attr_count_avlist((Phg_attr_avlist) (LINT_CAST
			    	(*avlist++)), attr);
	               else
	                   avlist++;
		       break;
                }
            break;
        }    
    }
    return count;
}

/* phg_attr_skip_value returns a pointer to the attribute after 
 * the value pointed to by avlist.  attr should be the 
 * attribute which describes the value at avlist.
 */
Phg_attr_avlist
phg_attr_skip_value(attr, avlist)
register Phg_attr_attribute	 attr;
register Phg_attr_avlist	 avlist;
{ 
    switch (PHG_ATTR_LIST_TYPE(attr)) {
        case PHG_ATTR_NULL:
            if (PHG_ATTR_LIST_PTR_TYPE(attr) == PHG_ATTR_LIST_IS_PTR)
                avlist++;
            else
 	        while (*avlist++);
 	    break;
 	        
 	case PHG_ATTR_RECURSIVE:
            if (PHG_ATTR_LIST_PTR_TYPE(attr) == PHG_ATTR_LIST_IS_PTR)
	        avlist++;
            else
                while (attr = (Phg_attr_attribute) *avlist++)
                    avlist = phg_attr_skip_value(attr, avlist);
 	    break;
 	        
 	case PHG_ATTR_COUNTED:
            if (PHG_ATTR_LIST_PTR_TYPE(attr) == PHG_ATTR_LIST_IS_PTR)
                avlist++;
            else
 	        avlist += ((int) *avlist) * PHG_ATTR_CARDINALITY(attr) + 1;
 	    break;
 	        
 	case PHG_ATTR_NONE:
 	    avlist += PHG_ATTR_CARDINALITY(attr);
 	    break;
     }
     return avlist;
}
 	
/* phg_attr_free frees the attribute-value list
 */
void
phg_attr_free(avlist)
Phg_attr_avlist avlist; 
{
   free((char *) avlist);
}

/* from attr_copy.c */

static	int	copy_1_attr();
static	int	copy_counted_list();
static	int	copy_singleton();
static	int	copy_null_list();

/*
 *	phg_attr_copy:	copy an attribute list, returning the size in bytes
 */
int
phg_attr_copy(source, dest)
    Phg_attr_attribute    **source, **dest;
{
    unsigned            attr;
    unsigned            result, size;

    size = 0;
    do {
	attr = **source;
	result = copy_1_attr(attr, source, dest);
	if (result == -1)
	    return -1;
	size += result;
    } while (attr != 0);
    return size;
}

static int
copy_1_attr(attr, source, dest)
    Phg_attr_attribute      attr, **source, **dest;
{
    int                 result, size;

    *source += 1;
    **dest = attr;
    *dest += 1;
    size = sizeof attr;
    if (attr == 0 || PHG_ATTR_BASE_TYPE(attr) == PHG_ATTR_BASE_NO_VALUE)
	return size;

    switch (PHG_ATTR_LIST_TYPE(attr)) {
      case PHG_ATTR_NONE:
	result = copy_singleton(attr, source, dest);
	break;
      case PHG_ATTR_NULL:
	result = copy_null_list(attr, source, dest);
	break;
      case PHG_ATTR_COUNTED:
	result = copy_counted_list(attr, source, dest);
	break;
      case PHG_ATTR_RECURSIVE:
	result = phg_attr_copy(source, dest);
	break;
      default:
	return -1;
    }
    if (result == -1)
	return -1;
    else
	return size + result;
}

static int
copy_counted_list(attr, source, dest)
    Phg_attr_attribute      attr, **source, **dest;
{
    register unsigned   count, n;
    register Phg_attr_attribute *srcp, *destp;

    (void)PHG_ATTR_CARDINALITY(attr);
    srcp = *source;
    destp = *dest;
    count = *srcp++;
    *destp++ = count;
    for (n = count; n--;) {
	*destp++ = *srcp++;
    }
    *source = srcp;
    *dest = destp;
    return count + 1;

}

static int
copy_singleton(attr, source, dest)
    Phg_attr_attribute      attr, **source, **dest;
{
    register int        count, size;
    register Phg_attr_attribute *srcp, *destp;

    count = PHG_ATTR_CARDINALITY(attr);
    size = count * 4;
    srcp = *source;
    destp = *dest;
    while (count-- > 0) {
	*destp++ = *srcp++;
    }
    *source = srcp;
    *dest = destp;
    return size;
}

static int
copy_null_list(attr, source, dest)
    Phg_attr_attribute      attr, **source, **dest;
{
    register int        count, size;
    register Phg_attr_attribute *srcp, *destp;

    count = 0;
    size = PHG_ATTR_CARDINALITY(attr);
    srcp = *source;
    destp = *dest;
    while (*srcp != 0) {
	register int        i;

	for (i = size; i--;) {
	    *destp++ = *srcp++;
	    count++;
	}
    }
    *destp++ = *srcp++;
    count++;
    *source = srcp;
    *dest = destp;
    return count * sizeof attr;
}

/* From attr_util.c */

#ifdef notdef
/* attr_create_list creates an avlist from the VARARGS passed
 * on the stack.  The storage is always allocated.
 */
/*VARARGS*/
Phg_attr_avlist
phg_attr_create_list(va_alist)
va_dcl
{
    va_list	valist;
    Phg_attr_avlist	avlist;

    va_start(valist);
    avlist = phg_attr_make((char **)0, 0, valist);
    va_end(valist);
    return avlist;
}
#endif

/* attr_find searches and avlist for the first occurrence of
 * a specified attribute.
 */
Phg_attr_avlist
phg_attr_find(attrs, attr)
register Phg_attr_avlist	attrs;
register Phg_attr_attribute	attr;
{
    for (; *attrs; attrs = phg_attr_next(attrs)) {
	if (*attrs == (caddr_t)attr) break;
    }
    return(attrs);
}

/* From attr_ptbl.c */

#ifndef NON_PORTABLE	

static int	 	valist_count_recursive();
static va_list		valist_skip_value();

#define	valist_get(valist)	va_arg(valist, caddr_t)

/* Copy count elements from list to dest.
 * Advance both list and dest to the next element after
 * the last one copied.
 */
#define	valist_copy_many(valist, dest, count)	\
    { \
	register caddr_t *last = dest + count; \
	\
	while (dest < last) \
	    *dest++ = valist_get(valist); \
    }


/* A macro to copy attribute values 
 * count is the number of caddr_t size chunks to copy.
 */
#define	valist_copy_values(src, dest, count) \
    if (count == 1) \
        *dest++ = valist_get(src); \
    else { \
	valist_copy_many(src, dest, count); \
    }


/* copy the var-args list from * valist to dest.  
 * Recursive lists are collapsed into dest.
 */
 
Phg_attr_avlist
phg_attr_copy_valist(dest, valist) 
register Phg_attr_avlist	dest;
va_list	valist;
{
   register Phg_attr_attribute	attr;
   register unsigned int		cardinality;
   
   while (attr = (Phg_attr_attribute) valist_get(valist)) {
       cardinality = PHG_ATTR_CARDINALITY(attr);
       switch(PHG_ATTR_LIST_TYPE(attr)) {
           case PHG_ATTR_NONE:	/* not a list */
               *dest++ = (caddr_t) attr;
               valist_copy_values(valist, dest, cardinality);
               break;
               
           case PHG_ATTR_NULL:	/* null terminated list */
               *dest++ = (caddr_t) attr;
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE:
                       /* Note that this only checks the first four bytes
                        * for the null termination.
			* Copy each value element until we have copied the
			* null termination.
                        */
		       do {
		           valist_copy_values(valist, dest, cardinality);
		       } while (*(dest - 1));
                       break;
                       
                   case PHG_ATTR_LIST_IS_PTR:
                       *dest++ = valist_get(valist);
                       break;
               }
               break;
               
            case PHG_ATTR_COUNTED:		/* counted list */
               *dest++ = (caddr_t) attr;
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE: {
		       register unsigned int	count;

		       *dest = valist_get(valist);	/* copy the count */
			count = ((unsigned int) *dest++) * cardinality;
			valist_copy_values(valist, dest, count);
                        break;
		   }
                        
                   case PHG_ATTR_LIST_IS_PTR:
                       *dest++ = valist_get(valist);
                       break;
               }
               break;
               
            case PHG_ATTR_RECURSIVE:	/* recursive attribute-value list */
                if (cardinality != 0)	/* don't strip it */
                    *dest++ = (caddr_t) attr;
                    
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE:
                       dest = phg_attr_copy_valist(dest, valist);
                       if (cardinality != 0)	/* don't strip it */
                           dest++;	/* move past the null terminator */
		       valist = valist_skip_value(attr, valist);
                       break;
                       
                   case PHG_ATTR_LIST_IS_PTR:
                       if (cardinality != 0)	/* don't collapse inline */
                           *dest++ = valist_get(valist);
                       else {
			   Phg_attr_avlist new_avlist =(Phg_attr_avlist) valist_get(valist);
			   if (new_avlist)
			       /* Copy the list inline -- don't 
				* move past the null termintor.
				* Here both the attribute and null
				* terminator will be stripped away.
				*/
			       dest = phg_attr_copy_valist(dest, new_avlist);
		       }
	               break;
	       }
	       break;
	 }
    }
    *dest = 0; 
    return(dest);
}

/* return a pointer to the attribute after 
 * the value pointed to by list.  attr should be the 
 * attribute which describes the value at list.
 */
static va_list
valist_skip_value(attr, valist)
register Phg_attr_attribute	 attr;
va_list	valist;
{ 
    switch (PHG_ATTR_LIST_TYPE(attr)) {
        case PHG_ATTR_NULL:
            if (PHG_ATTR_LIST_PTR_TYPE(attr) == PHG_ATTR_LIST_IS_PTR)
                (void) valist_get(valist);
            else
 	        while (valist_get(valist));
 	    break;
 	        
 	case PHG_ATTR_RECURSIVE:
            if (PHG_ATTR_LIST_PTR_TYPE(attr) == PHG_ATTR_LIST_IS_PTR)
                (void) valist_get(valist);
            else
                while (attr = (Phg_attr_attribute) valist_get(valist))
                    valist = valist_skip_value(attr, valist);
 	    break;
 	        
 	case PHG_ATTR_COUNTED:
            if (PHG_ATTR_LIST_PTR_TYPE(attr) == PHG_ATTR_LIST_IS_PTR) {
                (void) valist_get(valist);
		break;
	    }
	    /* else fall through ... */
 	case PHG_ATTR_NONE: {
		register unsigned int count = PHG_ATTR_CARDINALITY(attr);

		if (PHG_ATTR_LIST_TYPE(attr) == PHG_ATTR_COUNTED)	
		    /* use the count */
		    count *= (unsigned int) valist_get(valist);
		while (count--)
		    (void) valist_get(valist);
	    }
 	    break;
     }
     return valist;
}


/* valist_count counts the number of slots in the varargs-list valist.
 * Recursive lists are counted as being collapsed inline.
 */
int
valist_count(valist)
va_list valist; 
{
   /* count the null termination */
   return(valist_count_recursive(valist, 0) + 1);
}

static int
valist_count_recursive(valist, last_attr) 
va_list	valist; 
register Phg_attr_attribute	last_attr;
{
   register Phg_attr_attribute	attr;
   register unsigned int		count = 0;
   register unsigned int		num;
   register unsigned int		cardinality;
   
   while (attr = (Phg_attr_attribute) valist_get(valist)) {
       count++;			/* count the attribute */
       cardinality = PHG_ATTR_CARDINALITY(attr);
       phg_attr_check_pkg(last_attr, attr);
       last_attr = attr;
       switch(PHG_ATTR_LIST_TYPE(attr)) {
           case PHG_ATTR_NONE:	/* not a list */
               count += cardinality;
	       valist = valist_skip_value(attr, valist);
               break;
               
           case PHG_ATTR_NULL:	/* null terminated list */
               switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                   case PHG_ATTR_LIST_IS_INLINE:
                       /* Note that this only checks the first four bytes
                        * for the null termination.
                        */
                       while (valist_get(valist))
                           count++;
                       count++;			/* count the null terminator */
                       break;
               
                   case PHG_ATTR_LIST_IS_PTR:
                       count++;
		       (void) valist_get(valist);
                       break;
               }
               break;
               
            case PHG_ATTR_COUNTED:		/* counted list */
                switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                    case PHG_ATTR_LIST_IS_INLINE: {
		       va_list	orig_list = valist;

                       num = ((unsigned int) (valist_get(valist))) * cardinality + 1;
                       count += num;
		       valist = valist_skip_value(attr, orig_list);
                       break;
		    }
                    case PHG_ATTR_LIST_IS_PTR:
                        count++;
		        (void) valist_get(valist);
                        break;
                }
                break;      
               
            case PHG_ATTR_RECURSIVE:	/* recursive attribute-value list */
                if (cardinality == 0)	/* don't include the attribute */
                    count--;
                    
                switch (PHG_ATTR_LIST_PTR_TYPE(attr)) {
                    case PHG_ATTR_LIST_IS_INLINE:
                        count += valist_count_recursive(valist, attr);
                        if (cardinality != 0)	/* count the null terminator */
                            count++;
                        valist = valist_skip_value(attr, valist);
                        break;
                       
                    case PHG_ATTR_LIST_IS_PTR:
                        if (cardinality != 0) {	/* don't collapse inline */
                            count++;
			    (void) valist_get(valist);
                        } else {
			    Phg_attr_avlist new_avlist =(Phg_attr_avlist) valist_get(valist);

			    if (new_avlist)
				/* Here we count the elements of the
				 * recursive list as being inline.
				 * Don't count the null terminator.
				 */
				count += phg_attr_count_avlist(new_avlist, attr);
		       }
		       break;
                }
            break;
        }    
    }
    return count;
}

#endif /* not NON_PORTABLE */
