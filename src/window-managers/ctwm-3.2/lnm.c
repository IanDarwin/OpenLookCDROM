/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */
/*
 *    Tranlate (VMS) logical name from "in" to "out".
 */

#include <string.h>
#include <starlet.h>
#include <lnmdef.h>
#include <descrip.h>

static struct dsc$descriptor_s create_descriptor (string)
char *string;
{
   struct dsc$descriptor_s descrip_string;

   /* build it */

   descrip_string.dsc$b_dtype	= DSC$K_DTYPE_T;
   descrip_string.dsc$b_class	= DSC$K_CLASS_S;
   descrip_string.dsc$w_length	= strlen(string);
   descrip_string.dsc$a_pointer	= string;

   /* return it */

   return (descrip_string);
}

int GetLogical(Logical_Name, outbuf)
char *Logical_Name, *outbuf;
{
	static short len;
	int istatus;
	int i;
	static char retbuf[256];
	char *rptr;
        struct dsc$descriptor_s search_list = create_descriptor("LNM$FILE_DEV");
        struct dsc$descriptor_s logical_name = create_descriptor(Logical_Name);
	struct {
	    short len;
	    short code;
	    char *buf;
	    short *retlen;
	} ItmLst[] = { {255, LNM$_STRING, retbuf, &len},
		       {0, 0, (char *)0, 0}
	};

	static long attr=LNM$M_CASE_BLIND;

	istatus = sys$trnlnm(&attr,
			     &search_list,
			     &logical_name,
			     0,
			     ItmLst);

	if (!(istatus&1)) {
	    return(istatus);
	} else {
	    retbuf[len] = '\0';
	    rptr = retbuf;
	    if (*rptr == 27)	/* process permanent file */
               rptr += 4;	/* skip past it the iff */
	    strcpy (outbuf, rptr);
	    return(1);
	}
}

CreateLogical(name,table,definition, accmode)
char *name, *table, *definition;
int  accmode;
{
	struct dsc$descriptor_s table_name = create_descriptor (table);
	struct dsc$descriptor_s logical_name = create_descriptor (name);
	struct {
	    short len;
	    short item;
	    char *buffer;
	    short *buflen;
	} itmlst[2];
	itmlst[0].len = strlen(definition);
	itmlst[0].item = LNM$_STRING;
	itmlst[0].buffer = definition;
	itmlst[0].buflen = 0;
	itmlst[1].len = itmlst[1].item = 0;

	(void) sys$crelnm(0, &table_name,
			  &logical_name,
			  &accmode, itmlst);
}

/*
 *	Routine to delete logical names
 */

DeleteLogical(name,table)
char *name, *table;
{
	struct dsc$descriptor_s table_name = create_descriptor (table);
	struct dsc$descriptor_s logical_name = create_descriptor (name);
	(void) sys$dellnm(&table_name,
			  &logical_name,
			  0);
}
