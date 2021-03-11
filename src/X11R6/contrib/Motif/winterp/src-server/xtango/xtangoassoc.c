/* -*-C-*-
*******************************************************************************
*
* File:         xtangoassoc.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoassoc.c,v 2.8 1994/06/09 01:26:01 npm Exp $
* Description:  ASSOCIATION PACKAGE (assoc)
* Author:       John T. Stasko, Doug Hayes, Niels Mayer
* Created:      1990
* Modified:     Sun Jun  5 05:23:48 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:	X11r6 contrib release
*
* Xtango 1.52 Copyright 1990-1994 Georgia Institute of Technology
* 			     (by John T. Stasko and Doug Hayes).
* WINTERP 2.0 Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* This version of Xtango 1.52 (varargs version) represents a subset of
* the Xtango distribution that has been modified specifically for use with
* WINTERP. Non-WINTERP uses of Xtango should use the complete, standard
* version of Xtango, which is available under separate copyright via
* anonymous ftp from par.cc.gatech.edu:pub/xtangovarargs.tar.Z and
* par.cc.gatech.edu:pub/xtango.tar.Z.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Georgia Institute of Technology, 
* John T. Stasko, Doug Hayes, Enterprise Integration Technologies, 
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Georgia Institute of Technology, John T. Stasko,
* Doug Hayes, Enterprise Integration Technologies, Hewlett-Packard Company,
* and Niels Mayer makes no representations about the suitability of this 
* software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* GEORGIA INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GEORGIA
* INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*******************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangoassoc.c,v 2.8 1994/06/09 01:26:01 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include  "xtangolocal.h"

/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

#define 	PRIME	997    /* number to mod by */

typedef struct RECORD_NODE   /* structure to store records */
{
   int key[5];
   int record;	 /* universal value, must be cast be receiving routine */
   struct RECORD_NODE *next;
} *RECORD_PTR;


typedef struct TABLE_NODE    /* a hash table */
{
   char name[100];		/* association name */
   int keys;			/* number of keys in this one */
   RECORD_PTR bucket[PRIME];	/* buckets of the hash table */
   struct TABLE_NODE *next;
} *TABLE_PTR;

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

TABLE_PTR  Hash[PRIME];      /* primary hash table structure */

int  ASSOC_init = 0;	     /* set to 1 when ASSOCinit() called */

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

TABLE_PTR	table_exists();
RECORD_PTR	record_exists();
int             remove_record();
int		same_record();
int		hash_string();
int		hash_keys();
void		assoc_dump();


/***************************************************************/
/*							       */
/*   ASSOCinit - must be called to start-up.  Sets up some     */
/*	basic associations.				       */
/*							       */
/***************************************************************/

void
ASSOCinit()
{
   int i;

   if (ASSOC_init) return;
   ASSOC_init = 1;

   DEBUG("ASSOCinit()\n");

   for (i=0; i<PRIME; ++i)
      Hash[i] = NULL;

   ASSOCmake("ID",2);
   ASSOCmake("ID3",3);
   ASSOCmake("IMAGE_AT",2);
}



/***************************************************************/
/*							       */
/*   ASSOCmake - receive a name of an association to create    */
/*	and the number of keys involved in this association.   */
/*							       */
/***************************************************************/

void
ASSOCmake(name,keys)
   char *name;
   int keys;
{
   TABLE_PTR table;
   int val;
   int index;

   if (!ASSOC_init) ASSOCinit();

   DEBUG("ASSOCmake(\"%s\",%d)\n", name, keys);

   if (table_exists(name))
      return;

   table = (TABLE_PTR) malloc( sizeof( struct TABLE_NODE ) );
   val = hash_string(name);
   strcpy(table->name,name);

/* for (index=0,ptr=name; *ptr; ++index,ptr++)
      table->name[index] = *ptr;
   table->name[index+1] = 0;	 */
   table->keys = keys;
   for (index=0; index<PRIME; ++index)
      table->bucket[index] = NULL;
   table->next = Hash[val];
   Hash[val] = table;
}



/***************************************************************/
/*							       */
/*   ASSOCstore - store a record in a certain association under*/
/*	a set of keys.					       */
/*							       */
/***************************************************************/

void
ASSOCstore
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(char *name, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   TABLE_PTR table;
   int keys,p[5],i;
   int hash_val;
   RECORD_PTR r;
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, name);
#else  /* defined(_NO_PROTO) */
   char *name;
   va_start(ap);
   name = va_arg(ap, char*);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!ASSOC_init) ASSOCinit();

   DEBUG("ASSOCstore(\"%s\" ... )\n", name);

   if (!(table = table_exists(name)))
      { fprintf(stderr,"Attempt to set association %s which does not exist\n",name);
        va_end(ap);
	return;
      }

   keys = table->keys;
   for (i=0; i<keys; i++)
      p[i] = va_arg(ap, int);
   hash_val = hash_keys(keys,p);

   if (!(r = record_exists(table,hash_val,p)))
      { r = (RECORD_PTR) malloc( sizeof( struct RECORD_NODE ) );
	r->next = table->bucket[hash_val];
	table->bucket[hash_val] = r;
      }
   for (i=0; i<keys; i++)
      r->key[i] = p[i];
   r->record = va_arg(ap, int);
   va_end(ap);
}



/***************************************************************/
/*							       */
/*   ASSOCmember - tell if a record has been saved under the   */
/*      given association and keys.     		       */
/*							       */
/***************************************************************/

int 
ASSOCmember
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(char *name, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   TABLE_PTR t;
   int keys,p[5],i,ret;
   int hash_val;
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, name);
#else  /* defined(_NO_PROTO) */
   char *name;
   va_start(ap);
   name = va_arg(ap, char*);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!ASSOC_init) ASSOCinit();

   DEBUG("ASSOCmember(\"%s\" ... )\n", name);

   if (!(t = table_exists(name))) {
      va_end(ap);
      return(0);
    }
   keys = t->keys;
   for (i=0; i<keys; i++)
      p[i] = va_arg(ap, int);
   hash_val = hash_keys(keys,p);
   ret = (record_exists(t,hash_val,p) ? 1 : 0);
   va_end(ap);
   return(ret);
}



/***************************************************************/
/*							       */
/*   ASSOCdelete - remove a record from the association table  */
/*							       */
/***************************************************************/

int 
ASSOCdelete
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(char *name, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   TABLE_PTR t;
   int keys,p[5],i;
   int hash_val;
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, name);
#else  /* defined(_NO_PROTO) */
   char *name;
   va_start(ap);
   name = va_arg(ap, char*);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!ASSOC_init) ASSOCinit();

   DEBUG("ASSOCdelete(\"%s\"...)\n", name);

   if (!(t = table_exists(name)))
      { fprintf(stderr,"Attempt to delete association %s which does not exist\n",name);
        va_end(ap);
	return(0);
      }

   keys = t->keys;
   for (i=0; i<keys; i++)
      p[i] = va_arg(ap, int);
   hash_val = hash_keys(keys,p);
   if (!remove_record(t,hash_val,p)) 
      { fprintf(stderr,"Attempt to delete association %s which does not exist\n",name);
        va_end(ap);
        return(0);
      }
   else {
      va_end(ap);
      return(1);
    }
}



/***************************************************************/
/*							       */
/*   ASSOCretrieve - return the record saved under the given   */
/*	association and keys.				       */
/*							       */
/***************************************************************/

int  /* universal ptr */
ASSOCretrieve
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(char *name, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   TABLE_PTR t;
   RECORD_PTR r;
   int keys,i,p[5];
   int hash_val;
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, name);
#else  /* defined(_NO_PROTO) */
   char *name;
   va_start(ap);
   name = va_arg(ap, char*);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!ASSOC_init) ASSOCinit();

   DEBUG("ASSOCretrieve(\"%s\" ... )\n", name);

   if (!(t = table_exists(name)))
      { fprintf(stderr,"Attempt to get association %s which does not exist\n",name);
        va_end(ap);
#ifdef WINTERP
	return((int) NULL);
#else
	return(NULL);
#endif /* WINTERP */
      }

   keys = t->keys;
   for (i=0; i<keys; i++)
      p[i] = va_arg(ap, int);
   hash_val = hash_keys(keys,p);

   if (!(r = record_exists(t,hash_val,p)))
      { switch (keys)
	   { case 0:
		fprintf(stderr,"No record for assoc %s\n",name);
                if (TANGO__data->debug & TANGO_DEBUG_EXTERNAL)
 		   assoc_dump();
		break;
	     case 1:
		fprintf(stderr,"No record for assoc %s, keys = (%d)\n",name,p[0]);
                if (TANGO__data->debug & TANGO_DEBUG_EXTERNAL)
 		   assoc_dump();
		break;
	     case 2:
		fprintf(stderr,"No record for assoc %s, keys = (%d,%d)\n",name,p[0],p[1]);
                if (TANGO__data->debug & TANGO_DEBUG_EXTERNAL)
 		   assoc_dump();
		break;
	     case 3:
		fprintf(stderr,"No record for assoc %s, keys = (%d,%d,%d)\n",name,p[0],p[1],p[2]);
                if (TANGO__data->debug & TANGO_DEBUG_EXTERNAL)
 		   assoc_dump();
		break;
	     case 4:
		fprintf(stderr,"No record for assoc %s, keys = (%d,%d,%d,%d)\n",name,p[0],p[1],p[2],p[3]);
                if (TANGO__data->debug & TANGO_DEBUG_EXTERNAL)
 		   assoc_dump();
		break;
	     case 5:
		fprintf(stderr,"No record for assoc %s, keys = (%d,%d,%d,%d,%d)\n",name,p[0],p[1],p[2],p[3],p[4]);
                if (TANGO__data->debug & TANGO_DEBUG_EXTERNAL)
 		   assoc_dump();
		break;
	   }
        va_end(ap);
#ifdef WINTERP
	return((int) NULL);
#else
	return(NULL);
#endif /* WINTERP */
      }
   else {
      va_end(ap);
      return(r->record);
    }
}



/***************************************************************/
/*							       */
/*   ASSOC_clear - clear out the hash table of all association */
/*	relations.					       */
/*							       */
/***************************************************************/

void
ASSOC_clear()
{
   int i,j;
   TABLE_PTR table;
   RECORD_PTR record;

   for (i=0; i<PRIME; ++i)
      { while (Hash[i])
	   { table = Hash[i];
	     for (j=0; j<PRIME; ++j)
		{ while (table->bucket[j])
		     { record = table->bucket[j];
		       table->bucket[j] = table->bucket[j]->next;
		       free(record);
		     }
		  table->bucket[j] = NULL;
		}
	     Hash[i] = Hash[i]->next;
	     free(table);
	   }
	Hash[i] = NULL;
      }

   ASSOC_init = 0;
}



/***************************************************************/
/*							       */
/*   table_exists - return the table of the given association, */
/*	or NULL if it does not exist.			       */
/*							       */
/***************************************************************/

TABLE_PTR
table_exists(name)
   char *name;
{
   int val;
   TABLE_PTR tp;

   val = hash_string(name);
   for (tp = Hash[val]; tp; tp=tp->next)
      if (!(strcmp(name,tp->name)))
	 break;
   return(tp);
}



/***************************************************************/
/*							       */
/*   record_exists - return the record saved under the given   */
/*	keys in the given table if it exists, NULL otherwise.  */
/*							       */
/***************************************************************/

RECORD_PTR
record_exists(tp,hash_val,k)
   TABLE_PTR tp;
   int hash_val;
   int k[];
{
   RECORD_PTR rp;

   for (rp=tp->bucket[hash_val]; rp; rp=rp->next)
      if (same_record(rp,tp->keys,k))
	 break;
   return(rp);
}



/***************************************************************/
/*							       */
/*   remove_record - clear a record out of the data base       */
/*							       */
/***************************************************************/

int
remove_record(tp,hash_val,k)
   TABLE_PTR tp;
   int hash_val;
   int k[];
{
   RECORD_PTR rp,old;

   for (rp=tp->bucket[hash_val]; rp; rp=rp->next) 
      { if (same_record(rp,tp->keys,k)) 
           { if (rp == tp->bucket[hash_val]) 
                { tp->bucket[hash_val] = rp->next;
                  free(rp);
                }
             else 
 	        { old->next = rp->next;
                  free(rp);
                }
             return(1);
           }
        old = rp;
      }
   return(0);
}



/***************************************************************/
/*							       */
/*   same_record - return 1 if the given RECORD_PTR has the    */
/*	given set of keys.				       */
/*							       */
/***************************************************************/

int
same_record(rp,keys,k)
   RECORD_PTR rp;
   int keys;
   int k[];
{
   int i;

   for (i=0; i<keys; i++) 
      if (rp->key[i] != k[i])
         return(0);
   return(1);
}



/***************************************************************/
/*							       */
/*   hash_string - hash the given string by adding all of its  */
/*	characters as they are mapped to integers	       */
/*							       */
/***************************************************************/

int
hash_string(name)
   char *name;
{
   int val;
   char *ptr;

   for (val=0,ptr=name; *ptr; ++ptr)
      val += (int) *ptr;
   return( val % PRIME );
}



/***************************************************************/
/*							       */
/*   hash_keys - return the hash value of the given number of  */
/*	the given keys. 				       */
/*							       */
/***************************************************************/

int
hash_keys(num,k)
   int num;
   int k[];
{
   int val,i;

   val = 0;
   for (i=0; i<num; i++)
      val += abs(k[i]);
   return( val % PRIME );
}



/***************************************************************/
/*							       */
/*   assoc_dump - write out all the current associations.      */
/*							       */
/***************************************************************/

void
assoc_dump()
{
   int i,j;
   TABLE_PTR table;
   RECORD_PTR record;

   fprintf(stderr,"Dumping associations\n");
   for (i=0; i<PRIME; ++i)
      { table = Hash[i];
	while (table)
	   { fprintf(stderr,"\nAssociation %s with keys\n",table->name);
	     for (j=0; j<PRIME; ++j)
		{ record = table->bucket[j];
		  while (record)
		     { switch(table->keys)
		       { case 0:
			    fprintf(stderr,"(No parameters)\n");
			    break;
			 case 1:
			    fprintf(stderr,"%d\n",record->key[0]);
			    break;
			 case 2:
			    fprintf(stderr,"%d %d\n",record->key[0],
                                      record->key[1]);
			    break;
			 case 3:
			    fprintf(stderr,"%d %d %d\n",record->key[0],
                                         record->key[1],record->key[2]);
			    break;
			 case 4:
			    fprintf(stderr,"%d %d %d %d\n",record->key[0],
                                 record->key[1],record->key[2],record->key[3]);
			    break;
			 case 5:
			    fprintf(stderr,"%d %d %d %d %d\n",record->key[0],
                               record->key[1],record->key[2],record->key[3],
                               record->key[4]);
			    break;
			 default:
			    /* ? */;
		       }
		       record = record->next;
		     }
		  }
	     table = table->next;
	   }
      }
   fprintf(stderr,"\n\n");
}

/**************************************************************/
/*****************   end of xtangoassoc.c    ******************/
/**************************************************************/
