
/************************************************

  AGMACROS.H

  Mark Ackerman
  MIT/Center for Coordination Science
  MIT/Project Athena

  -- Copyright (c) 1994 Regents of the University of California.
  -- All rights reserved.
  --
  -- This software was developed by the Answer Garden project
  -- at the University of California, Irvine.
  --
  -- Redistribution and use in source and binary forms are permitted
  -- provided that the above copyright notice and this paragraph are
  -- duplicated in all such forms and that any documentation,
  -- advertising materials, and other materials related to such
  -- distribution and use acknowledge that the software was developed
  -- by the University of California, Irvine.  The name of the
  -- University may not be used to endorse or promote products derived
  -- from this software without specific prior written permission.
  -- THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
  -- IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
  -- WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
  
  -- Answer Garden is a trademark of the Regents of the University of
  -- California.  All rights reserved.

  These are for the Suns of the world.

************************************************/
#ifndef _AGmacros_h
#define _AGmacros_h

  /* Xos will go out and pick up either string.h or strings.h.  If SYSV,
     then index and rindex will be set correctly */
#include <X11/Xos.h> 

#define AGindex(ptr,chr)  index(ptr,chr)

#define StringEqual(s1,s2)  (!AGstrcmp(s1,s2))
#define StringNotEqual(s1,s2) (AGstrcmp(s1,s2))
		    

/* CodeCenter/Saber requires the "!= NULL" check. */

#define AGstrlen(string)  ((string != NULL) ? strlen(string) : 0)

#define AGstrcmp(s1,s2)  ((s2 != NULL) ? \
			  (  (s1 != NULL) ?  strcmp(s1,s2) : -1 ) : \
			  (  (s1 != NULL) ?  1 : 0 ) \
			  )

#define AGstrcpy(s1,s2)  ((s2 != NULL) ? strcpy(s1,s2) : strcpy(s1,"") )
#define AGstrncpy(s1,s2,n)  ((s2 != NULL) ? strncpy(s1,s2,n) : strncpy(s1,"",1) )
#define AGstrcat(s1,s2)  ((s2 != NULL) ? strcat(s1,s2) : strcat(s1,""))

#define AGMakeString(string,first_char,end_char) \
    { \
	    AGstrncpy(string,first_char,end_char+1-first_char); \
	    string[(int)(end_char+1-first_char)] = EOS; \
	    }


/* 5/14/93 MSA (for protection) */
#define AGIsWidget(w) (w != NULL && XtIsWidget(w))

#ifdef SAO_MOD
#define AGQuotes(c) (c == '"')
#endif


#endif /*_AGmacros_h*/


