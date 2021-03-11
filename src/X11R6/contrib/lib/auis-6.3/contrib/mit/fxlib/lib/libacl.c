/*LINTLIBRARY*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/libacl.c,v 1.3 1992/12/15 21:53:01 rr2b R6tape $";
#endif

/**********************************************************************
 * Access Control List Library
 *
 * $Author: rr2b $
 * $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/libacl.c,v $
 * $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/lib/RCS/libacl.c,v 1.3 1992/12/15 21:53:01 rr2b R6tape $
 *
 * Copyright 1989, 1990 by the Massachusetts Institute of Technology.
 *
 * For copying and distribution information, please see the file
 * <mit-copyright.h>.
 **********************************************************************/
#include <mit-copyright.h>

 

#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <krb.h>
#include <sys/errno.h>
#include <netdb.h>
#include "memory.h"

#define TRUE (0 == 0)
#define FALSE (0 == 1)
/* If ACL_ALREADY != 0, it is considered a failure to try to add
 * a principal to an acl that already contains it or to delete a
 * principal from an acl that doesn't contain it.
 */
#define ACL_ALREADY 1
char *
acl_canonicalize_principal(principal, buf)
     char *principal;
     char *buf;      /* RETVAL */
{
  char name[ANAME_SZ], instance[INST_SZ], realm[REALM_SZ];
  char *s1, *s2;

  s1 = index(principal, '@');
  if (s1) {
    (void) strcpy(realm, s1);
    s2 = index(principal, '.');
    if (s2 != NULL && s2 < s1) {
      (void) strncpy(instance, s2, s1 - s2);
      instance[s1 - s2] = '\0';
      (void) strncpy(name, principal, s2 - principal);
      name[s2 - principal] = '\0';
    }
    else {   /* s2 */
      (void) strcpy(instance, ".");
      (void) strncpy(name, principal, s1 - principal);
      name[s1 - principal] = '\0';
    }
  }    
  else {   /* s1 */
    (void) strcat(strcpy(realm, "@"), KRB_REALM);
    s2 = index(principal, '.');
    if (s2) {
      (void) strcpy(instance, s2);
      (void) strncpy(name, principal, s2 - principal);
      name[s2 - principal] = '\0';
    }
    else {   /* s2 */
      (void) strcpy(instance, ".");
      (void) strcpy(name, principal);
    }
  }
  return(strcat(strcat(strcpy(buf, name), instance), realm));
}

_acl_match(criterion, sample)
     char *criterion, *sample;
{
  register char *c, *s;

  c = criterion; s = sample;

  while(TRUE) {
    if (*c == *s && *s == '\0') return(TRUE);
    if (*c == '*') {
      if (*(c+1) == '\0') return(TRUE);
      while(*s != '\0')
        if (_acl_match(c+1, s++)) return(TRUE);
      return(FALSE);
    }
    if (*c != *s) return(FALSE);
    c++; s++;
  }
}

acl_check(acl, principal)
     char *acl;
     char *principal;
{
  FILE *fp;
  char buf[MAX_K_NAME_SZ], canon[MAX_K_NAME_SZ];

  fp = fopen(acl, "r");
  if (!fp) return(0);
  (void) acl_canonicalize_principal(principal, canon);

  while(fgets(buf, MAX_K_NAME_SZ, fp)) {
    buf[strlen(buf)-1] = '\0';   /* strip trailing newline */
    if (_acl_match(buf, canon)) {
      (void) fclose(fp);
      return(1);
    }
  }
  (void) fclose(fp);
  return(0);
}

acl_exact_match(acl, principal)
     char *acl;
     char *principal;
{
  FILE *fp;
  char buf[MAX_K_NAME_SZ];

  fp = fopen(acl, "r");
  if (!fp) return(0);

  while(fgets(buf, MAX_K_NAME_SZ, fp)) {
    buf[strlen(buf)-1] = '\0';
    if (!strcmp(buf, principal)) {
      (void) fclose(fp);
      return(1);
    }
  }
  (void) fclose(fp);
  return(0);
}

acl_add(acl, principal)
     char *acl;
     char *principal;
{
  FILE *fp;
  char canon[MAX_K_NAME_SZ];

  (void) acl_canonicalize_principal(principal, canon);
  if (acl_exact_match(acl, principal)) return(ACL_ALREADY);

  fp = fopen(acl, "a");
  if (!fp) return(1);
  
  fputs(canon, fp);
  fputs("\n", fp);
  if (fclose(fp) == EOF) return(1);
  return(0);
}

acl_delete(acl, principal)
     char *acl;
     char *principal;
{
  FILE *fp1, *fp2;
  char canon[MAX_K_NAME_SZ], buf[MAX_K_NAME_SZ];
  char *tmpf;
  int retval = ACL_ALREADY;

  fp1 = fopen(acl, "r");
  if (!fp1) return(1);

  if ((tmpf = (char *)malloc((unsigned)strlen(acl)+5)) == NULL)
    return(1);
  fp2 = fopen(strcat(strcpy(tmpf, acl), ".tmp"), "w");
  if (!fp2) return(1);

  (void) strcat(acl_canonicalize_principal(principal, canon), "\n");
  while(fgets(buf, MAX_K_NAME_SZ, fp1)) {
    if (strcmp(buf, canon)) {
      fputs(buf, fp2);
    }
    else retval = 0;
  }
  (void) fclose(fp1);
  if (fclose(fp2) == EOF) {
    (void) unlink(tmpf);
    free(tmpf);
    return(1);
  }
  retval |= rename(tmpf, acl);
  free(tmpf);
  return(retval);
}

acl_initialize(acl, mode)
     char *acl;
     int mode;
{
  FILE *fp;

  if ((fp = fopen(acl, "w")) == NULL) return(1);
  (void) fclose(fp);

  return(chmod(acl, mode));
}
