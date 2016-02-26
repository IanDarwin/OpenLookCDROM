/*LINTLIBRARY*/
#ifndef lint
static char sccsid[] = "@(#)rec.c 1.2 88/12/09" ;
#endif

/*  Record handling routines used by the faces program.
 * 
 *  Copyright (c) Rich Burridge - Sun Microsystems Australia.
 *                                All rights reserved.
 *
 *  Permission is given to distribute these sources, as long as the
 *  copyright messages are not removed, and no monies are exchanged. 
 * 
 *  No responsibility is taken for any errors on inaccuracies inherent
 *  either to the comments or the code of this program, but if reported
 *  to me, then an attempt will be made to fix them.
 */

#include "faces.h"
#include "extern.h"

char *
Malloc(n)
int n ;
{
  char *val ;

  if ((val = malloc((unsigned) n)) == NULL)
    FPRINTF(stderr,"%s: Out of memory.\n",progname) ;
  return val ;
}


add_alias(crec, username, alias)  /* Add new alias to hostnames' list. */
struct comminfo *crec ;
char *username, *alias ;
{
  struct peopinfo *cptemp, *ptemp ;

  ptemp = (struct peopinfo *) Malloc(sizeof(struct peopinfo)) ;
  ptemp->alias = (char *) Malloc(strlen(alias)) ;
  STRCPY(ptemp->alias, alias) ;
  ptemp->username = (char *) Malloc(strlen(username)) ;
  STRCPY(ptemp->username, username) ;
  ptemp->next = NULL ;

  if (crec->people == NULL) crec->people = ptemp ;
  else
    {
      cptemp = crec->people ;
      while (cptemp != NULL)
        if (cptemp->next == NULL)
          {
            cptemp->next = ptemp ;
            return ;
          }
        else cptemp = cptemp->next ;
    }
}


add_machine(machine, community)    /* Add new machine to list. */
char *machine, *community ;
{
  struct machinfo *temp ;

  temp = (struct machinfo *) Malloc(sizeof(struct machinfo)) ;
  temp->machine = (char *) Malloc(strlen(machine)) ;
  STRCPY(temp->machine, machine) ;
  temp->community = (char *) Malloc(strlen(community)) ;
  STRCPY(temp->community, community) ;
  temp->next = NULL ;

  if (machines == NULL) machines = mlast = temp ;   /* Start chain. */
  else if (mlast != NULL)
    {
      mlast->next = temp ;     /* Add record to existing chain. */
      mlast = temp ;           /* Point to end of chain. */
    }
}


add_ps_rec(row, column, name)  /* Add record for later animation. */
int row, column ;
char *name ;
{
  struct psinfo *temp ;

  temp = (struct psinfo *) Malloc(sizeof(struct psinfo)) ;
  temp->name = (char *) Malloc(strlen(name)) ;
  STRCPY(temp->name, name) ;
  temp->row = row ;
  temp->column = column ;
  temp->next = NULL ;

  if (psrecs == NULL) psrecs = plast = temp ;    /* Start chain. */
  else if (plast != NULL)
    {
      plast->next = temp ;     /* Add record to existing chain. */
      plast = temp ;
    }
}


add_record(community, username, timestamp, size)
char *community, *username, *timestamp ;
int size ;
{
  struct recinfo *temp ;

  temp = (struct recinfo *) Malloc(sizeof(struct recinfo)) ;
  temp->community = (char *) Malloc(strlen(community)) ;
  STRCPY(temp->community, community) ;
  temp->username = (char *) Malloc(strlen(username)) ;
  STRCPY(temp->username, username) ;
  temp->iconname = (char *) Malloc(strlen(iconname)) ;
  STRCPY(temp->iconname, iconname) ;
  STRCPY(temp->ts, timestamp) ;
  temp->size = size ;
  temp->total = 1 ;
  temp->next = NULL ;
  noicons++ ;
  if (recs == NULL) recs = last = temp ;        /* Start chain. */
  else
    {
      last->next = temp ;  /* Add record to existing chain. */
      last = temp ;        /* Point to the end of the chain. */
    }
}


check_comm(hostname, username, alias)  /* Check community list. */
char *hostname, *username, *alias ;
{
  struct comminfo *ctemp ;
 
  ctemp = communities ;      /* Try and find machine record for hostname. */
  while (ctemp != NULL)
    if (EQUAL(ctemp->community, hostname))
      {  
        add_alias(ctemp, username, alias) ;
        return ;
      }  
    else ctemp = ctemp->next ;
 
  ctemp = (struct comminfo *) Malloc(sizeof(struct comminfo)) ;
  ctemp->community = (char *) Malloc(strlen(hostname)) ;
  STRCPY(ctemp->community, hostname) ; 
  ctemp->people = NULL ; 
  ctemp->next = NULL ; 
 
  if (communities == NULL) communities = clast = ctemp ;  /* Start chain. */ 
  else 
    { 
      clast->next = ctemp ;   /* Add record to existing chain. */
      clast = ctemp ;         /* Point to end of chain. */
    }
  add_alias(ctemp, username, alias) ;
}


read_aliases()     /* Setup the hostname aliases subchains. */
{
  char alias[MAXLINE] ;      /* Alias for this community/username. */
  char hostname[MAXLINE] ;   /* This records' hostname. */
  char username[MAXLINE] ;   /* This records real username. */
  char *ptr1, *ptr2 ;
  FILE *fd ;

  if ((fd = fopen(peopfile, "r")) == NULL)   /* Open people file. */
    {
      FPRINTF(stderr,"%s: cannot open %s\n", progname, peopfile) ;
      return ;
    }
  while (fgets(nextline, MAXLINE, fd) != NULL)
    {
      ptr1 = index(nextline, '/') ;
      STRNCPY(hostname, nextline, (int) (ptr1-nextline)) ;
      hostname[(int) (ptr1-nextline)] = '\0' ;
      ptr2 = index(nextline, '=') ;
      STRNCPY(alias, ptr1+1, (int) (ptr2-ptr1-1)) ;
      alias[(int) (ptr2-ptr1-1)] = '\0' ;
      STRNCPY(username, ptr2+1, strlen(ptr2)-2) ;
      username[strlen(ptr2)-2] = '\0' ;
      check_comm(hostname, username, alias) ;
    }
  FCLOSE(fd) ;
}


read_machines()       /* Setup the chain of machine/community records. */
{
  char community[MAXLINE] ;   /* This records' community. */
  char machine[MAXLINE] ;     /* This records' machine name. */
  char *ptr ;
  FILE *fd ;

  if ((fd = fopen(machfile, "r")) == NULL)   /* Open machine file. */
    {
      FPRINTF(stderr,"%s: cannot open %s\n", progname, machfile) ;
      return ;
    }
  while (fgets(nextline, MAXLINE, fd) != NULL)
    {
      ptr = index(nextline, '=') ;
      STRNCPY(machine, nextline, (int) (ptr-nextline)) ;
      machine[(int) (ptr-nextline)] = '\0' ;
      STRNCPY(community, ptr+1, strlen(ptr)-2) ;
      community[strlen(ptr)-2] = '\0' ;
      add_machine(machine, community) ;
    }
  FCLOSE(fd) ;
}


struct recinfo *
rec_exists(community,username)    /* Check if record exists for mail item. */
char *community, *username ;
{
  struct recinfo *temp ;     /* Pointer to mail records used for chaining. */

  temp = recs ;
  while (temp != NULL)
    {
      if (EQUAL(temp->username, username) &&
          EQUAL(temp->community, community))
        return(temp) ;       /* Record found. */
      temp = temp->next ;    /* Point to next record. */
    }
  return(NULL) ;
}


remove_record(thisrec)        /* Remove this record from the chain. */
struct recinfo *thisrec ;
{
  if (thisrec->community != NULL) free(thisrec->community) ;
  if (thisrec->username != NULL) free(thisrec->username) ;
  if (thisrec->iconname != NULL) free(thisrec->iconname) ;
  free((char *) thisrec) ;
}
