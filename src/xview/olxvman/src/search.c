/*
 * xman - X window system manual page display program.
 *
 * $XConsortium: search.c,v 1.11 89/10/03 15:58:01 kit Exp $
 * $oHeader: search.c,v 4.0 88/08/31 22:13:19 kit Exp $
 *
 * Copyright 1987, 1988 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Author:    Chris D. Peterson, MIT Project Athena
 * Created:   November 3, 1987
 */

#if ( !defined(lint) && !defined(SABER))
  static char rcs_version[] = "$Athena: search.c,v 4.7 89/01/06 15:59:02 kit Exp $";
#endif

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/notice.h>
#include <gdd.h>
#include "olxvman_ui.h"
#include "globals.h"

/* Map <CR> and control-M to goto begining of file. */

#define SEARCHARGS 10

FILE * DoManualSearch();
static int BEntrySearch();

/*	Function Name: DoSearch
 *	Description: This function performs a search for a man page or apropos
 *                   search upon search string.
 *	Arguments: man_globals - the pseudo globas for this manpage.
 *                 type - the type of search.
 *	Returns: none.
 */

#define LOOKLINES 6

/* 
 * Manual searches look through the list of manual pages for the right one
 * with a binary search.
 *
 * Apropos searches still exec man -k.
 *
 * If nothing is found then I send a warning message to the user, and do
 * nothing.
 */

FILE *
DoSearch(search_string, type)
char *search_string;
int type;
{
  char cmdbuf[BUFSIZ],*mantmp,*manpath;
  char tmp[BUFSIZ],path[BUFSIZ];
  char string_buf[BUFSIZ], cmp_str[BUFSIZ], error_buf[BUFSIZ];
  FILE * file;
  int count;
  Bool flag;

  /* If the string is empty or starts with a space then do not search */

  if ( streq(search_string,"") ) {
    PrintWarning("Search string is empty.");
    return(NULL);
  }

  if (search_string[0] == ' ') {
    PrintWarning("First character cannot be a space.");
    return(NULL);
  }

  strcpy(tmp, MANTEMP);		/* get a temp file. */
  mantmp = mktemp(tmp);

  /* set the command */

  manpath=getenv("MANPATH");
  if (manpath == NULL || streq(manpath,"") )
    strcpy(path,MANDIR);
  else
    strcpy(path,manpath);

  if (type == APROPOS) {
    char label[BUFSIZ];

    sprintf(label,"Results of apropos search on: %s", search_string);

#ifdef NO_MANPATH_SUPPORT	/* not quite correct, but the best I can do. */
    sprintf(cmdbuf, APROPOS_FORMAT, search_string, mantmp);
#else
    sprintf(cmdbuf, APROPOS_FORMAT, path, search_string, mantmp);
#endif

    if(system(cmdbuf) != 0) {	/* execute search. */
      sprintf(error_buf,"Something went wrong trying to run %s\n",cmdbuf);
      PrintWarning(error_buf);
    }

    if((file = fopen(mantmp,"r")) == NULL)
      PrintError("lost temp file? out of temp space?");

/* 
 * Since we keep the FD open we can unlink the file safely, this
 * will keep extra files out of /tmp. 
 */

    unlink(mantmp);

    sprintf(string_buf,"%s: nothing appropriate", search_string);

    /*
     * Check first LOOKLINES lines for "nothing appropriate".
     */
  
    count = 0;
    flag = FALSE;
    while ( (fgets(cmp_str, BUFSIZ, file) != NULL) && (count < LOOKLINES) ) {
      if ( cmp_str[strlen(cmp_str) - 1] == '\n') /* strip off the '\n' */
	  cmp_str[strlen(cmp_str) - 1] = '\0';

      if (streq(cmp_str, string_buf)) {
	flag = TRUE;
	break;
      }
      count++;
    }

    /*
     * If the file is less than this number of lines then assume that there is
     * nothing apropriate found. This does not confuse the apropos filter.
     */

    if (flag) {
      fclose(file);
      file = NULL;
      return(NULL);
    }
  
    fseek(file, 0L, 0);		/* reset file to point at top. */
  }
  else {			/* MANUAL SEACH */
    file = DoManualSearch(search_string);
    if (file == NULL) {
      sprintf(string_buf,"No manual entry for %s.", search_string);
      return(NULL);
    }
  }

  return(file);
}

/*	Function Name: DoManualSearch
 *	Description: performs a manual search.
 *	Arguments: man_globals - the manual page specific globals.
 *	Returns: the filename of the man page.
 */

#define NO_ENTRY -100

FILE * 
  DoManualSearch(string)
char * string;
{
    int e_num = NO_ENTRY;
    int i;
    
    /* search current section first. */
    
    i = CurrentSection;
    e_num = BEntrySearch(string, manual[i].entries, manual[i].nentries);
    
    /* search other sections. */
    
    if (e_num == NO_ENTRY) 
    {
	i = 0;			/* At the exit of the loop i needs to
				   be the one we used. */
	while ( TRUE ) 
	{
	    if (i == CurrentSection)
	      if (++i >= sections) return(NULL);
	    e_num = BEntrySearch(string, manual[i].entries, manual[i].nentries);
	    if (e_num != NO_ENTRY) break;
	    if (++i >= sections) return(NULL);
	}
    }
    
    return(FindManualFile(i, e_num));
}

/*	Function Name: BEntrySearch
 *	Description: binary search through entries.
 *	Arguments: string - the string to match.
 *                 first - the first entry in the list.
 *                 number - the number of entries.
 *	Returns: a pointer to the entry found.
 */

static int
BEntrySearch(string, first, number)
char * string;
char ** first;
int number;
{
  int check, cmp, len_cmp, global_number;
  char *head, *tail;
  
  global_number = 0;
  while (TRUE) {

    if (number == 0) {
      return(NO_ENTRY);		/* didn't find it. */
    }

    check = number/2;

    head = rindex(first[ global_number + check ], '/');
    if (head == NULL) 
      PrintError("index failure in BEntrySearch");
    head++;

    tail = rindex(head, '.');
    if (tail == NULL) 
      PrintError("index failure in BEntrySearch");

    cmp = strncmp(string, head, (tail - head));
    len_cmp = strlen(string) - (int) (tail - head);

    if ( cmp == 0 && len_cmp == 0) {
      return(global_number + check);
    }
    else if ( cmp < 0 || ((cmp == 0) && (len_cmp < 0)) ) 
      number = check;
    else /* cmp > 0 || ((cmp == 0) && (len_cmp > 0)) */ {
      global_number += (check + 1);
      number -= ( check + 1 );
    }
  }
}
