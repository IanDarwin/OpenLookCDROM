/* 
 *
 * Copyright (c) 1988, 1989, 1990, 1991, Ellen M. Sentovich and Rick L. Spickelmier.
 * 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose and without fee is hereby granted, provided 
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting 
 * documentation.
 * 
 */
#include <X11/Xos.h>
#include <stdio.h>
#include <xview/xview.h>
#include <xview/font.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/termsw.h>
#include <xview/text.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "utils.h"
#include "codes.h"

/*
 * get a list of author lines for the current group in the range
 *  'first' to 'last'
 *
 *   returns: void
 *
 * Note that XHDR is not part of the rfc977 standard, but is implemented
 * by the Berkeley NNTP server
 *
 * This code was also modified from xrn.
 *
 *
 */
extern void
getauthorlist(subjects, first, last, only_unread)
char *subjects[];
int first, last, only_unread;
{
  static char message[MAX_MESSAGE_LEN];
  char *author, *end, *brackbeg, *brackend;
  int count = 0, status, artnum;

  put_server("XHDR from %d-%d", first, last);
  status = get_server(message, sizeof(message));
  if (status != OK_HEAD) {
    reconnect_server();
    put_server("XHDR from %d-%d", first, last);
    status = get_server(message, sizeof(message));
    if (status != OK_HEAD)
      return;
  }
		

  for(;;) {

    get_server(message, sizeof(message));
	
    if (*message == '.') {
      break;
    }

    /*
     * message is of the form:
     *
     *    Number Author
     *
     *    201 ricks@shambhala (Rick L. Spickelmier)
     *    202 Jens Thommasen <jens@ifi.uio.no>
     *    203 <oea@ifi.uio.no>
     *    302 "Rein Tollevik" <rein@ifi.uio.no>
     *
     * must get the number since not all authors will be returned
     */

    /* Ignore articles already read */
    sscanf(message, "%d", &artnum);
    if (!is_unread(Global->group, artnum) && only_unread)
      continue;

    /* Can be made fancyer at the expence of extra cpu time */
    author = strchr(message, ' ');
    assert(author != NIL(char));
    author++;

	/* First check for case 1, user@domain ("name") -> name */

	brackbeg = strchr(message, '(');
	brackend = strchr(message, '\0') - sizeof(char);
	/* brackend now points at the last ')' if this is case 1 */
	if (brackbeg != NIL(char) && (brackend > brackbeg) &&
	    (*brackend == ')')) {
	    author = brackbeg + sizeof(char);

	    /* Remove surrounding quotes ? */
	    if ((*author == '"') && (*(brackend - sizeof(char)) == '"')) {
	      author++;
	      brackend--;
	    }

	    /* Rather strip trailing spaces here */

	    *brackend = '\0';
	} else {
	    /* Check for case 2, "name" <user@domain> -> name */
	    brackbeg = strchr(message, '<');
	    if (brackbeg != NIL(char) && (strchr(brackbeg, '>') != NIL(char))
		&& (brackbeg > message)) {
		while (*--brackbeg == ' ')
		  ;

		/* Remove surrounding quotes ? */
		if ((*brackbeg == '"') && (*author ==  '"')) {
		    *brackbeg = '\0';
		    author++;

		    /* Rather strip trailing spaces here */

		} else {
		    *++brackbeg = '\0';
		}
	    } else {

		/* 
		 * Check for case 3, <user@domain> -> usr@domain
	         *
		 * Don't need to do this again:
	         * brackbeg = strchr(message, '<');
                 */

		brackend = strchr(message, '>');
		if ((author == brackbeg) && (brackend != NIL(char))) {
		    author++;
		    *brackend = '\0';
		} else {
		    if ((end = strchr(author, ' ')) != NIL(char)) {
			*end = '\0';
		    }
		}
	    }
	}
      /*
       * do a final trimming - just in case the authors name ends
       * in spaces or tabs - it does happen
       */
      end = author + strlen(author) - 1;
      while ((end > author) && ((*end == ' ') || (*end == '\t'))) {
	*end = '\0';
	end--;
      }
      sprintf(subjects[count], "%-48.48s %-23.23s", subjects[count], author);
      subjects[count][SUBJECT_LENGTH + AUTHOR_LENGTH + 1] = '\0';
      ++count;
    }
    return;
}
