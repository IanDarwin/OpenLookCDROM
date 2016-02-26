/*
 * This file is provided for unrestricted use
 * provided that this legend is included on all tape media
 * and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#include <X11/Xos.h>
#include <ctype.h>
#include <sys/stat.h>
#include <pwd.h>
#ifdef SVR4
#include <limits.h>
#include <sys/systeminfo.h>
#endif
#ifdef linux
#include <sys/utsname.h>
#endif
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notice.h>
#define _OTHER_TEXTSW_FUNCTIONS /* For additional defines in textsw.h */
#include <xview/textsw.h>
#include <xview/window.h>
#include <xview/xv_xrect.h>
#include <xview/notify.h>
#include <xview/font.h>
#include <gfm_ui.h>
#include <gfm.h>
#include <gio.h>

#include "xvnews_ui.h"
#include "xvnews.h"
#include "codes.h"
#include "patchlevel.h"

extern struct globals *Global;

STATIC_FUNCTION( int checkHeader, (xvnews_post_popup_objects *,
				   char *, char *));
STATIC_FUNCTION( int confirm_cancel, (xvnews_post_popup_objects *));
#ifndef USE_INEWS_TO_POST
STATIC_FUNCTION( void errorPost, (xvnews_post_popup_objects *, char *));
#endif
STATIC_FUNCTION( char *replyAuthor, (xvnews_xvnews_window_objects *));

extern void get_news_header(ip, flg)
xvnews_post_popup_objects	*ip;
int	flg;
{
	char	*head, *sig;
	char	file1[128];
	Textsw_index	point;
	xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(ip->post_popup, XV_KEY_DATA, POST_PARENT, NULL);

	xv_set(ip->post_window, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY,
		TEXTSW_LOWER_CONTEXT, -1, NULL);
	head = newsHead(bp, flg, &point);
	textsw_insert(ip->post_window, head, strlen(head));

#ifdef ADD_SIGNATURE
	sprintf(file1, "%s/.signature", Global->home);
	if (Global->group) {
		char	file2[128];

		sprintf(file2, "%s/%s/.signature",
			 Global->newsdir, groupDir(Global->group->newsgroup));
		sig = addSignature(file1, file2);
	} else
		sig = addSignature(file1, NULL);
#else
	sig = NULL;
#endif	

	if (sig != NULL) {
		textsw_insert(ip->post_window, sig, strlen(sig));
		free(sig);
	}

	xv_set(ip->post_window, TEXTSW_INSERTION_POINT, point - 1, 
		TEXTSW_LOWER_CONTEXT, 0, NULL);
}

extern int headToFile(head, file)
char	*head, *file;
{
	FILE	*ifp_out;

	if ((ifp_out = fopen(file, "a")) == NULL)
		return -1;

	fprintf(ifp_out, "%s", head);
	fclose(ifp_out);
	return 0;
}

extern void get_mail_header(ip, flg)
xvnews_post_popup_objects	*ip;
int	flg;
{
  char	buff[256], *head;
  	char	*indent, *old, *err;
	char	file[128], *sig = NULL;
	int	art;
	Textsw_index	point;
	xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(ip->post_popup, XV_KEY_DATA, POST_PARENT, NULL);
	xvnews_props_objects *prp = (xvnews_props_objects *)xv_get(bp->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);

	head = mailHead(bp, flg, (int *)&point);
	textsw_insert(ip->post_window, head, strlen(head));
	xv_set(ip->post_window, TEXTSW_INSERTION_POINT, point - 1, NULL);
#ifdef ADD_SIGNATURE
	sprintf(file, "%s/.signature", Global->home);
	sig = addSignature(file, NULL);
#endif
	if (flg == MAIL_FORWARD) { 
		sprintf(buff, "In article <%s>, %s writes:\n",
			get_messageid(0), (char *)currentAuthor(0));
		textsw_insert(ip->post_window, buff, strlen(buff));
		old = (char *)xv_get(bp->articles_list, PANEL_LIST_STRING,
			xv_get(bp->articles_list, PANEL_LIST_FIRST_SELECTED), NULL);
		sscanf(old, "%d", &art);
		indent = (char *)xv_get(prp->indent_text, PANEL_VALUE, NULL);
		if ((err = include_news(art, "/tmp/.xvnews.include", indent)) != NULL) 
			xv_set(ip->post_window, FRAME_LEFT_FOOTER, err, NULL);
		else {
			xv_set(ip->post_window, TEXTSW_LOWER_CONTEXT, -1, NULL);
			xv_set(ip->post_window,
				TEXTSW_INSERT_FROM_FILE,    "/tmp/.xvnews.include",
				NULL);
			xv_set(ip->post_window, TEXTSW_LOWER_CONTEXT, 0, NULL);
			unlink("/tmp/.xvnews.include");
			if (sig != NULL)
				textsw_insert(ip->post_window, sig, strlen(sig));
			xv_set(ip->post_window, TEXTSW_INSERTION_POINT, 4, NULL);
		}
	} else
		if (sig != NULL) {
		  textsw_insert(ip->post_window, sig, strlen(sig));
		  xv_set(ip->post_window, TEXTSW_INSERTION_POINT,
			 point - 1, NULL);
		}
	if (sig != NULL)
		free(sig);
	xv_set(ip->post_window, TEXTSW_LOWER_CONTEXT, 0, NULL);
	textsw_normalize_view(ip->post_window, 0);
}
	
extern char *
mailHead(ip, flg, point)
xvnews_xvnews_window_objects	*ip;
int flg;
int *point;
{
  static char	header[2048];
  char	 *subject;

  subject = (char *)currentSubject(ip, xv_get(ip->articles_list,
					      PANEL_LIST_FIRST_SELECTED));

	if (flg == MAIL_REPLY) {
		if (toupper(subject[0]) == 'R' && toupper(subject[1]) == 'E' &&
                   subject[2] == ':')
			sprintf(header, "To: %s\nSubject: Re:%s\nCc: \n\n\n",
				replyAuthor(ip), &subject[3]);
		else
			sprintf(header, "To: %s\nSubject: Re: %s\nCc: \n\n\n",
				replyAuthor(ip), subject);
	} else
		sprintf(header, "To: \nSubject: %s\nCc: \n\n\n", subject);

	*point = strlen(header);

	return header;
}
	

char *
getname(engr)
        char    *engr;
{
static	char     fullname[64];
	char	 *a;
        struct passwd *pw;

	if (getenv("NAME") != NULL)
		return (char *)getenv("NAME");
        if ((pw = getpwnam(engr)) == NULL)
                strcpy(fullname,"");
        else
                strncpy(fullname, pw->pw_gecos, 63);
	if ((a = strchr(fullname, '&')) != NULL) {
		strcpy(a, (char *)cuserid(NULL));
		a = (char *)strstr(fullname, (char *)cuserid(NULL));
		*a = toupper(*a);
	}
	if ((a = strchr(fullname, ',')) != NULL)
		*a = '\0';
        return(fullname);
}

static int confirm_cancel(pp)
xvnews_post_popup_objects       *pp;
{

	return notice_prompt(pp->post_controls, NULL,
                NOTICE_MESSAGE_STRINGS, "Do you really want to cancel?",
                NULL,
        NOTICE_BUTTON_YES,      "Cancel Post",
        NOTICE_BUTTON_NO,       "Abort Cancel",
        NULL);
}

extern void cancel_message(ip)
xvnews_post_popup_objects	*ip;
{
	int	result;

	result = confirm_cancel(ip); 

	if (!result)
		return;

	xv_destroy_safe(ip->post_popup);
}

extern void deliver_message(ip)
xvnews_post_popup_objects       *ip;
{
  char	*mbuf;
  int i, lines, post;
  Textsw_index    len;
  FILE		*pipe;
#ifdef USE_INEWS_TO_POST
#else
  static char message[MAX_MESSAGE_LEN];
  int result;
#endif

	xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(ip->post_popup, XV_KEY_DATA, POST_PARENT, NULL);

	xv_set(ip->post_window, TEXTSW_INSERTION_POINT, TEXTSW_INFINITY, NULL);
	len = (Textsw_index)xv_get(ip->post_window, TEXTSW_INSERTION_POINT);

	if (len > 0) {
                if ((mbuf = (char *)malloc(len+8)) == NULL) {
			xvnews_err(bp, "Malloc failed for posting!\n");
			return;
		}
		memset(mbuf, '\0', len+1);
                len = (Textsw_index)xv_get(ip->post_window,
                       TEXTSW_CONTENTS, 0, mbuf, len);
                if (len > 0) {
			i = strlen(mbuf);
			post = xv_get(ip->post_popup, XV_KEY_DATA, POST_TYPE);
			if (post == NEWS_POST || post == NEWS_REPLY) {
				if (checkHeader(ip, mbuf, "Newsgroups") ||
					checkHeader(ip, mbuf, "Subject")) {
					window_bell(ip->post_popup);
					free(mbuf);
					return;
			}
			lines = textsw_file_line_count(ip->post_window, mbuf);
			mbuf = checkPeriods(mbuf, lines);
#ifdef USE_INEWS_TO_POST
			if ((pipe = popen(INEWS_PROGRAM, "w")) == NULL) {
			    xvnews_err(bp,
				       "Couldn't open a pipe to inews!\n");
			    free(mbuf);
			    return;
			}
		        fprintf(pipe, "%s\n", mbuf);
			pclose(pipe);
			  
			xvnews_err(bp, 
				   "Article passed to Inews for posting ...\n");
#else
			put_server("POST");
			result = get_server(message, sizeof(message));
			if (result != CONT_POST) { 
				reconnect_server();
				put_server("POST");
				result = get_server(message, sizeof(message));
                                if (result != CONT_POST) { 
                                        xv_set(ip->post_popup,  FRAME_LEFT_FOOTER,
                                             "Posting not allowed!\n", NULL);
                                        window_bell(ip->post_popup); 
                                        free(mbuf); 
                                        return; 
				}
                        }
			put_server("%s", mbuf);
			put_server("\n.\n");
			result = get_server(message, sizeof(message));
			if (result != POSTED_OK) {
				if (result != ERR_POSTFAIL) {
					reconnect_server();
					put_server("POST");
                                	get_server(message, sizeof(message));
                                	put_server("%s", mbuf);
					put_server("\n.\n");
                                	result = get_server(message,
							    sizeof(message));
				}
				if (result != POSTED_OK) {
					errorPost(ip, message);
					free(mbuf);
					return;
				}
			}
			xvnews_err(bp, "Article Posted\n");
#endif /* USE_INEWS_TO_POST */

			filePost(ip, bp, mbuf);
		} else {
				if ((pipe = popen(MAIL_PROGRAM, "w")) == NULL) {
					xvnews_err(bp,
					   "Couldn't open a mail pipe!\n");
					free(mbuf);
					return;
				}
				fprintf(pipe, "%s\n", mbuf);
				xvnews_err(bp, "Mail sent\n");
				pclose(pipe);
			}
		}
                free(mbuf);
        }
	textsw_reset(ip->post_window, 0, 0);
	xv_destroy_safe(ip->post_popup);
}

extern char *
include_news(art, file, indent)
int art;
char	*file, *indent;
{
static	char	article[MAX_ARTICLE_LEN];
	int	status;
	FILE	*ifp_out;

	if ((ifp_out = fopen(file, "a")) == NULL) {
		sprintf(article, "Unable to open file %s!\n", file);
		return article;
	}
	put_server("BODY");
	status = get_server(article, sizeof(article));
	if (status != OK_BODY) {
		reconnect_server();
		put_server("BODY %d", art);
		status = get_server(article, sizeof(article));
		if (status != OK_BODY) 
			return "Cannot include article, news server problems!\n";
	}
	
	for (;;) {
		get_server(article, sizeof(article));
		if (article[0] == '.' && article[1] == '\0')
			break;
		else 
				fprintf(ifp_out, "%s%s\n", indent, article);
	}
	fprintf(ifp_out, "\n");
	fclose(ifp_out);

	return NULL;
}

extern char *
get_messageid()
{
	char	*c, article[MAX_ARTICLE_LEN];
static	char	number[80];
	int	status;

	put_server("STAT");
	status = get_server(article, sizeof(article));
	if (status != OK_NOTEXT) {
		reconnect_server();
		put_server("STAT %d", Global->article);
		status = get_server(article, sizeof(article));
		if (status != OK_NOTEXT) 
			return NULL;
	}

	c = strchr(article, '<');
	sscanf(++c, "%[^>]s", number);

	return number;
}

/*
 * This function retrieves the article-number from the currently
 * selected group.
 */

STATIC int get_art_num(ip)
  xvnews_xvnews_window_objects *ip;
{
  int row;
  char *str;
  int art;
  
  row = (int)xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED, NULL);
  str = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row, NULL);
  sscanf(str, "%d", &art);
  return art;
}

STATIC char *get_header_field(fieldname, art)
  char *fieldname;
  int art;
{
  char	message[MAX_MESSAGE_LEN];
  static char *c, follow[MAX_MESSAGE_LEN];
  int status;

  assert( fieldname );
  assert( art > 0 );

  put_server("XHDR %s %d", fieldname, art);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		reconnect_server();
		return NULL;
	}

	while(*message != '.') {
		get_server(message, sizeof(message));
		if (*message != '.')
			strcpy(follow, message);
	}

	if (strstr(follow, "(none)")) {
	    return NULL;
	  }

  /* TODO: poster should be handeld properly!
     For now I ignore it... Should only be handled in Followup-To: */

	c = follow;
	while (isdigit(*c) || *c == ' ')
		c++;

	return c;
}

extern void includeFile(win, file)
Xv_window       win;
char	*file;
{
	char str[256], name[256];
	int	result, i = strlen(file), lines = textsw_file_line_count(win, NULL);
	int	size = 0;
	xvnews_post_popup_objects *ip = (xvnews_post_popup_objects *) xv_get(xv_get(win, XV_OWNER, NULL), XV_KEY_DATA, INSTANCE, NULL);
	xvnews_xvnews_window_objects    *bp = (xvnews_xvnews_window_objects *) xv_get(ip->post_popup, XV_KEY_DATA, POST_PARENT, NULL);
	struct stat	statbuf;

	if (stat(file, &statbuf)) {
		xvnews_err(bp, "%s: No such file!\n", file);
		return;
	}

	sprintf(str, "Would you like %s", file);
	result = notice_prompt(win, NULL, NOTICE_MESSAGE_STRINGS, str,
			"loaded as text or uuencoded?",
			NULL,
			NOTICE_BUTTON,      "text", 101,
			NOTICE_BUTTON,       "compress & uuencode", 102,
			NOTICE_BUTTON,       "uuencode", 103,
			NOTICE_BUTTON,       "cancel", 104,
			NULL);

	while(file[i] != '/')
		--i;
	strncpy(name, &file[i+1], strlen(file));
	switch (result) {
		case 101:
			strcpy(str, file);
			size = statbuf.st_size;
			break;
		case 102:
			sprintf(str,
				"%s -c %s | /usr/bin/uuencode %s.%s > /tmp/xvnews.uu\n",
				COMPRESS_PROGRAM, file, name,
				COMPRESS_EXTENSION);
			system(str);
			strcpy(str, "/tmp/xvnews.uu");
			if (!stat(str, &statbuf)) 
				size = statbuf.st_size;
			break;
		case 103:
			sprintf(str, "/usr/bin/uuencode %s %s > /tmp/xvnews.uu\n", file, name);
			system(str);
			strcpy(str, "/tmp/xvnews.uu");
			if (!stat(str, &statbuf)) 
				size = statbuf.st_size;
			break;
		default:
			return;
	}
	xv_set(win, TEXTSW_INSERT_FROM_FILE, str, NULL);
	unlink("/tmp/xvnews.uu");
	sprintf(name, "Loaded %d lines, %d bytes from file %s\n", textsw_file_line_count(win, NULL) - lines, size, file);
	xv_set(ip->post_popup, FRAME_LEFT_FOOTER, name, NULL);
}

static int checkHeader(ip, art, line)
xvnews_post_popup_objects       *ip;
char	*art;
char	*line;
{
	char error[64], *a, *c = strstr(art, line);
	Textsw_index    first = 0, last = TEXTSW_INFINITY;

	if (c == NULL)
		return 0;

	a = strchr(c, '\n');
	*a = '\0';

	if ((int)strlen(c) > (int)strlen(line) + 2) {
		*a = '\n';
		return 0;
	}

	sprintf(error, "No %s specified for article!\n", line);
	xv_set(ip->post_popup, FRAME_LEFT_FOOTER, error, NULL);

	if (textsw_find_bytes(ip->post_window, &first, &last, line,
			 strlen(line), 0) != -1)
		xv_set(ip->post_window, TEXTSW_INSERTION_POINT, last + 2, NULL);

	return 1;
}

extern int filePost(ip, bp, art)
xvnews_post_popup_objects       *ip;
xvnews_xvnews_window_objects    *bp;
char	*art;
{
	xvnews_props_objects *pp = (xvnews_props_objects *)xv_get(bp->xvnews_window, XV_KEY_DATA, PROPS_POPUP, NULL);
	char	*log = (char *)xv_get(pp->log_text, PANEL_VALUE, NULL);
	char	from[80], from_line[128];
	time_t	clock;
	FILE	*ifp_out;

	if (!strlen(log))
		return 0;

	if (ip != NULL && !xv_get(ip->log_choice, PANEL_VALUE, NULL))
		return 0;

	if ((ifp_out = fopen(log, "a")) == NULL) {
		xvnews_err(bp, "Article posted, cannot open %s to log posting!\n", log);	
		return -1;
	}

	time(&clock);
	strftime(from, 80, "%a %h %e %T %Y", localtime(&clock));
	sprintf(from_line, "From %s %s", (char *)cuserid(NULL), from);
	fprintf(ifp_out, "%s\n%s\n\n", from_line, art);
	fclose(ifp_out);

	return 1;
}

extern char *
checkPeriods(buf, lines)
char *buf;
int lines;
{
	char *c, *p, *new;

	if (strstr(buf, "\n.") == NULL)
		return buf;
	new = (char *)malloc(strlen(buf) + (lines + 2));
	p = new;
	c = buf;
	while (*c != '\0') {
		if (*c == '.' && *(c - 1) == '\n') 
			*p++ = '.';
		*p++ = *c++;
	}
	*p = '\0';
	free(buf);

	return new;
}

#ifndef USE_INEWS_TO_POST
STATIC void errorPost(ip, message)
xvnews_post_popup_objects       *ip;
char	*message;
{
	char	error[256];
	char *c = message;

	while(isdigit(*c++));
	sprintf(error, "Server rejected article! Error: %s\n", c);

	xv_set(ip->post_popup, FRAME_LEFT_FOOTER, error, NULL);
	window_bell(ip->post_popup);
}
#endif

STATIC char *
replyAuthor(ip)
xvnews_xvnews_window_objects    *ip;
{
static	char	author[128];
	char message[MAX_MESSAGE_LEN], *old;
	int	status, art, row = xv_get(ip->articles_list, PANEL_LIST_FIRST_SELECTED);

	old = (char *)xv_get(ip->articles_list, PANEL_LIST_STRING, row);
	sscanf(old, "%d", &art);
	put_server("XHDR reply-to %d", art);
	status = get_server(message, sizeof(message));
	if (status != OK_HEAD) {
		reconnect_server();
		put_server("XHDR reply-to %d", art);
		status = get_server(message, sizeof(message));
		if (status != OK_HEAD) 
			return NULL;
	}
	get_server(message, sizeof(message));
	sscanf(message, "%*d %s", author);
	while (*message != '.')
		get_server(message, sizeof(message));
	if (strstr(author, "(none)") != NULL)
		return (char *)currentAuthor(1);

	return author;

}

extern char *
newsHead(bp, flg, point)
xvnews_xvnews_window_objects    *bp;
int	flg;
Textsw_index *point;
{
  char host[64], *name;
  static char buff[BUFFERLEN], default_domain[] = DOMAIN;
  char *subject, *domain, domain_long[80], *follow;
  int art = 0;

#ifdef linux
  struct utsname utsbuf;
  uname(&utsbuf);
#endif       

#ifdef SVR4
  sysinfo(SI_HOSTNAME, host, 32);
#else
  gethostname(host, 32);
#endif

  /* 
   * Name can be the actual cuserid, or the MAIL_ALIAS as defined by
   * the user. Some might argue that this allows forgeries. I
   * laugh at them and argue that Internet allows forgeries, but
   * nobody has removed Internet either.
   */

  if (!(name = (char *)getenv("MAIL_ALIAS"))) {
    name = cuserid(NULL);
  }

  /* Start a default new buffer */
  strcpy(buff, "Distribution: \n");
  if (flg == NEWS_REPLY) {
    /* We are doing a followup to the current article */
    art = get_art_num(bp);
    if ((follow = get_header_field("Distribution", art))) {
      sprintf(buff, "Distribution: %s\n", follow);
    }
    else {
      sprintf(buff, "Distribution: \n");
    }

    if ((follow = get_header_field("Followup-to", art))) {
      sprintf(buff, "%sNewsgroups: %s\n", buff, follow);
    }
    else {
      if ((follow = get_header_field("Newsgroups", art))) {
	sprintf(buff, "%sNewsgroups: %s\n", buff, follow);
      }
      else
	sprintf(buff, "%sNewsgroups: %s\n", buff, Global->group->newsgroup);
    }
    sprintf(buff, "%sFollowup-To: \nReferences: <%s>\n",
	    buff, get_messageid());
  } else {
    /* We are composing a new article */
    if (Global->single) {
      sprintf(buff, "%sNewsgroups: %s\n", buff, Global->single);
    }
    else
      if (Global->group)
	sprintf(buff, "%sNewsgroups: %s\n", buff, Global->group->newsgroup);
      else {
	sprintf(buff, "%sNewsgroups: \n", buff);
	*point = strlen(buff);
      }
    sprintf(buff, "%sFollowup-To: \n", buff);
  }

  /* Get the domain so that we can generate From: and Reply-To: */
  if ((domain = (char *)getenv("DOMAIN"))) {
    /* Use DOMAIN env. var. */;
  }
  else{
    if (strlen(default_domain)) {
      domain = default_domain;
    }
    else{
      /* Get the domainname from the system */
#ifdef linux
      strcpy(domain_long, utsbuf.domainname);
#endif		   
#ifdef SVR4
      sysinfo(SI_SRPC_DOMAIN, domain_long, 32);
#else
      getdomainname(domain_long, 64);
#endif
      if (domain_long[0] == '+')
	domain_long[0] = '.';
      domain = strchr(domain_long, '.');
      if (domain == NULL)
	domain = domain_long;
      else
	++domain;
    }
  }
  sprintf(buff, "%sFrom: %s@%s (%s)\nReply-To: %s@%s\n", buff,
	  name, domain, getname(name), name, domain);

  /* Get the Organization: header */
  if (getenv("ORGANIZATION"))
    sprintf(buff, "%sOrganization: %s\n", buff, getenv("ORGANIZATION"));
  else
    sprintf(buff, "%sOrganization: %s\n", buff, ORGANIZATION);

#ifdef BETA
  sprintf(buff,
	  "%sX-Newsreader: xvnews %-1.1f Beta PL%d, use only for testing please.\n",
	  buff,	VERSION, PATCHLEVEL);
#endif

  if (flg == NEWS_REPLY) {
    subject = get_header_field("Subject", art);
    /*
      subject = (char *)currentSubject(bp, xv_get(bp->articles_list,
      PANEL_LIST_FIRST_SELECTED));
      */
    if (toupper(subject[0]) == 'R' && toupper(subject[1]) == 'E' &&
	subject[2] == ':')
      sprintf(buff, "%sSubject: Re:%s\nSummary: \nKeywords: \n\n\n",
	      buff, &subject[3]);	
    else
      sprintf(buff, "%sSubject: Re: %s\nSummary: \nKeywords: \n\n\n",
	      buff, subject);
  } else {
    sprintf(buff, "%sSubject: \nKeywords: \n\n\n", buff);
    if ((Global->group && flg != NEWS_REPLY) || Global->single)
      *point = strlen(buff) - 13;
  }

  if (flg == NEWS_REPLY)
    *point = strlen(buff);

  return buff;
}	

extern char *
addSignature(file1, file2)
char	*file1, *file2;
{
	char	aline[128], *sig, *result;
	FILE	*ifp_in = NULL;
	struct	stat	statbuf;

	if ((ifp_in = fopen(file2 == NULL ? file1:file2, "r")) == NULL) {
		ifp_in = fopen(file1, "r");
		stat(file1, &statbuf);
	} else
		stat(file2 == NULL ? file1:file2, &statbuf);

	if (ifp_in == NULL)
		return NULL;

	sig = (char *)malloc(statbuf.st_size + 2 + 
			     strlen(DEFAULT_SIGNATURE_SEPARATOR));
	strcpy(sig, DEFAULT_SIGNATURE_SEPARATOR);
	result = sig + strlen(DEFAULT_SIGNATURE_SEPARATOR);
	*result = '\0';
	while(fgets(aline, 127, ifp_in) != NULL) {
		aline[127] = '\0';
		sprintf(result, "%s%s", result, aline);
	}

	fclose(ifp_in);

	return sig;
}
