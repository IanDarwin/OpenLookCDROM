
/*
 *	@(#)mail_reader.c	1.32 12/16/94
 *
 *	(c) Copyright 1993-1994 by Mark Grant. All right reserved.
 *	The author assumes no liability for damages resulting from the 
 *	use of this software, even if the damage results from defects in
 *	this software. No warranty is expressed or implied.
 *
 *	NOTE: It is recommended that you compile this file with SAFE
 *	defined until you are sure that it is not going to trash your
 *	mail files !
 *
 *	This software is being distributed under the GNU Public Licence,
 *	see the file COPYING for more details.
 *
 *			- Mark Grant (mark@unicorn.com) 29/6/94
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <unistd.h>
#include <string.h>
#include <malloc.h>
#include <time.h>
#include <sys/param.h>
#include <errno.h>

#include "def.h"
#include "buffers.h"
#include "message.h"

/* Define line buffer sizes - this way either all routines will work ok,
   or none will */

#define LINE_LENGTH	512

/* Line variable is for general use */

static	char	*line;
static	int	line_length;
static	int	read_only = FALSE;

extern	char	default_mail_file [];
extern	FILE	*mail_fp;

#ifdef SYSV
void	usleep(n)
unsigned long n; 
{
	struct	timeval	t;

	t.tv_sec = n / 1000000;
	t.tv_usec = n % 1000000;

	(void) select (0, NULL, NULL, NULL, &t);
}
#endif

/* Unfortunately, we have to cope with people who use very, very long lines */

static	update_line (i)

{
	while  (i >= (line_length - 16)) {
		line_length += LINE_LENGTH;
		line = realloc (line, line_length);
	}
}

/* Read a line from the file */

static	read_line(fp,header)

FILE	*fp;
int	header;

{
	int	c,i=0;

	if (!line) {
		line_length = LINE_LENGTH;
		line = malloc (line_length);
	}

read_loop:

	do {
		c = getc(fp);
		if (c!= EOF && c!= '\n')
			line[i++] = c;

		/* Unfortunately, some people send VERY long lines !
		   otherwise we could check outside the loop... */

		if (i >= (line_length - 16)) {
			update_line (i);
		}

	} while (c != EOF && c!= '\n');

	/* Header lines can be spread out ! */

	if (c != EOF && header && (i > 1)) {
		c = getc(fp);

		/* If it's a space, we must be on a continuation line */

		if (c == ' ' || c== '\t') {
			line [i++] = '\n';
			line [i++] = c;

			goto read_loop;
		}
		else {
			if (c != EOF)
				ungetc (c,fp);
		}
	}

	line[i]=0;
}

/* Count the number of lines in a buffer */

static	int	count_lines(b,n)

byte	*b;
int	n;

{
	int	l;

	/* Firewall */

	if (!b)
		return NULL;

	/* Count the \n's */

	l = 0;
	while (*b && n) {
		if (*b == '\n')
			l++;

		b++;
		n--;
	}

	return l;
}

/* List of current and deleted messages */

MESSAGE_LIST	messages;
MESSAGE_LIST	deleted;

/* Add a message to the deleted list */

add_to_deleted(om) 

MESSAGE	*om;

{
	om->dnext = deleted.start;
	om->dprev = NULL;

	if (deleted.start) {
		deleted.start->dprev = om;
	}
	else
		deleted.end = om;

	deleted.start = om;
	deleted.number ++;

	om->flags |= MESS_DELETED;
}

/* Add a string to the header of the specified message */

static	void	add_header(m,s)

MESSAGE	*m;
char	*s;

{
	if (!m->header)
		m->header = new_buffer();

	add_to_buffer(m->header,s,strlen(s));
}

/* Add a string to the body of the specified message */

static	void	add_body(m,s)

MESSAGE	*m;
char	*s;

{
	if (!m->body)
		m->body = new_buffer();

	add_to_buffer(m->body,s,strlen(s));
}

/* ret_string is for general use */

static	char	ret_string[] = "\n";

/* These strings define PGP messages */

static	char	pgp_signed[] = "-----BEGIN PGP SIGNED MESSAGE";
static	char	pgp_encrypted[] = "-----BEGIN PGP MESSAGE";

/*
 * Take the sender string and work out the email address. Usually there
 * Are two forms, either foo@bar.com (Foobar) or Foobar <foo@bar.com>
 */

static	get_email_address(m)

MESSAGE	*m;

{
	char	address[256];
	char	*a,*s,*e;

	a = address;
	s = m->sender;

	/* Firewall */

	if (!s)
		return;

	/* Ignore spaces */

	while (*s == ' ' || *s == '\t')
		s++;

	/* Check for opening < */

	if (e = strchr(s,'<')) {
		e++;
		while (*e != '>' && *e != ' ' && *e) {
			*a++ = *e++;
		}
	}
	else {
		while (*s != ' ' && *s != '(' && *s)
			*a++ = *s++;
	}

	*a = 0;

	/* Copy it into the message */

	m->email = strdup(address);
}

static	char	*header_strdup(s)

char	*s;

{
	char	*temp;
	char	*d;

	temp = malloc (strlen(s)+2);

	d = temp;
	while (*s) {
		while (*s == '\n') {
			s++;
			while (*s == ' ' || *s == '\t')
				s++;
		}

		if (*s)
			*d++ = *s++;
	}

	*d = 0;
	return temp;
}

static	int	lines;

static	copy_and_join(s,d)

char	*s;
char	*d;

{

	while (*s == ' ')
		s++;

	while (*s) {
		if (*s != '\n')
			*d++ = *s;
		s++;
	}

	*d = 0;
}

/* Process a line from a message header */

static	process_header_line(line,m)

char	*line;
MESSAGE	*m;

{
	static	char	*temp = NULL;
	static	int	temp_length;

	if (!temp) {
		temp_length = LINE_LENGTH;
		if (temp_length < line_length)
			line_length = temp_length;

		temp = malloc (temp_length);
	}

	if (temp_length < line_length) {
		temp_length = line_length;
		temp = realloc (temp, temp_length);
	}

	/* Check for From: */

	if (!strncasecmp(line,"From:",5)) {
		if (m->sender)
			free_string (m->sender);
		copy_and_join(line+6,temp);
		m->sender = header_strdup(temp);
	}

	/* Check for Date: */

	else if (!strncasecmp(line,"Date:",5)) {
		if (m->header_date)
			free_string (m->header_date);
		copy_and_join(line+6,temp);
		m->header_date = header_strdup(temp);
	}

	/* Check for Subject: */

	else if (!strncasecmp(line,"Subject:",8)) {
		if (m->subject)
			free_string (m->subject);
		copy_and_join(line+9,temp);
		m->subject = header_strdup(temp);

		/* If the subject includes 'Digest' and
		   does not start with Re:, assume this
		   is a digest file */

		if (strncasecmp(m->subject,"Re:",3) &&
			strstr(m->subject,"Digest")) {
			m->flags |= MESS_DIGEST;
		}
	}

	/* Check for X-Lines: */

	else if (!strncasecmp(line,"X-Lines:",8)) {
		sscanf(line+8,"%d",&lines);
		m->lines = lines;
	}

	/* Check for Lines: */

	else if (!strncasecmp(line,"Lines:",6)) {
		sscanf(line+6,"%d",&lines);
		m->lines = lines;
	}

	/* Check for Status: */

	else if (!strncasecmp(line,"Status:",7)) {
		if (line[8] == 'R')
			m->status = MSTAT_READ;
		else
			m->status = MSTAT_UNREAD;
	}

	/* Check for Message-Id: */

	else if (!strncasecmp(line,"Message-Id:",11)) {
		if (m->message_id)
			free_string (m->message_id);
		copy_and_join(line+12,temp);
		m->message_id = header_strdup(temp);
	}

	/* Check for Content-Length */

	else if (!strncasecmp(line,"Content-Length:",16)) {
		copy_and_join(line+17,temp);
		m->size = atoi(temp);
	}

	/* Check for Encrypted: PGP */

	else if (!strncasecmp(line,"Encrypted: PGP",15)) {
		m->flags |= MESS_ENCRYPTED;
	}
}

static	time_t	get_time (m)

MESSAGE	*m;

{
#ifndef SYSV
	struct	tm	t;
#else
	struct	tm	*t;
#endif

	bzero (&t, sizeof (t));

	/* These are the only two common time formats I've come across */

#ifndef SYSV
	if (*(m->date) > '9') {
		if (m->date[20] > '9')  {
			strptime (m->date, "%a %h %d %T", &t);
			strptime (m->date+23, "%Y", &t);
		}
		else
			strptime (m->date, "%a %h %d %T %Y", &t);
	}
	else {
		if (m->date[7] == '1')
			strptime (m->date, "%d %b %Y %T", &t);
		else
			strptime (m->date, "%d %b %y %T", &t);
	}

	return timegm (&t);
#else
	t = getdate (m->date);

	if (t)
		return mktime (t);
	else
		return 0;
#endif
}

MESSAGE	*message_from_message(m)

MESSAGE	*m;

{
	MESSAGE	*newm;
	int	i;
	int	c;
	char	temp[256];
	char	*p,*d;
	char	*m_in;
	int	no_from;
	int	line_count = 0;
	int	not_pgp_message = FALSE;

	i=0;
	no_from = FALSE;

	if (m->decrypted)
		m_in = (char *) m->decrypted->message;
	else
		m_in = (char *)message_contents(m)->message;

	while (*m_in && *m_in != '\n') {
		line[i++] = *m_in++;
		if (i >= line_length-16)
			update_line (i);
	}

	line[i] = 0;
	if (*m_in)
		m_in++;

	if (strncmp(line,"From ",5)) {
		no_from = TRUE;
	}

	lines = (-1);

	newm = new_message();
	add_header(newm,line);
	add_header(newm,ret_string);
	set_mem_message (newm);

	if (!no_from) {

		/* Read the sender from the From line */

		p = line+5;
		d = temp;

		while (*p && *p != '\n' && *p != ' ') {
			*d++ = *p++;
			if (p >= line + (line_length-16))
				update_line (p - line);
		}

		*d = 0;

		newm->sender = strdup(temp);

		/* Read the date from the From line */

		if (*p == ' ') {

			p++;
			d = temp;

			while (*p && *p != '\n') {
				*d++ = *p++;
			}

			*d = 0;

			newm->date = strdup(temp);
			newm->header_date = strdup (temp);
		}
	}
	else
		newm->date = strdup(m->date);

	newm->number = m->number;
	newm->status = MSTAT_NONE;
	newm->list_pos = m->list_pos;

	line[0]= ' ';
	while (*m_in && line[0]) {
		i = 0;
		while (*m_in && *m_in != '\n') {
			line[i++] = *m_in++;
			if (i >= line_length-16)
				update_line (i);
		}
		line[i]=0;
		if (*m_in)
			m_in++;

		if (line[0]) {
			add_header(newm,line);
			add_header(newm,ret_string);
			process_header_line(line,newm);
		}
	}

	get_email_address(newm);

	/* If some lines, read them */

	if (*m_in) {

		while (*m_in) {
			i = 0;
			while (*m_in && *m_in != '\n') {
				line[i++] = *m_in++;
				if (i >= line_length-16)
					update_line (i);
			}
			line[i] = 0;
			if (*m_in)
				m_in++;

			if (!not_pgp_message) {
				if (!strncmp(line,pgp_signed,
					strlen(pgp_signed))) {
					newm->flags |= MESS_SIGNED;
				}
				if (!strncmp(line,pgp_encrypted,
					strlen(pgp_encrypted))) {
					newm->flags |= MESS_ENCRYPTED;
				}
				for (i = 0; line[i]; i++)
					if (line[i] != ' ' && line[i] != '\n')
						not_pgp_message = TRUE;
			}
				
			add_body(newm,line);
			add_body(newm,ret_string);

			line_count++;
		}
	}

	if (no_from) {
		BUFFER	*b;

		b = new_buffer();

		sprintf(temp,"From %s %s\n",newm->email,newm->date);

		add_to_buffer(b,temp,strlen(temp));
		add_to_buffer(b,newm->header->message,
			newm->header->length);

		free_buffer(newm->header);
		newm->header = b;
	}

	newm->size = newm->body->length;
	newm->lines = line_count;
	newm->time_secs = get_time (m);

	return newm;
}

#define LINE_COUNT	0
#define CONTENT_LENGTH	1
#define FROM_LINE	2

static	time_t	last_mail_read;
static	long	last_pos;
static	char	last_file [MAXPATHLEN];

read_mail_from_file(fp,kill_messages)

FILE	*fp;
int	kill_messages;

{
	int	i;
	int	c;
	MESSAGE	*m;
	BUFFER	*mb;
	static	char	temp[LINE_LENGTH];
	char	*p,*d;
	int	killed = FALSE;
	int	not_pgp_message = FALSE;
	int	method;
	int	finished = FALSE;
#ifdef ACCEPT_PATH
	int	no_from = FALSE;
#endif

	i=0;
	mb = new_buffer ();

	if (!line) {
		line_length = LINE_LENGTH;
		line = malloc (line_length);
	}

	line[0]=0;

	while (!feof(fp) && strncmp(line,"From ",5)) {
#ifdef ACCEPT_PATH
		if (!strncmp(line, "Path:", 5)) {
			no_from = TRUE;
			break;
		}
		else 
#endif
			read_line(fp,TRUE);
	}

	/* Ok, found the first message */

	while (!feof(fp)) {

		i++;

		lines = (-1);
		killed = FALSE;
		not_pgp_message = FALSE;
		method = LINE_COUNT;

		m = new_message();
		clear_buffer (mb);

#ifdef ACCEPT_PATH
		if (!no_from) {
#endif
			add_header(m,line);
			add_header(m,ret_string);
#ifdef ACCEPT_PATH
		}
#endif

		m->size = (-1);

		/* Read the sender from the From line */

		p = line+5;
		d = temp;

		while (*p && *p != '\n' && *p != ' ') {
			*d++ = *p++;
		}

		*d = 0;

		m->sender = strdup(temp);
		m->number = i;
		m->status = MSTAT_NONE;

		/* Read the date from the From line */

#ifdef ACCEPT_PATH
		if (!no_from) {
#endif
		if (*p == ' ') {

			p++;
			d = temp;

			while (*p && *p != '\n') {
				*d++ = *p++;
			}

			*d = 0;

			m->date = strdup(temp);
		}
#ifdef ACCEPT_PATH
		}
#endif

		line[0]= ' ';
		while (!feof(fp) && line[0]) {
			read_line(fp, TRUE);

			if (line[0]) {
				add_header(m,line);
				add_header(m,ret_string);
				process_header_line(line,m);
			}
		}

		if (lines < 0) {
			if (m->size < 0)
				method = FROM_LINE;
			else
				method = CONTENT_LENGTH;
		}

		get_email_address(m);

#ifdef ACCEPT_PATH
		if (no_from) {
			BUFFER	*b;

			b = new_buffer ();

			if (!m->date) {
				m->date = strdup (m->header_date);
			}

			sprintf (line, "From %s %s\n", m->email,
				m->date);

			add_to_buffer (b, line, strlen (line));
			add_to_buffer (b, m->header->message,
				m->header->length);
				
			free_buffer (m->header);
			m->header = b;
		}
#endif

		if (kill_messages && kill_user(m->email))
			killed = TRUE;

		if (maybe_cfeed(m->email))
			m->flags |= MESS_CFEED;

		/* If some lines, read them */

		m->offset = ftell (mail_fp);

		if (lines > 0 || method != LINE_COUNT) {

			finished = FALSE;

			if (method == CONTENT_LENGTH) {
				mb->message = (byte *)malloc(m->size+2);

				fread (mb->message,
					m->size, 1, fp);
				mb->length = m->size;
				mb->size = m->size+2;

				mb->message[m->size] = 0;
				m->lines = count_lines (mb->message,
					mb->length);

				finished = TRUE;
			}

			while (!finished) {
				read_line(fp,FALSE);

				if (method == FROM_LINE) {
					if (!strncmp(line,"From ",5) ||
#ifdef ACCEPT_PATH
						!strncmp (line, "Path:", 5) ||
#endif
						feof(fp)) {
						finished = TRUE;
						continue;
					}
					m->lines ++;
				}

				if (!not_pgp_message) {
					if (!strncmp(line,pgp_signed,
						strlen(pgp_signed))) {
						m->flags |= MESS_SIGNED;
					}
					if (!strncmp(line,pgp_encrypted,
						strlen(pgp_encrypted))) {
						m->flags |= MESS_ENCRYPTED;
					}
					for (i = 0; line[i]; i++)
						if (line[i] != ' ' && line[i] != '\n')
							not_pgp_message = TRUE;
				}
				
				add_to_buffer(mb,line, strlen (line));
				add_to_buffer(mb,ret_string,
					strlen (ret_string));

				if (method == LINE_COUNT) {
					finished = (--lines <= 0);
				}
				else if (method == CONTENT_LENGTH) {
					finished = (mb->length >= 
						m->size);
				}
			}

			m->size = mb->length;
		}

		if (!killed) {
			add_to_message_list_end (&messages,m);
		}
		else {
			add_to_deleted (m);
		}

		set_file_message (m);

		m->time_secs = get_time (m);

		/* Look for next From line */

#ifdef ACCEPT_PATH
		no_from = FALSE;
#endif
		while (!feof(fp) && strncmp(line,"From ",5)) {
#ifdef ACCEPT_PATH
			if (!strncmp (line, "Path:", 5)) {
				no_from = TRUE;
				break;
			}
#endif
			read_line(fp,TRUE);
		}
	}

	last_mail_read = time(0);
	last_pos = ftell (fp);

	clearerr(fp);
	free_buffer (mb);
}

is_new_mail ()

{
	struct	stat	s_buf;

	if (!mail_fp)
		return FALSE;

	fstat (fileno(mail_fp), &s_buf);

	return (last_mail_read < s_buf.st_mtime);
}

/* Locking code doesn't work at the moment - it was intended to prevent
   problems when the user starts up a mailtool at the same time, but the
   mailtool program seems to ignore locks.
*/

close_mail_file ()

{
	if (mail_fp) {
#ifdef USE_LOCKING
		flock (fileno (mail_fp), LOCK_UN);
#ifdef USE_LOCKF
		fseek (mail_fp, 0l, 0);
		lockf (fileno (mail_fp), F_ULOCK, 0);
#endif
#endif
		fclose (mail_fp);
		mail_fp = NULL;

		bzero (last_file, MAXPATHLEN);
	}
}

int	read_mail_file(s)

char	*s;

{
	close_mail_file ();

	strcpy (last_file, s);

	errno = 0;
	mail_fp = fopen(s, "r+t");
	if (!mail_fp) {
		mail_fp = fopen (s, "rt");
		if (!mail_fp) {
			return -1;
		}
		read_only = TRUE;
	}
	else
		read_only = FALSE;

#ifdef USE_LOCKING
#ifdef USE_LOCKF
	if (flock (fileno (mail_fp), LOCK_EX|LOCK_NB) < 0 ||
		lockf (fileno (mail_fp), F_TLOCK, 0) < 0) {
#else
	if (flock (fileno (mail_fp), LOCK_EX|LOCK_NB) < 0) {
#endif
		if (!confirm_unlocked_read()) {
			fclose (mail_fp);
			mail_fp = NULL;
			return -1;
		}
	}
#endif

	read_mail_from_file(mail_fp, !strcmp(s, default_mail_file));

	return 0;
}

read_new_mail()

{
	if (!mail_fp)
		return;

	fseek (mail_fp, last_pos, 0);

	read_mail_from_file(mail_fp, reading_file (default_mail_file));
}

int	write_buffer_to_mail_file(b,to,cc,sub,s)

BUFFER	*b;
char	*to,*sub;
char	*s,*cc;

{
	FILE	*fp;
	time_t	t;
	int32	lines;
	int32	i;

	fp = fopen(s,"a+t");

	if (fp == NULL) {
		return -1;
	}

	lines = count_lines(b->message,b->length);

	t = time(0);
	fprintf(fp, "From %s %s",cuserid(NULL),ctime(&t));
	fprintf(fp, "To: %s\n",to);
	if (cc && *cc)
		fprintf(fp, "Cc: %s\n",cc);
	if (sub && *sub)
		fprintf(fp,"Subject: %s\n",sub);
	fprintf(fp, "Date: %s",ctime(&t));
	fprintf(fp, "Content-Length: %d\n",b->length);
	fprintf(fp, "X-Lines: %d\n",lines);
	
	putc('\n',fp);
	for (i=0; i<b->length;i++) {
		putc(b->message[i],fp);
	}
	putc('\n',fp);

	fclose(fp);

	return 1;
}

int	append_message(m,fp,save_all)

MESSAGE	*m;
FILE	*fp;
int	save_all;

{
	char	*m_in,*l,*d;
	int	lines;
	char	temp [LINE_LENGTH];
	BUFFER	*body;

	m_in = (char *)m->header->message;

	while (*m_in) {
		l = line;

		/* Split into (logical) lines */

	continue_scanning:
		while (*m_in && *m_in != '\n')
			*l++ = *m_in++;
		if (*m_in) {
			m_in++;
			if (*m_in == ' ' || *m_in == '\t') {
				*l++ = '\n';
				goto continue_scanning;
			}
		}

		*l = 0;

		/* See if we want to ignore this line */

		l = line;
		d = temp;

		while (*l && *l != ':') {
			*d++ = *l++;
		}
		*d = 0;
	
		if (!ignore_line(temp) || save_all) 
			if (strncasecmp(line,"Status:",7) &&
				strncasecmp(line,"X-Lines:",8) &&
				strncasecmp(line,"Content-Length:",15)) {
				fprintf(fp,"%s\n",line);
			}
	}

	body = message_contents (m);
	lines = count_lines(body->message,body->length);

	fprintf(fp,"Content-Length: %d\n",body->length);
	fprintf(fp,"X-Lines: %d\n",lines);

	switch (m->status) {

		case MSTAT_UNREAD:
		l = "O";
		break;

		case MSTAT_READ:
		l = "RO";
		break;

		default:
		l = NULL;

	}

	if (l)
		fprintf(fp,"Status: %s\n",l);

	if (body && body->message) 
		fprintf(fp,"\n%s\n",body->message);
	else
		fprintf(fp, "\n\n");
}

int	append_message_to_file(m,s,save_all)

MESSAGE	*m;
char	*s;
int	save_all;

{
	FILE	*fp;
	long	l;
	char	lock_file [MAXPATHLEN];
	int	fd;

	sprintf(lock_file,"%s.lock",last_file);

	/* Wait for lock file */

	while ((fd = creat(lock_file,0)) < 0) {
		usleep (50000);
	}
	close (fd);

	errno = 0;
	fp = fopen (s,"a+t");

	if (!fp) {
		printf("Fopen failed : errno = %d\n",errno);
		return -1;
	}

	l = ftell (fp);
	append_message (m, fp, save_all);

	/* Check for errors and truncate if found */

	fflush (fp);
	if (ferror (fp)) {
		ftruncate (fileno(fp), l);
		fclose (fp);
		unlink (lock_file);

		return -1;
	}

	fclose (fp);
	unlink (lock_file);

	return 0;
}

/* Replace the specified message with the new one */

void	replace_message_with_message(m,newm)

MESSAGE	*m,*newm;

{
	if (m->prev) 
		m->prev->next = newm;
	
	if (m->next)
		m->next->prev = newm;

	if (m == messages.start)
		messages.start = newm;

	if (m == messages.end)
		messages.end = newm;

	newm->prev = m->prev;
	newm->next = m->next;

	m->prev = m->next = NULL;
}

/* Is the buffer passed in a valid mail message ? */

int	is_mail_message(b)

BUFFER	*b;

{
	char	*i,*l;

	/* Firewall */

	if (!b || !b->message)
		return FALSE;

	/* Check for 'From ' or 'Received: from' on first line */

	i = (char *)b->message;
	l = line;

	while (*i && *i!='\n')
		*l++ = *i++;

	if (!strncmp(line,"From ",5) || !strncmp(line,"Received: from",14))
		return TRUE;

	return FALSE;
}

int	reading_file (s)

char	*s;

{
	return !strcmp(s, last_file);
}

int	save_changes ()

{
	char	tmp_file [MAXPATHLEN];
	char	lock_file [MAXPATHLEN];
#ifdef SAFE
	char	save_file [MAXPATHLEN];
#endif
	FILE	*tmp_fp;
	int	fd, c;
	MESSAGE	*m;

	if (!mail_fp)
		return;

	if (read_only) {
		if (read_only_notice_proc ())
			close_mail_file ();
		return;
	}

	/* Create path names */

	sprintf(lock_file,"%s.lock",last_file);
	sprintf(tmp_file,"%s.%d.tmp",last_file,getpid());
#ifdef SAFE
	sprintf(save_file,"%s.save",last_file);
#endif

	/* Wait for lock file */

	while ((fd = creat(lock_file,0)) < 0) {
		usleep (50000);
	}
	close (fd);

	/* Open the temporary file */

	tmp_fp = fopen (tmp_file, "wt");
	
	if (!tmp_fp) {
		unlink (lock_file);
		return -1;
	}

	m = messages.start;

	while (m) {
		if (!(m->flags & MESS_DELETED)) {
			if (m->status == MSTAT_NONE)
				m->status = MSTAT_UNREAD;
			append_message (m,tmp_fp,TRUE);
		}
		m = m->next;
	}

	/* If there's new mail, append it to the end of the file */

	clearerr (mail_fp);
	fseek (mail_fp, last_pos, 0);

	c = getc (mail_fp);

	while (c != EOF) {
		putc (c, tmp_fp);
		c = getc (mail_fp);
	}

	/* Check for errors */

	fflush (tmp_fp);
	if (ferror(tmp_fp)) {
		fclose (tmp_fp);
		unlink (tmp_file);
		unlink (lock_file);

		return -1;
	}

	/* Close the temporary file */

	fclose (tmp_fp);

	/* Rename the temporary file to the real file */

#ifdef SAFE	
	unlink (save_file);
	rename (last_file, save_file);
#else
	unlink (last_file);
#endif
	rename (tmp_file, last_file);

	close_mail_file ();

	/* Remove the lock file */

	unlink (lock_file);

	return 0;
}

char	*current_mail_file ()

{
	return	last_file;
}

int	is_mail_file_open ()

{
	if (mail_fp)
		return TRUE;

	return FALSE;
}

