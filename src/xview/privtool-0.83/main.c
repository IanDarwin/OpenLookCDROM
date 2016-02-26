
/*
 * @(#)main.c	1.19 12/16/94
 *
 *	(c) Copyright 1993-1994 by Mark Grant. All right reserved.
 *	The author assumes no liability for damages resulting from the 
 *	use of this software, even if the damage results from defects in
 *	this software. No warranty is expressed or implied.
 *
 *	This software is being distributed under the GNU Public Licence,
 *	see the file COPYING for more details.
 *
 *			- Mark Grant (mark@unicorn.com) 29/6/94
 *
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <malloc.h>

#include "def.h"
#include "buffers.h"
#include "message.h"
#include "mailrc.h"

char	default_mail_file[MAXPATHLEN];
FILE	*mail_fp;

static	LIST	mailrc;
static	LIST	alias;
static	LIST	pgpkey;
static	LIST	kills_l;
static	LIST	killu_l;
static	LIST	cfeed;
static	LIST	ignore;

static	int	security_level = DEFAULT_SECURITY;

static	char	*strip_quotes[] = {

	"folder",
	"record",
	"indentprefix",
	"popuplines",
	"retrieveinterval",
	"testinterval",
	"bell",
	"flash",
	"headerlines",
	"toolcols",
	"printmail",
	"filemenusize",
	"filemenu2",
	"templates",
	NULL,

};

MAILRC	*new_mailrc()

{
	MAILRC	*m;

	m = (MAILRC *) malloc(sizeof(MAILRC));

	if (m) {
		m->next = NULL;
		m->prev = NULL;
		m->name = NULL;
		m->value = NULL;
		m->flags = 0;
	}
	
	return m;
}

void	free_mailrc(m)

MAILRC	*m;

{
	if (m) {

		if (m->name)
			free (m->name);
		if (m->value)
			free (m->value);

		free (m);
	}
}

/* Clear the Aliases list */

static	clear_aliases()

{
	MAILRC	*m,*om;

	m = alias.start;

	while (m) {

		om = m;
		m = m->next;

		free_mailrc(om);
	}
}

static	add_to_list(l,m)

LIST	*l;
MAILRC	*m;

{
	if (l->end) {
		l->end->next = m;
		m->prev = l->end;
		l->end = m;
	}
	else
		l->start = l->end = m;
}

static	add_pgpkey(s)

char	*s;

{
	char	*n,*v;
	MAILRC	*m;

	while (*s == ' ')
		s++;

	n = s;

	while (*s != ' ' && *s != '=' && *s) {
		s++;
	}

	if (!*s)
		return;

	*s = 0;
	v = s + 1;

	m = new_mailrc();

	m->name = strdup(n);
	m->value = strdup(v);
	m->flags = MAILRC_PREFIXED|MAILRC_OURPREF;

	add_to_list(&pgpkey,m);
}

static	char	*search_list(l,s)

LIST	*l;
char	*s;

{
	MAILRC	*m;

	m = l->start;

	while (m) {
		if (!strcasecmp (m->name,s))
			return m->value;
		m = m->next;
	}

	return NULL;
}

char	*find_mailrc(s)

char	*s;

{
	return search_list(&mailrc,s);
}

char	*find_alias(s)

char	*s;

{
	return search_list(&alias,s);
}

char	*find_pgpkey(s)

char	*s;

{
	return search_list(&pgpkey,s);
}

int	ignore_line(s)

char	*s;

{
	return (search_list(&ignore,s) != NULL);
}

int	kill_user(s)

char	*s;

{
	return (search_list(&killu_l,s) != NULL);
}

int	maybe_cfeed(s)

char	*s;

{
	return (search_list(&cfeed,s) != NULL);
}

static	add_entry(l,s)

LIST	*l;
char	*s;

{
	MAILRC	*m;

	m = new_mailrc();

	m->name = strdup(s);
	m->value = "";
	m->flags = MAILRC_PREFIXED|MAILRC_OURPREF;

	add_to_list(l,m);
}

static	add_cfeed(s)

char	*s;

{
	add_entry(&cfeed,s);
}

static	add_killu(s)

char	*s;

{
	add_entry(&killu_l,s);
}

static	add_kills(s)

char	*s;

{
	add_entry(&kills_l,s);
}

static	add_ignore(s)

char	*s;

{
	char	*n;

	n = s;
	while (*n) {

		/* Ignore spaces or quotes */

		while (*n == '\'' || *n == ' ') 
			n++;

		s = n;

		/* Find end */

		while (*n != ' ' && *n && *n != '\'')
			n++;

		if (*n) {
			*n = 0;
			n++;
		}

		if (n != s) {
			add_entry(&ignore,s);
		}
	}
}

static	add_alias(s)

char	*s;

{
	char	*n,*v;
	MAILRC	*m;

	while (*s == ' ')
		s++;

	n = s;

	while (*s != ' ' && *s) {
		s++;
	}

	if (!*s)
		return;

	*s = 0;
	v = s + 1;

	m = new_mailrc();

	m->name = strdup(n);
	m->value = strdup(v);

	add_to_list(&alias,m);
}

static	void	read_contents(fp)

FILE	*fp;

{
	char	line[1024],*s,*n,*v;
	int	c,i;
	int	prefixed;
	MAILRC	*m;

	while (!feof(fp)) {

		s = line;
		prefixed = 0;

		do {
			c = getc(fp);
			if (c!= EOF && c!= '\n')
				*s++ = c;
		} while (c != EOF && c!= '\n');

		*s = 0;

		s = line;

		if (line[0] == '#') {
			if (line [1] != '-' && line[1] != '@') 
				continue;

			switch (line[1]) {

				case '-':
				prefixed = MAILRC_PREFIXED;
				break;

				case '@':
				prefixed = MAILRC_PREFIXED|MAILRC_OURPREF;
				break;

			}

			s = line+2;

			if (!strcasecmp (s, "clearaliases")) {
				clear_aliases();
				continue;
			}
			if (!strncasecmp(s,"pgpkey",6)) {
				add_pgpkey(s+6);
				continue;
			}

			if (!strncasecmp(s,"security",8)) {
				security_level = atoi(s+9);

				if (security_level > MAX_SECURITY)
					security_level = MAX_SECURITY;

				if (security_level < MIN_SECURITY)
					security_level = MIN_SECURITY;

				continue;
			}

			/* Kill subject */

			if (!strncasecmp(s,"kills",5)) {
				add_kills(s+6);
				continue;
			}

			/* Kill userid */

			if (!strncasecmp(s,"killu",5)) {
				add_killu(s+6);
				continue;
			}

			/* Encrypted feed */

			if (!strncasecmp(s,"cfeed",5)) {
				add_cfeed(s+6);
				continue;
			}
		}
		else {
			if (strncasecmp(line, "set",3)) {
				if (!strncasecmp(line,"alias",5)) 
					add_alias(line+5);
				if (!strncasecmp(line,"ignore",6))
					add_ignore(line+6);

				continue;
			}

			s = line+3;
		}

		/* Look for the variable name */

		while (*s == ' ')
			s++;

		n = s;

		/* Scan through the name for the space */

		while (*s && *s != ' ' && *s != '=')
			s++;

		/* If no '=something', set value to null string */

		if (!*s) {
			v = "";
		}
		else {

			/* Now set v to point to the value */

			*s = 0;
			v = s + 1;

			while (*v == ' ')
				v++;
		}

		/* We want to strip quotes from some entries */

		i = 0;
		while (strip_quotes[i]) {
			if (!strcasecmp(strip_quotes[i],n)) {

				/* Strip out first quote */

				if (*v = '\'') 
					v++;

				/* Strip out final quote */

				s = v;
				while (*s)
					s++;
				if (*--s == '\'')
					*s = 0;

				/* End the loop */

				break;
			}
			i++;
		}

		/* Right, we've got n pointing to the name, and v to
		   the value, let's do it ! */

		m = new_mailrc();

		if (m) {

			m->value = strdup(v);
			m->name = strdup(n);
			m->flags = prefixed;

			add_to_list(&mailrc,m);
		}
	}
}

static	void	read_mailrc()

{
	char	*loc;
	char	path[MAXPATHLEN];
	FILE	*mailrcf;

	/* Default to $MAILRC */

	loc = getenv("MAILRC");

	if (!loc) {

		/* If that fails, try $HOME/.mailrc */

		loc = getenv("HOME");

		if (loc) {
			strcpy (path, loc);
			strcat	(path, "/.mailrc");
		}
		else
			strcpy (path, ".mailrc");
	}
	else
		strcpy (path, loc);

	/* See if we can open it */

	if ((mailrcf = fopen(path,"rt")) == NULL) {
		printf ("Can't find .mailrc in $MAILRC, $HOME or current directory !\n");
		exit (1);
	}

	/* Read the contents here */

	read_contents(mailrcf);

	/* Then close the file */

	fclose(mailrcf);
}

#ifndef PGPTOOLS
static	char	pgp_exec [] = PGPEXEC;
#endif

main(argc,argv)

int	argc;
char	*argv[];

{
	char	*s;

	/* Check for PGP */

#ifndef PGPTOOLS
	if (!pgp_path()) {
		printf ("Can't find PGP at '%s', exiting !\n", pgp_exec);
	}
#endif

	/* Set it here before we forget ! */

	umask (077);

	/* Initialise pgp library */

	init_pgplib();

	/* Initialise messages.c */

	init_messages();

	s = getenv("MAIL");

	if (s)
		strcpy (default_mail_file, s);
	else {
		sprintf(default_mail_file,"/var/spool/mail/%s",
			cuserid(NULL));
	}

	read_mailrc();
	read_mail_file(default_mail_file,TRUE);

	s = getenv("PGPPASS");

	if (security_level < 2 && !s)
		security_level = 2;

	setup_display(security_level,s);

	close_mail_file ();
	close_messages ();
	close_pgplib();
	exit (0);
}

/* Copy_to_nl() : Strip preceding spaces and trailing nls from a string */

void	copy_to_nl(from,to)

char	*from,*to;

{
	if (from) {
		while (*from == ' ')
			from++;

		while (*from && *from != '\n')
			*to++ = *from++;
	}

	*to = 0;
}

#ifndef PGPTOOLS
char	*pgp_path()

{
	static	char	pgp_path[MAXPATHLEN];
	static	int	first = TRUE;

	if (first) {
	}

	if (!access (pgp_exec, X_OK))
		return pgp_exec;
	else
		return NULL;

}
#endif
