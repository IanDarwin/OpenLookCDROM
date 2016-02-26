
/*
 *	@(#)messages.c	1.18 12/16/94
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
#include <malloc.h>

#include "def.h"
#include "buffers.h"
#include "message.h"

extern	FILE	*mail_fp;

void	free_string(s)

char	*s;

{
	if (s) {
		bzero (s, strlen(s));
		free (s);
	}
}

MESSAGE	*new_message()

{
	MESSAGE	*m;
	static	word32	id = 1;

	m = (MESSAGE *)malloc(sizeof(MESSAGE));

	m->body = 0;
	m->header = 0;
	m->signature = 0;
	m->decrypted = 0;
	m->sender = 0;
	m->email = 0;
	m->subject = 0;
	m->date = 0;
	m->next = 0;
	m->prev = 0;
	m->lines = 0;
	m->size = 0;
	m->number = 0;
	m->list_pos = 0;
	m->description = 0;
	m->message_id = 0;
	m->header_date = 0;
	m->offset = 0;
	m->data_type = DT_NONE;
	m->unique_id = id++;

	m->flags = 0;

	return m;
}

free_message(b)

MESSAGE	*b;

{
	if (b->body)
		free_buffer (b->body);
	if (b->header)
		free_buffer (b->header);
	if (b->signature)
		free_buffer (b->signature);
	if (b->decrypted)
		free_buffer (b->decrypted);

	if (b->sender)
		free_string (b->sender);
	if (b->email)
		free_string (b->email);
	if (b->date)
		free_string (b->date);
	if (b->subject)
		free_string (b->subject);
	if (b->description)
		free_string (b->description);
	if (b->message_id)
		free_string (b->message_id);
	if (b->header_date)
		free_string (b->header_date);

	free(b);
}

/* Add a message to the specified message list */

void	add_to_message_list_start(l,m)

MESSAGE_LIST	*l;
MESSAGE		*m;

{
	if (l->start) {
		m->next = l->start;
		l->start->prev = m;
		l->start = m;
		m->prev = NULL;
	}
	else {
		m->next = NULL;
		m->prev = NULL;

		l->start = l->end = m;
	}

	l->number++;

	if (m->status == MSTAT_NONE)
		l->new++;

	if (m->status == MSTAT_UNREAD)
		l->unread++;

	if (m->flags & MESS_ENCRYPTED)
		l->encrypted++;
}

void	add_to_message_list_end(l,m)

MESSAGE_LIST	*l;
MESSAGE		*m;

{
	if (l->end) {
		m->prev = l->end;
		l->end->next = m;
		l->end = m;
		m->next = NULL;
	}
	else {
		m->next = NULL;
		m->prev = NULL;

		l->start = l->end = m;
	}

	l->number++;

	if (m->status == MSTAT_NONE)
		l->new++;

	if (m->status == MSTAT_UNREAD)
		l->unread++;

	if (m->flags & MESS_ENCRYPTED)
		l->encrypted++;
}

#define BUF_SIZE	512

static	BUFFER	*messb = NULL;

BUFFER	*message_contents(m)

MESSAGE	*m;

{
	static	word32 	last_id = 0;
	byte	buf [BUF_SIZE];
	int	l, s;

	switch (m->data_type) {

		case DT_MEM:
		return m->body;

		case DT_FILE:

		/* Is this the same message as last time ? */

		if (m->unique_id == last_id && messb)
			return messb;

		/* Ok, read it from the file */

		if (!messb)
			messb = new_buffer ();
		else
			clear_buffer (messb);

		fseek (mail_fp, m->offset, 0);
		l = m->size;
		while (l > 0) {
			if (l > BUF_SIZE)
				s = BUF_SIZE;
			else
				s = l;

			fread (buf, s, 1, mail_fp);
			add_to_buffer (messb, buf, s);
			l -= s;
		}

		last_id = m->unique_id;

		bzero (buf, BUF_SIZE);
		return messb;

		default:
		return NULL;
	}
}

set_mem_message (m)

MESSAGE	*m;

{
	m->data_type = DT_MEM;
}

set_file_message (m)

MESSAGE	*m;

{
	m->data_type = DT_FILE;
}

init_messages ()

{
}

close_messages ()

{
	if (messb)
		free_buffer (messb);
}

