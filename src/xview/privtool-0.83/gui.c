
/*
 *	@(#)gui.c	1.36 12/16/94
 *
 *	Gui.c : It is intended that this file will contain as much
 *	as possible of the user-interface code, allowing privtool to
 *	be compiled for different operating systems and UI toolkits
 *	by 'simply' replacing the lowest level of functionality.
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
#include <stdlib.h>
#include <string.h>
#include <time.h>

#include "def.h"
#include "buffers.h"
#include "message.h"
#include "mailrc.h"
#include "gui.h"

/* Local variables */

static	int	security;
static	time_t	last_displayed = 0;

static	char	*mail_args[] =

{
	"/usr/lib/sendmail",
	"-t",
	NULL
};

static	char	*print_args[] =

{
	"/usr/ucb/lpr",
	NULL
};

/* Titles for passphrase windows */

static	char	sign_passphrase[]="Passphrase entry (Signing)";
static	char	decrypt_passphrase[]="Passphrase entry (Decryption)";

/* Global variables */

char	*passphrase;
void	(*callback_proc)();

MESSAGE	*last_message_read;
MESSAGE	*message_to_decrypt;
MESSAGE	*displayed_message;

int	deliver_flags;

/* Program name */

char	prog_name[] = "Privtool V0.83 BETA";

/* Set the description of the message for the message list */

void	set_message_description(m)

MESSAGE	*m;

{
	char	mess_desc[256];
	char	mess_size[32];
	char	mess_type[8];

	/* Default to spaces */

	strcpy(mess_type,"   ");

	/* Set new/unread status */

	if (m->status == MSTAT_NONE) {
		mess_type[0] = 'N';
	}
	else if (m->status == MSTAT_UNREAD) {
		mess_type[0] = 'U';
	}

	/* Check signature status */

	if (m->flags & MESS_SIGNED) {
		if (!(m->flags & MESS_VERIFIED)) {
			mess_type [1] = 's';
		}
		else if (m->flags & MESS_BAD) {
			mess_type [1] = 'X';
		}
		else {
			mess_type [1] = 'S';
		}
	}

	/* Check encryption status */

	if (m->flags & MESS_ENCRYPTED) {
		if (!(m->flags & (MESS_SIGNED|MESS_VERIFIED)))
			mess_type[1] = '?';
		if (m->flags & MESS_VERIFIED) 
			mess_type[2] = 'D';
		else
			mess_type[2] = 'E';
	}

	/* Create message size string */

	sprintf(mess_size,"%d/%d",m->lines,m->size);

	if (m->subject) {
		sprintf(mess_desc,"%3.3s %-5d %-30.30s %20.20s  %s  %-64.64s",
			mess_type,m->number,m->email,m->date,
				mess_size,m->subject);
	}
	else {
		sprintf(mess_desc,"%3.3s %-5d %-30.30s %20.20s  %s ",
			mess_type,m->number,m->email,m->date,mess_size);
	}

	/* Clear the old description if possible */

	if (m->description)
		free (m->description);

	/* And create the new description */

	m->description = (char *)strdup(mess_desc);
}

/* Expand a string into a buffer of email addresses, taking aliases
   into account */

#define COUNT_MAX	10

void	expand_send(b,s)

BUFFER	*b;
char	*s;

{
	char	*p,*a;
	char	address[512];
	static	int	count = 0;

	/* Just in case we get an alias loop, abort ! */

	if (count >= COUNT_MAX) {
		fprintf (stderr, "Privtool: Probable alias loop detected !\n");
		return;
	}

	/* Increment entry count */

	count++;
	p = s;

	while (*p) {

		/* Addresses can be space or comma seperated */

		a = address;
		while (*p == ' ' || *p == ',')
			p++;
		while (*p != ' ' && *p != ',' && *p) {
			*a++ = *p++;
		}

		*a = 0;
		a = find_alias (address);

		/* If we got an alias, expand it */

		if (a) {
			/* We have to duplicate the string as it will get
			   trashed lower down */

			a = strdup (a);
			expand_send (b, a);
			free_string (a);
		}
		else {
			if (b->length)
				add_to_buffer (b, " ", 1);
			add_to_buffer (b, address, strlen(address));
		}
	}

	/* Decrement entry count */

	count--;
}

static	char	**split_string (s, c)

char	*s;
int	*c;

{
	int	count = 0;
	int	i, j, k, l;
	char	**list;

	l = strlen (s);
	if (l)
		count++;

	for (i = 0; i < l; i++) {
		if (s [i] == ' ')
			count++;
	}

	*c = count;

	if (!count)
		return NULL;

	list = (char **) malloc ((count + 1) * sizeof (char *));

	j = 0;
	for (i = 0; i < count; i++) {
		for (k = j; k < l && s[k] != ' '; k++);
		s [k] = 0;
		list [i] = (char *)s + j;
		j = k + 1;
	}

	list [i] = NULL;

	return list;
}

static	char	**split_list (b, c)

BUFFER	*b;
int	*c;

{
	return split_string (b->message, c);
}

/* Display selected message */

void	display_message(m)

MESSAGE	*m;

{
	int	i;

	/* Add time to random seed */

	update_random();
	time (&last_displayed);

	displayed_message = m;

	hide_header_frame();

	/* Set the icon back in case it was 'new mail' */

	show_normal_icon ();
	create_display_window();

	/* Let user know we're doing something */

	set_display_footer("Displaying Message...");

	/* Display sender and date */

	display_sender_info(m);
	clear_display_window();

	if (m->flags & MESS_DIGEST) {

	/* Put special Digest handling here */

m->flags &= ~(MESS_SIGNED|MESS_ENCRYPTED);

	}

	/* Process encrypted/signed messages through PGP */

	if (m->flags & (MESS_SIGNED|MESS_ENCRYPTED)) {
		if (!(m->flags & MESS_VERIFIED)) {

			/* Either create a new buffer or clear the old one */

			if (!m->decrypted)
				m->decrypted = new_buffer();
			else
				clear_buffer(m->decrypted);

			/* Ditto for signature info */

			if (!m->signature)
				m->signature = new_buffer();
			else
				clear_buffer(m->signature);

			/* If it's encrypted, we need a passphrase */

			if (m->flags & MESS_ENCRYPTED) {
				if (!passphrase) {
					message_to_decrypt = m;
					callback_proc = decrypt_with_passphrase;
					get_passphrase(decrypt_passphrase);
					clear_display_footer();
					return;
				}
			}

			/* Keep the user informed */

			if (m->flags & MESS_ENCRYPTED)
				set_display_footer("Decrypting message...");
			else
				set_display_footer("Verifying signature...");

			/* Call PGP to decrypt/verify */

			i = decrypt_message(message_contents(m),
				m->decrypted,
				m->signature,passphrase,FL_ASCII);

			/* Destroy the passphrase if we got one */

			if (m->flags & MESS_ENCRYPTED) {
				destroy_passphrase(FALSE);
			}

			/* Uh-oh, bad passphrase ! */

			if (i == DEC_BAD_PHRASE) {
				destroy_passphrase(TRUE);
				if (bad_pass_phrase_notice(ERROR_READING)) {
					callback_proc = decrypt_with_passphrase;
					message_to_decrypt = m;
					get_passphrase(decrypt_passphrase);
				}
				clear_display_footer();
				return;
			}

			if (i == DEC_BAD_FILE) {
				bad_file_notice (ERROR_READING);
				m->flags &= ~(MESS_ENCRYPTED|MESS_SIGNED);
				goto display_plaintext;
			}

			/* Process the error and set flags */

			set_flags_from_decryption(m,i);
		}

		/* Handle encrypted mailing list feeds */

		if ((m->flags & MESS_CFEED) &&
			is_mail_message(m->decrypted)) {
			MESSAGE	*newm;

			/* Keep the user informed */

			set_display_footer("Found encrypted mailfeed...");

			/* Replace the old message with the decrypted one */

			displayed_message = NULL;
			newm = message_from_message(m);
			replace_message_with_message(m,newm);
			last_message_read = newm;

			if (!(newm->flags & MESS_ENCRYPTED))
				messages.encrypted--;

			newm->status = MSTAT_READ;
			if (m->status == MSTAT_NONE) {
				messages.new--;
				update_message_list();
			}
			if (m->status == MSTAT_UNREAD) {
				messages.unread--;
				update_message_list();
			}

			/* Update message list */

			newm -> flags |= MESS_SELECTED;
			set_message_description(newm);
			display_message_description(newm);

			/* Then go back round the loop */

			free_message(m);
			display_message(newm);

			return;
		}

		/* Otherwise, just display the decrypted message */

		display_message_body(m->decrypted);
		display_message_sig(m->signature);

		if (buffer_contains_key (m->decrypted))
			show_addkey ();
	}
	else {

	display_plaintext:
		/* Or display the plaintext message */

		display_message_body(message_contents (m));

		if (buffer_contains_key (message_contents (m)))
			show_addkey ();
	}

	/* Update status to say we read it */

	if (m->status != MSTAT_READ) {
		if (m->status == MSTAT_NONE) {
			messages.new--;
			update_message_list ();
		}
		if (m->status == MSTAT_UNREAD) {
			messages.unread--;
			update_message_list ();
		}
		m->status = MSTAT_READ;
	}

	/* Lock the display window to prevent edits, and show it */

	lock_display_window();
	show_display_window(m);

	/* Just in case anything's changed */

	set_message_description(m);
	display_message_description(m);

	/* Destroy passphrase and clear window, just in case */

	destroy_passphrase(FALSE);

	/* Clear the footer now it's displayed */

	clear_display_footer();
}

/* Selecting next or previous message */

static	void	select_message_proc(m)

MESSAGE	*m;

{
	if (last_message_read && m) {
		last_message_read->flags &= ~MESS_SELECTED;
		display_message_description(last_message_read);

		last_message_read = m;
		last_message_read->flags |= MESS_SELECTED;
		display_message(last_message_read);

		/* Yes, we do this twice for a reason - last_message_read
		   may have changed on the way through display_message */

		if (last_message_read) {
			last_message_read->flags |= MESS_SELECTED;
			set_message_description(last_message_read);
			display_message_description(last_message_read);
		}
	}
}

void	prev_message_proc()

{
	update_random();

	if (last_message_read) {
		MESSAGE	*m;

		m = last_message_read->prev;
		while (m && (m->flags & MESS_DELETED))
			m = m->prev;

		if (m)
			select_message_proc (m);
	}
}

void	next_message_proc()

{
	MESSAGE	*m;

	update_random();

	if (last_message_read) {
		m = last_message_read->next;
		while (m && (m->flags & MESS_DELETED))
			m = m->next;
		if (m)
			select_message_proc (m);
	}
}

void	quit_proc()

{
	MESSAGE	*m, *om;

	close_mail_file ();
	update_random();

	/* First free the message list, so that everything gets erased */

	m = messages.start;

	while (m) {
		om = m;
		m = m->next;
		free_message (om);

		messages.number--;
	}

	/* Destroy the passphrase, just in case */

	destroy_passphrase(TRUE);

	shutdown_ui();
	close_messages ();
	close_pgplib();

	exit(1);
}

/* Save Changes and quit */

void	save_and_quit_proc ()

{
	set_main_footer ("Saving changes and quitting...");

	/* Save any changes to the file */

	if (save_changes () < 0) {
		if (failed_save_notice_proc ()) {
			clear_main_footer ();
			return;
		}
	}

	quit_proc ();
}

void	got_passphrase()

{
	void	(*call_proc)();

	update_random();

	passphrase = read_passphrase_string();
	close_passphrase_window();

	if (callback_proc) {
		call_proc = callback_proc;
		callback_proc = NULL;

		/* Have to do it this way as callback_proc may reset
		   the value in callback_proc on error */

		(*call_proc)();
	}
}

/* Destroy_passphrase() is used to carefully delete all possible
   copies of the user's passphrase */

void	destroy_passphrase(force)

int	force;

{
	int	i;

	if (security_level() > 2 || force) {

		/* If we don't have a passphrase, get the value of the
	    	   passphrase item in case it was partially entered
	   	   then aborted */

		if (!passphrase) {
			passphrase = read_passphrase_string();
		}

		/* Now destroy the passphrase in the panel item */

		if (passphrase) {
			while (*passphrase) {
				*passphrase = 0;
				passphrase++;
			}
		}

		/* Then set the value of the item to nothing, just in case */

		clear_passphrase_string();
		
		/* Delete the pointer */

		passphrase = NULL;
	}

	/* And close the passphrase window if it was open */

	close_passphrase_window();
}

void	abort_passphrase_proc()

{
	update_random();

	callback_proc = NULL;
	destroy_passphrase(FALSE);
	close_passphrase_window();
}

void	get_passphrase(s)

char	*s;

{
	update_random();

	create_passphrase_window();
	open_passphrase_window(s);
	clear_passphrase_string();

	passphrase = NULL;
}

void	delete_message(om)

MESSAGE	*om;

{
	/* Clear last_message_read if it's deleted */

	if (om == last_message_read) {
		MESSAGE	*m = NULL;

		if (om->next) {
			m = om->next;
			while (m && (m->flags & MESS_DELETED))
				m = m->next;
		}

		if (!m) {
			m = om->prev;
			while (m && (m->flags & MESS_DELETED))
				m = m->prev;
		}

		last_message_read = m;
	}

	/* Clear selected flag */

	om->flags &= ~MESS_SELECTED;
	om->list_pos = (-1);

	/* Update message info */

	if (om->flags & MESS_ENCRYPTED)
		messages.encrypted--;

	if (om->status == MSTAT_NONE)
		messages.new--;
	if (om->status == MSTAT_UNREAD)
		messages.unread--;

	/* And add it to the deleted list */

	add_to_deleted (om);

	messages.number --;
}

int	security_level()

{
	return security;
}


void	setup_display(level,phrase)

int	level;
char	*phrase;

{
	security = level;
	if (security < 2)
		passphrase = phrase;

	setup_ui(level);
}

void	decrypt_with_passphrase()

{
	display_message(message_to_decrypt);
}

void	set_flags_from_decryption(m,i)

MESSAGE	*m;
int	i;

{
	m->flags |= MESS_VERIFIED;

	switch(i) {

		/* Uh-oh, couldn't find the key */

		case SIG_NO_KEY:
		case DEC_NO_KEY:
		m->flags &= ~MESS_VERIFIED;
		break;

		/* 
		   Whoops, no signature, boys ! 
		*/

		case SIG_NONE:
		m->flags &= ~MESS_SIGNED;
		break;

		/* Uh-oh, Bad signature */

		case SIG_BAD:
		m->flags |= MESS_BAD;
		break;

		/* 
		    That's more like it ! 
		    In case it was a signed, encrypted message, set the
		    signed bit.
		*/

		case SIG_GOOD:
		m->flags |= MESS_SIGNED;
		break;
	}
}

static	char	*replying_message_id;
static	char	*replying_message_sender;

void	setup_send_window()

{
	if (replying_message_id) {
		free_string (replying_message_id);
		replying_message_id = NULL;
	}

	if (replying_message_sender) {
		free_string (replying_message_sender);
		replying_message_sender = NULL;
	}

	x_setup_send_window ();
}
/* Set up reply variables from the message we're replying to */

void	set_reply (m)

MESSAGE	*m;

{
	if (replying_message_id) {
		free_string (replying_message_id);
		replying_message_id = NULL;
	}

	if (replying_message_sender) {
		free_string (replying_message_sender);
		replying_message_sender = NULL;
	}

	if (m) {
		if (m->message_id)
			replying_message_id = strdup (m->message_id);
		if (m->sender)
			replying_message_sender = strdup (m->sender);
	}
}

static	int	remove_duplicates (list)

char	**list;

{
	int	i, j;

	i = 0;
	while (list [i])
		i++;

	if (!i)
		return 0;

	/* Sort the list */

	qsort (list, i, sizeof (char *), strcmp);

	i = 1;
	j = 1;

	/* Loop through the sorted list */

	while (list [i]) {
		/* If they're different, move them */

		if (strcmp (list [i], list [i-1])) {
			list [j++] = list [i];
		}

		/* Otherwise, move on */

		i++;
	}

	list [j] = 0;

	return j;
}

static	add_list_to_header (b, l)

BUFFER	*b;
char	**l;

{
	int	i;
	int	w, s;

	w = 16;
	for (i = 0; l[i]; i++) {
		s = strlen (l[i]) + 1;
		if (w+s > 75) {
			add_to_buffer (b, "\n    ", 5);
			w = 5;
		}
		add_to_buffer (b, l[i], s-1);
		if (l[i+1])
			add_to_buffer (b, ",", 1);
	}
	add_to_buffer (b, "\n", 1);
}

/* Support folder specification */

char	*expand_filename (s)

char	*s;

{
	char	*folder;
	char	*home;
	static	int	filename_size = 0;
	static	char	*filename = NULL ;
	int	sz;

	folder = find_mailrc("folder");
	home = getenv ("HOME");

	if (s && *s) {

		/* If folder specified, we need to build up
		   the full pathname */

		if (folder && *folder && *s == '+') {

			/* We'll be kind to malloc here */

			sz = (strlen (s) + 
				strlen (folder) + 4 
				+ QUANTA);

			if (*folder != '/') {
				sz += strlen (home) + 1;
			}

			sz /= QUANTA;
			sz *= QUANTA;

			if (!filename) {
				filename = (char *)malloc (sz);
				filename_size = sz;
			}
			else if (sz > filename_size) {
				filename = realloc (filename, sz);
				filename_size = sz;
			}

			if (*folder == '/')
				sprintf(filename, "%s/%s", folder, s+1);
			else
				sprintf (filename, "%s/%s/%s",
					home, folder, s+1);

			s = filename;
		}
	}

	return s;
}

void	deliver_proc()

{
	char	recipient[512];
	char	subject[256];
	char	cc[256];
	char	buff[256];
	BUFFER	*mail_message;
	BUFFER	*raw_message = NULL;
	BUFFER	*log_message;
	int	ret_val;
	char	**userid = NULL;
	char	**addrs = NULL;
	char	**cc_addrs = NULL;
	int	id_count = 0;
	int	cc_count = 0;
	char	*uid,*alias;
	char	*key_name;
	int	encrypt_flags;
	int	i, j;
	BUFFER	*full_list;
	BUFFER	*cc_list;

	update_random();

	copy_to_nl(read_recipient(),recipient);
	copy_to_nl(read_subject(),subject);
	copy_to_nl(read_cc(),cc);

	full_list = new_buffer ();
	cc_list = new_buffer ();

	expand_send (full_list, recipient);
	if (*cc)
		expand_send (cc_list, cc);

	if (!full_list->length) {
		free_buffer (full_list);
		free_buffer (cc_list);
		return;
	}

	addrs = split_list (full_list, &id_count);
	if (*cc)
		cc_addrs = split_list (cc_list, &cc_count);

	if (!id_count) {
		free_buffer (full_list);
		free_buffer (cc_list);

		if (addrs)
			free (addrs);
		if (cc_addrs)
			free (cc_addrs);

		return;
	}

	id_count = remove_duplicates (addrs) + 2;
	if (cc_addrs)
		cc_count = remove_duplicates (cc_addrs);

	if (*recipient) {
		BUFFER	*to_send;

		to_send = new_buffer();
		read_message_to_deliver(to_send);

try_again:
		/* Check for alias */

		if (deliver_flags & PGP_OPTIONS) {
			BUFFER	*encrypted;
			char	*s;

			encrypt_flags = FL_ASCII;
			if (deliver_flags & DELIVER_SIGN)
				encrypt_flags |= FL_SIGN;
			if (deliver_flags & DELIVER_ENCRYPT)
				encrypt_flags |= FL_ENCRYPT;

			if (deliver_flags & DELIVER_SIGN) {
				if (!passphrase) {
					free_buffer(to_send);
					if (addrs)
						free (addrs);
					if (cc_addrs)
						free (cc_addrs);
					free_buffer (full_list);
					free_buffer (cc_list);
					callback_proc = deliver_proc;
					get_passphrase(sign_passphrase);
					return;
				}
			}

			/* Allow for .mailrc conversion of key ids */

			userid = (char **)malloc ((id_count+cc_count) * 
				sizeof(char *));

			i = 0;
			for (j = 0; j < id_count && addrs [j]; j++) {
				alias = find_pgpkey (addrs [j]);
				if (alias) {
					userid [i++] = alias;
				}
				else
					userid [i++] = addrs [j];
			}

			for (j = 0; j < cc_count && cc_addrs [j]; j++) {
				alias = find_pgpkey (cc_addrs [j]);
				if (alias) {
					userid [i++] = alias;
				}
				else
					userid [i++] = cc_addrs [j];
			}

			userid [i] = NULL;

			/* Get our key name for the routine */

			key_name = cuserid(NULL);
			if (s = find_pgpkey (key_name))
				key_name = s;

			/* If cooked logging, encrypt for us too */

			if ((deliver_flags & DELIVER_LOG) &&
				(deliver_flags & DELIVER_ENCRYPT) &&
				!(deliver_flags & DELIVER_RAW)) {
				userid[i++] = key_name;
			}

			userid [i] = 0;

			/* Strip duplicates from list */

			(void) remove_duplicates (userid);

			encrypted = new_buffer();
			ret_val = encrypt_message(userid,to_send,
				encrypted,encrypt_flags,passphrase,
				key_name); 

			free (userid);

			if (deliver_flags & DELIVER_SIGN) {
				destroy_passphrase(FALSE);
			}

			if (ret_val == ERR_NO_KEY) {
				if (no_key_notice_proc(ERROR_DELIVERY)) {
					deliver_flags &= ~DELIVER_ENCRYPT;

					update_log_item(deliver_flags);
					goto try_again;
				}
				else {
					free_buffer(encrypted);
					free_buffer(to_send);
					free_buffer (full_list);
					free_buffer (cc_list);
					if (addrs)
						free (addrs);
					if (cc_addrs)
						free (cc_addrs);
					return;
				}	
			}

			if (ret_val == ERR_NO_SECRET_KEY) {
				if (no_sec_notice_proc(ERROR_DELIVERY)) {
					deliver_flags &= ~DELIVER_SIGN;

					update_log_item(deliver_flags);
					goto try_again;
				}
				else {
					free_buffer(encrypted);
					free_buffer(to_send);
					free_buffer (full_list);
					free (addrs);
					return;
				}	
			}

			if (ret_val == ERR_BAD_PHRASE) {
				free_buffer(encrypted);
				free_buffer(to_send);
				free_buffer (full_list);
				free (addrs);
				destroy_passphrase(TRUE);
				if (bad_pass_phrase_notice(ERROR_DELIVERY)) {
					callback_proc = deliver_proc;
					get_passphrase(sign_passphrase);
				}
				return;
			}

			if ((deliver_flags & DELIVER_ENCRYPT) &&
				(deliver_flags & DELIVER_RAW)) {
				raw_message = to_send;
				sprintf(buff,
					"\n     [ Privtool Note : Real message was sent encrypted");
				if (deliver_flags & DELIVER_SIGN) {
					strcat(buff, " and signed");
				}
				strcat(buff, " ]\n\n");

				add_to_buffer(raw_message, buff,
					strlen(buff));
			}
			else
				free_buffer(to_send);

			to_send = encrypted;
		}

		mail_message = new_buffer();

#ifdef MAILER_LINE
		sprintf(buff,"Mailer: %s\n",prog_name);
		add_to_buffer(mail_message,buff,strlen(buff));
#endif

		if (replying_message_id &&
			replying_message_sender) {
			sprintf(buff,"In-Reply-To: %s from \"%s\"\n",
				replying_message_id,
				replying_message_sender);
			add_to_buffer (mail_message,buff,strlen(buff));
		}

		if (*subject) {
			sprintf(buff,"Subject: %s\n",subject);
			add_to_buffer(mail_message,buff,strlen(buff));
		}

		add_to_buffer (mail_message, "To: ", 4);
		add_list_to_header (mail_message, addrs);

		if (cc_addrs) {
			add_to_buffer (mail_message, "Cc: ", 4);
			add_list_to_header (mail_message, cc_addrs);
		}

		add_to_buffer(mail_message,"\n",1);
		add_to_buffer(mail_message,to_send->message,
			to_send->length);

		run_program(mail_args[0],mail_message->message,
			mail_message->length,mail_args,NULL);

		if (deliver_flags & DELIVER_LOG)  {
			char	*record;

			record = find_mailrc("record");

			if (record && *record) {

				/* Expand filename to full path */

				record = expand_filename (record);

				log_message = to_send;
				if (deliver_flags & DELIVER_RAW) {
					if (raw_message)
						log_message = raw_message;
				}

				write_buffer_to_mail_file(log_message,recipient,
					cc,subject,record);
			}
		}

		if (raw_message)
			free_buffer (raw_message);

		free_buffer(to_send);
		free_buffer(mail_message);
		free_buffer (full_list);
		free (addrs);

		close_deliver_window();
	}
}

void	set_deliver_flags(value)

int	value;

{
	deliver_flags = value;
}

void	move_message_proc(s)

char	*s;

{
	MESSAGE	*m;
	char	mess[128];
	int	n = 0;

	update_random();

	if (!s)
		return;

	s = expand_filename (s);

	/* Start at the head of the list */

	m = messages.start;

	while (m) {

		if (m->flags & MESS_SELECTED) {
			if (!append_message_to_file (m,s,FALSE)) {
				n++;
			}
			else if (!failed_save_notice_proc()) {
				return;
			}
		}

		m = m->next;
	}

	if (!n)
		set_main_footer("No messages saved.");
	else {
		sprintf(mess,"%d messages moved to %s",n,s);
		set_main_footer(mess);
		delete_message_proc();
	}
}

void	copy_message_proc(s)

char	*s;

{
	MESSAGE	*m;
	int	n = 0;
	char	mess[128];

	update_random();

	if (!s)
		return;

	s = expand_filename (s);

	/* Start at the head of the list */

	m = messages.start;

	while (m) {

		if (m->flags & MESS_SELECTED) {
			if (!append_message_to_file (m,s,FALSE)) {
				n++;
			}
			else if (!failed_save_notice_proc()) {
				return;
			}
		}

		m = m->next;
	}

	if (!n)
		set_main_footer("No messages saved.");
	else {
		sprintf(mess,"%d messages saved to %s",n,s);
		set_main_footer(mess);
	}
}

void	load_new_mail()

{
	MESSAGE	*m,*last;
	int	l,i;

	update_random();

	last = messages.end;

	set_main_footer("Retrieving new mail...");
	read_new_mail();

	if (last) {
		MESSAGE	*mm;

		m = last->next;
		i = last->number;
		mm = last;

		while (mm && (mm->flags & MESS_DELETED)) {
			mm = mm->prev;
		}

		if (mm)
			l = mm->list_pos;
		else
			l = 0;
	}
	else {
		m = messages.start;
		i = l = 0;
	}

	update_message_list ();

	while (m) {
		m->number = ++i;
		if (!(m->flags & MESS_DELETED))
			m->list_pos = ++l;

		set_message_description (m);
		display_message_description (m);

		m = m->next;
	}

	clear_main_footer();
}

void	check_for_new_mail()

{
	MESSAGE	*m,*last;
	static	long	test_interval = 0;
	time_t	now;

	/* If no messages displayed in testinterval seconds, then clear
	   the passphrase */

	if (!test_interval) {
		char	*f;

		f = find_mailrc("testinterval");
		if (f)
			test_interval = atoi (f);
		else
			test_interval = (-1);
	}

	if (test_interval > 0 && last_displayed > 0) {
		time (&now);
		if ((now - last_displayed) > test_interval)
			destroy_passphrase (TRUE);
	}

	/* Feed some bits to the random number generator */

	update_random();

	/* Finally, check for new mail ! */

	if (is_new_mail()) {
		show_newmail_icon ();
		load_new_mail ();
	}
}

int	load_file_proc(s)

char	*s;

{
	MESSAGE	*m,*om;
	int	i;
	int	res;

	destroy_passphrase(FALSE);

	s = expand_filename (s);

	set_main_footer ("Saving changes and loading new mail...");

	/* Save any changes to the file */

	if (save_changes () < 0) {
		if (failed_save_notice_proc ()) {
			clear_main_footer ();
			return -1;
		}
	}

	/* Free main list */

	m = messages.end;

	while (m) {
		om = m;
		m = m->prev;

		free_message(om);
	}

	deleted.start = NULL;
	deleted.end = NULL;
	deleted.number = 0;

	messages.start = NULL;
	messages.end = NULL;

	messages.number = 0;
	messages.encrypted = 0;
	messages.new = 0;
	messages.unread = 0;

	if (!s || !*s)
		res = read_mail_file(default_mail_file,TRUE);
	else
		res = read_mail_file(s,FALSE);

	m = messages.start;

	i = 1;
	while (m) {
		m->list_pos = i;
		m->number = i;
		set_message_description(m);
		display_message_description(m);

		m = m->next;

		i++;
	}

	update_message_list ();

	last_message_read = NULL;
	displayed_message = NULL;
	message_to_decrypt = NULL;

	clear_main_footer();

	return 0;
}

void	inbox_proc()

{
	update_random();

	if (reading_file (default_mail_file))
		load_new_mail ();
	else
		load_file_proc (default_mail_file);
}

void	save_changes_proc()

{
	char	*s;

	update_random();

	s = current_mail_file ();
	if (s && *s) {
		load_file_proc (s);
	}
}

void	done_proc()

{
	update_random();

	save_changes_proc ();
	close_all_windows ();
}

static	int	number_cmp (m1,m2)

MESSAGE	**m1,**m2;

{
	return (*m1)->number - (*m2)->number;
}

static	int	status_cmp (m1,m2)

MESSAGE	**m1,**m2;

{
	if ((*m1)->status == (*m2)->status)
		return (*m1)->number - (*m2)->number;

	return (*m1)->status - (*m2)->status;
}

static	int	size_cmp (m1,m2)

MESSAGE	**m1,**m2;

{
	return (*m1)->size - (*m2)->size;
}

static	int	date_cmp(m1,m2)

MESSAGE	**m1,**m2;

{
	return ((*m1)->time_secs - (*m2)->time_secs);
}

static	int	sender_cmp(m1,m2)

MESSAGE	**m1,**m2;

{
	return strcmp((*m1)->email,(*m2)->email);
}

static	int	subject_cmp(m1,m2)

MESSAGE	**m1,**m2;

{
	if ((*m1)->subject == NULL) {
		if ((*m2)->subject == NULL)
			return 0;
		return -1;
	}

	if ((*m2)->subject == NULL)
		return 1;

	return strcmp((*m1)->subject,(*m2)->subject);
}

static	int	sort_messages (proc)

int	(*proc)();

{
	MESSAGE	**m_list;
	int	n, i = 0, l = 1;
	MESSAGE	*m;

	update_random();

	n = messages.number + deleted.number;

	if (n <= 0)
		return;

	m_list = (MESSAGE **) malloc (n * sizeof (MESSAGE *));

	m = messages.start;

	while (m) {
		m_list [i++] = m;
		m = m->next;
	}

	qsort (m_list, n, sizeof (MESSAGE *), proc);

	messages.start = m_list[0];
	m_list[0]->prev = NULL;

	for (i = 0; i < n; i++) {
		if (i) {
			m_list[i]->prev = m_list[i-1];
		}
		if (i != (n-1)) {
			m_list[i]->next = m_list[i+1];
		}

		if (!(m_list[i]->flags & MESS_DELETED)) {
			m_list[i]->list_pos = l++;
			set_message_description (m_list[i]);
			display_message_description (m_list[i]);
		}
	}

	messages.end = m_list[n-1];
	messages.end->next = NULL;

	free (m_list);
}

void	sort_by_time ()

{
	sort_messages (date_cmp);
}

void	sort_by_number ()

{
	sort_messages (number_cmp);
}

void	sort_by_subject ()

{
	sort_messages (subject_cmp);
}

void	sort_by_sender ()

{
	sort_messages (sender_cmp);
}

void	sort_by_size ()

{
	sort_messages (size_cmp);
}

void	sort_by_status ()

{
	sort_messages (status_cmp);
}

void	undelete_last_proc ()

{
	MESSAGE	*m, *p;
	int	l;

	update_random();

	/* Return if nothing to do */

	if (!(deleted.start))
		return;

	m = deleted.start;

	if (!m->dnext) {
		deleted.start = deleted.end = NULL;
	}
	else {
		m->dnext->dprev = NULL;
		deleted.start = m->dnext;
	}

	/* Clear deleted list stuff */

	m->dnext = m->dprev = NULL;
	m->flags &= ~MESS_DELETED;

	deleted.number--;
	messages.number++;

	if (m->status == MSTAT_NONE)
		messages.new++;
	if (m->status == MSTAT_UNREAD)
		messages.unread++;

	if (m->flags & MESS_ENCRYPTED)
		messages.encrypted++;

	p = m->prev;

	while (p && (p->flags & MESS_DELETED)) {
		p = p->prev;
	}

	if (p)
		l = p->list_pos + 1;
	else
		l = 1;

	while (m) {
		m->list_pos = l++;
		set_message_description (m);
		display_message_description (m);

		m = m->next;
		while (m && (m->flags & MESS_DELETED))
			m = m->next;
	}

	update_message_list ();
}

static	char	dec_mess [] = "Decrypted message reads :\n\n";
static	char	sig_mess [] = "\n\nMessage was signed :\n\n";
static	char	end_mess [] = "\n\nEnd of signature information\n";

static	void	print_message_proc (raw)

int	raw;

{
	MESSAGE	*m;
	BUFFER	*out;
	char	*s;
	char	mess [128];
	int	c = 0;

	update_random ();

	out = new_buffer ();

	m = messages.start;

	while (m) {
		if (!(m->flags & MESS_DELETED) && (m->flags & MESS_SELECTED)) {
			c++;

			add_to_buffer (out, m->header->message,
				m->header->length);
			add_to_buffer (out, "\n", 1);

			if ((m->flags & MESS_VERIFIED) && !raw) {
				if (m->flags & MESS_ENCRYPTED) {
					add_to_buffer (out, dec_mess,
						strlen (dec_mess));
				}
				add_to_buffer (out, m->decrypted->message,
					m->decrypted->length);
				if (m->flags & MESS_SIGNED) {
					add_to_buffer (out, sig_mess,
						strlen (sig_mess));
					add_to_buffer (out, 
						m->signature->message,
						m->signature->length);
					add_to_buffer (out, end_mess,
						strlen (end_mess));
				}
			}
			else {
				add_to_buffer (out, 
					message_contents(m)->message,
					message_contents(m)->length);
			}

			add_to_buffer (out, "\n", 1);
		}

		m = m->next;
	}

	if (out->length) {
		char	*s;
		char	**args;
		int	count = 0;

		s = find_mailrc ("printmail");
		if (s) {
			s = strdup (s);
			args = split_string (s, &count);

			if (!count) {
				free (args);
				free (s);
				return;
			}
		}
		else
			args = print_args;

		run_program(args[0],out->message,out->length,args,NULL);

		sprintf(mess,"%d messages sent for printing",c);
		set_main_footer(mess);
	}

	free_buffer (out);
}

void	print_cooked_proc ()

{
	print_message_proc (FALSE);
}

void	print_raw_proc ()

{
	print_message_proc (TRUE);
}


