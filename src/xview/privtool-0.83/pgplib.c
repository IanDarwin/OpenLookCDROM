
/*
 *	@(#)pgplib.c	1.35 9/10/94
 *
 *	pgplib.c, (mostly) by Mark Grant 1993
 *
 *	Provides a basic interface to pgp and/or PGPTools.
 *
 *	Most parts (c) Copyright 1993-1994 by Mark Grant. All rights reserved.
 *	The author assumes no liability for damages resulting from the 
 *	use of this software, even if the damage results from defects in
 *	this software. No warranty is expressed or implied.
 *
 *	Some of this code is based on the file ptd.c by Pr0duct Cypher,
 *	the rest is being distributed under the GNU Public Licence,
 *	see the file COPYING for more details.
 *
 *			- Mark Grant (mark@unicorn.com) 29/6/94
 *
 */

#define PGPLIB

#include <stdio.h>
#ifndef SYSV
#include <vfork.h>
#else
#include <signal.h>
#endif

#include <string.h>
#include <malloc.h>
#include <sys/types.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/timeb.h>
#include <sys/resource.h>

#include "def.h"

#ifdef PGPTOOLS
#include <usuals.h>
#include <md5.h>
#include <idea.h>
#include <mpilib.h>
#include <fifo.h>
#include <pgptools.h>
#include <pgpkmgt.h>
#include <pgpmem.h>
#endif

#include "buffers.h"

/* Following are for grabbing messages from PGP or other programs */

static	BUFFER	error_messages;
static	BUFFER	stdout_messages;
static	BUFFER	ret_messages;

/* Following generally useful */

static	char	begin_key [] = "\n-----BEGIN PGP PUBLIC KEY BLOCK-----\nVersion: ";

/* All this is only needed if we're using PGPTools and not forking off
   a copy of PGP for encryption/decryption */

#ifdef PGPTOOLS

/* We'll set up all the strings we need here */

static	char	pgp_version [] = PGPVERSION;
static	char	two_n [] = "\n\n";
static	char	begin_armour[]="-----BEGIN PGP MESSAGE-----\nVersion: ";
static	char	end_armour[]="-----END PGP MESSAGE-----\n";
static	char	begin_signed[]="-----BEGIN PGP SIGNED MESSAGE-----\n\n";
static	char	begin_signature[]="\n-----BEGIN PGP SIGNATURE-----\nVersion: ";
static	char	end_signature[]="-----END PGP SIGNATURE-----\n";

static	char	good_sig [] = "Good Signature from user ";
static	char	bad_sig [] = "WARNING: Bad signature, doesn't match file contents !\nBad signature from user ";
static	char	no_key [] = "WARNING: Can't check signature integrity, can't find public key\n         from user 0x";

/* Random number generator variables */

#define RAND_SIZE	256

static	int	rand_pointer = 0;
static	byte	init_rand = FALSE;
static	byte	privseed[RAND_SIZE];
static	byte	random_seed[RAND_SIZE];

#ifdef CRAP_STRSTR

/* Use this routine if either your machine doesn't have strstr () or it's
   crap like it seems to be on SunOS */

static	char	*strstr (s1, s2)

char	*s1;
char	*s2;

{
	char	*p, *q, *r;

	p = s1;
	while (*p) {
		if (*p == *s2) {
			q = p + 1;
			r = s2 + 1;

			while (*q) {
				if (!*r)
					return p;
				if (*r != *q)
					break;

				r++;
				q++;
			}
		}

		p++;
	}

	return NULL;
}
#endif

/* Routine to open a file in $PGPPATH */

static	FILE	*open_pgp_file (s,attr)

char	*s,*attr;

{
	char	temp[1024];
	char	*pgppath, *getenv();
	FILE	*fp;

	/* Try PGPPATH first */

	pgppath = getenv("PGPPATH");
	if (pgppath) {
		sprintf (temp, "%s/%s", pgppath, s);
		fp = fopen (temp, attr);

		if (fp)
			return fp;
	}

	/* If not, try local. This could possibly cause security problems,
	   so you may want to take it out with -DMOST_SECURE */

#ifdef MOST_SECURE
	return	NULL;
#else
	return	fopen (s, attr);
#endif
}

/* Copy a fifo into a buffer */

static	void	fifo_to_buffer (f,b)

struct	fifo	*f;
BUFFER	*b;

{
/* Should this ever get used on DOS, you might want to reduce the value of
   BUF_SIZE, or replace the stack use with a malloc() */

#ifdef DOS
#define BUF_SIZE	512
#else
#define BUF_SIZE	2048
#endif

	int	n,sz;
	char	buf [BUF_SIZE];

	sz = fifo_length (f);

	/* Loop round copying fifo into buffer */

	while (sz > 0) {
		if (sz > BUF_SIZE)
			n = BUF_SIZE;
		else
			n = sz;

		n = fifo_aget (buf, n, f);
		add_to_buffer (b, buf, n);

		sz -= n;
	}
}

/* 
   This is called instead of pgp_randombyte() to ensure that our random
   seed gets passed through. Note that this means that you should not
   use any PGPTools function with implied called to pgp_randombyte()
   without initialising the random number generator - a line

 	(void) our_randombyte();

   will do fine if you don't want to duplicate the code.
*/

byte	our_randombyte()

{
	if (!init_rand) {
		struct	fifo	*f;

		/* Ok, we have to pass privseed in */

		init_rand = TRUE;

		/* Start filling at the beginning again */

		rand_pointer = 0;

		/* Initialise the random number generator */

		f = fifo_mem_create ();
		fifo_aput (privseed, RAND_SIZE, f);
		pgp_initrand (f);
	}

	/* Now call it since we know it's initialised */

	return pgp_randombyte();
}
#endif

/* Add arbitrary data to the random number seed */

void	add_to_random (d, s)

byte	*d;
int	s;

{
#ifdef PGPTOOLS
	while (s--) {

		/* We xor the data into privseed, and copy it into 
		   random_seed, since that will be xor-ed into the buffer
		   in the PGPTools random number code */

		privseed [rand_pointer] ^= *d;
		random_seed [rand_pointer++] = *d++;

		/* Pass it to PGPTools if we reached the end of the buffer */

		if (rand_pointer == RAND_SIZE) {
			struct	fifo	*f;

			rand_pointer = 0;
			f = fifo_mem_create ();

			/* We pass privseed through the first time, then
			   random_seed (the new data) after that. That
			   way, privseed[] will match the contents of the
			   PGPTools random seed which xors all the data
			   passed into it. */

			if (init_rand) {
				fifo_aput (random_seed, RAND_SIZE, f);
				bzero (random_seed, RAND_SIZE);
			}
			else {
				fifo_aput (privseed, RAND_SIZE, f);
				init_rand = TRUE;
			}
			
			pgp_initrand (f);
		}
	}
#endif
}

/* Following should be called from user interface code whenever the
   user interacts at a random time with the program, to add random
   bits to the random number seed. It takes the time it was called, 
   fiddles with it, and xors it into the random seed data 

   Since the 256-bytes are hashed down to 16 bytes prior to use, I think
   that it's OK just to use eight bits of time per call. You may want
   to reduce that.
*/

void	update_random()

{
#ifdef PGPTOOLS
	byte	low_byte;
	struct	timeb	timestamp;

	/* Millitm probably isn't correct, but this XOR is unlikely
	   to make things worse than using just the seconds ! */

	ftime (&timestamp);

	/* Take low byte of time since 1970 */

	low_byte = (timestamp.time & 0xFF);

	/* Shift off bottom two bits of millisecond time to scale to 
	   0-250, and XOR in. The bottom bits can't be trusted anyway. */

	low_byte ^= (timestamp.millitm >> 2);

	/* Xor resulting byte into random seed */

	add_to_random (&low_byte, 1);

	/* Clear it all just in case anyone's looking */

	low_byte = 0;
	bzero (&timestamp, sizeof (timestamp));
#endif
}

#ifdef PGPTOOLS

/* We take the contents of the privseed array, then use the IDEA cipher with
   a key based on the current time and process id to encrypt the array. This
   gives slightly more security than just using the array as read from the
   disk. */

static	void	idea_privseed(decryp)

int	decryp;

{
	struct	timeb	timestamp;
	int	pid;
	byte	key[16], *p;
	word16	iv[4];		/* Don't really care what's in here, 
				   whatever's on the stack will do. */
	int	i;

	/* First we create an IDEA key with the current time and
	   process ID */

	ftime (&timestamp);

	p = (byte *)&timestamp.time;

	for (i = 0; i < 4; i++)
		key [i] = key [i+8] = *p++;

	p = (byte *)&timestamp.millitm;

	for (i = 0; i < 2; i++)
		key [i+4] = key [i+12] = *p++;

	bzero (&timestamp, sizeof (timestamp));

	/* Finish off with the process ID */

	pid = getpid();

	p = (byte *)&pid;

	for (i = 0; i < 2; i++) 
		key [i+6] = key [i+14] = *p++;

	pid = 0;

	/* Right, now we have the key, encrypt/decrypt the data */

	initcfb_idea (iv, key, decryp);
	ideacfb (privseed, RAND_SIZE);
	close_idea ();

	/* Now zero the key and iv */

	bzero (key, 16);
	bzero (iv, 8);
}

static	int	get_public_key (id, userid, key, trust)

byte	*id;
byte	*userid;
struct	pgp_pubkey	*key;
byte	*trust;

{
	FILE	*pkr;
	struct	fifo	*keyring;
	struct	fifo	*temp;
	int	revoked;
	byte	our_uid [256];

	/* First check the table */

#ifdef USE_HASH
	if (pgp_hash_get (id, userid, key, trust))
		return TRUE;
#endif

	/* Oh well, now for the hard work */

	pkr = open_pgp_file ("pubring.pgp", "rb");

	if (!pkr)
		return FALSE;

	keyring = fifo_file_create (pkr);
	temp = pgpk_findkey (keyring, id, FALSE);

	if (!temp) {
		fifo_destroy (keyring);
		fclose (pkr);

		return FALSE;
	}

	pgpk_extract_key (temp, key, (struct pgp_seckey *)NULL, 
		(byte *)NULL, &revoked);

	/* We want the userid whether they asked for it or not */

	if (!userid)
		userid = our_uid;

	if (!pgpk_extract_username (temp, userid, trust)) 
		*userid = 0;

	/* Add it to the table */

#ifdef USE_HASH
	pgp_hash_put (id, (char *)userid, key, trust);
#endif

	/* Tidy up */

	fifo_destroy (temp);
	fifo_destroy (keyring);
	fclose (pkr);

	return TRUE;
}
#endif

void	init_pgplib()

{
#ifdef PGPTOOLS
	FILE	*fp;

	/* Set mpilib precision */

	set_precision (MAX_UNIT_PRECISION);

	/* Init hash table */

#ifdef USE_HASH
	pgp_hash_clear ();
#endif

	/* Setup random number seeding */

	if (fp = open_pgp_file ("privseed.bin", "rb")) {
		fread (privseed, RAND_SIZE, 1, fp);
		fclose (fp);
	}

	/* If we didn't find the file, running it through the
	   IDEA cipher will provide us some slightly random output */

	idea_privseed(FALSE);

	/* We xor randseed.bin into the first 24 bytes just for 
	   luck if we find it */

	if (fp = open_pgp_file ("randseed.bin", "rb")) {
		byte	randseed[24];

		fread (randseed, 24, 1, fp);
		add_to_random (randseed, 24);
		bzero (randseed, 24);
		fclose (fp);
	}
#endif
}

void	close_pgplib()

{
#ifdef PGPTOOLS
	FILE	*fp;
	byte	old_random [RAND_SIZE];
	int	i;

	/* Close hash table */

#ifdef USE_HASH
	pgp_hash_off ();
#endif

	/* IDEA privseed array before dumping */

	idea_privseed(FALSE);

	/* XOR in the old file, in case any other program has been
	   using it while we're running. Even if we start up and
	   exit in a few seconds, the output should be different
	   enough from the input for this to be safe. 

	   Just in case, with #ifdef MOST_SECURE we will only do
	   this if init_rand is set (implying that we filled the
	   buffer with random-ish values at least once. */

#ifdef MOST_SECURE
	if (init_rand)
#endif
	if (fp = open_pgp_file ("privseed.bin", "rb")) {
		fread (old_random, RAND_SIZE, 1, fp);
		for (i = 0; i < RAND_SIZE; i++)
			if (old_random [i] != privseed [i]) {
				privseed [i] ^= old_random [i];
			}

		fclose (fp);
	}

	/* We just dump privseed to privseed.bin on exit */

	if (fp = open_pgp_file ("privseed.bin", "wb")) {
		fwrite (privseed, RAND_SIZE, 1, fp);
		fclose (fp);
	}
#endif
}

/* Clear all the output-grabbing buffers */

static	void	clear_output()

{
	clear_buffer(&error_messages);
	clear_buffer(&stdout_messages);
	clear_buffer(&ret_messages);
}

/* Add a string to the return messages buffer */

static	void	add_to_ret(buf,len)

byte	*buf;
int	len;

{
	add_to_buffer(&ret_messages,buf,len);
}

/* Add a string to the stdout buffer */

static	void	add_to_std(buf,len)

byte	*buf;
int	len;

{
	add_to_buffer(&stdout_messages,buf,len);
}

/* Add a string to the error messages buffer */

static	void	add_to_error(buf,len)

byte	*buf;
int	len;

{
	add_to_buffer(&error_messages,buf,len);
}

/* This routine runs PGP with the specified arguments. Currently it
   doesn't check that the program exists ! */

#ifndef PGPTOOLS
static	int	run_pgp(message,msg_len,args,pass)

byte	*message;
int	msg_len;
char	*args[];
char	*pass;

{
	run_program(pgp_path(),message,msg_len,args,pass);
}
#endif

int	run_program(prog,message,msg_len,args,pass)

char	*prog;
byte	*message;
int	msg_len;
char	*args[];
char	*pass;

{
	int	fd_in[2],fd_err[2],fd_out[2],pass_fd[2];
	int	from_pgp,to_pgp,pgp_error,pass_in;
	int	child_pid;
	fd_set	r_fdset,w_fdset;
#ifndef SYSV
	struct	rusage	rusage;
#endif
	int	statusp;
	int	fds_found;
	char	buf[4097];
	int	size;
	int	i;
#ifdef SYSV
	struct	timeval	t;

	t.tv_sec = 1;
	t.tv_usec = 0;
#endif

	clear_output();

	pipe(fd_in);
	pipe(fd_err);
	pipe(fd_out);

	if (pass)
		pipe (pass_fd);

	strcpy(buf,prog);

	if (!(child_pid=vfork())) {
		char	pass_env[32];

		dup2(fd_in[0],0);
		dup2(fd_out[1],1);
		dup2(fd_err[1],2);

		close(fd_in[1]);
		close(fd_out[0]);
		close(fd_err[0]);
		if (pass) {
			close (pass_fd[1]);
			sprintf	(pass_env,"PGPPASSFD=%d\n",pass_fd[0]);
			putenv (pass_env);
		}

		if (execvp(prog,args)<0) {
			_exit(23);
		}

		exit(1);
	}

	close(fd_in[0]);
	close(fd_out[1]);
	close(fd_err[1]);

	from_pgp=fd_out[0];
	to_pgp=fd_in[1];
	pgp_error=fd_err[0];

	if (pass) {
		pass_in = pass_fd[1];
		close (pass_fd[0]);
	}
	else
		pass_in = (-1);

#ifndef SYSV
	while (!wait4(child_pid,&statusp,WNOHANG,&rusage)) {
#else
	while (!waitpid (child_pid, &statusp, WNOHANG)) {
#endif

		FD_ZERO(&r_fdset);
		FD_SET(from_pgp, &r_fdset);
		FD_SET(pgp_error, &r_fdset);

		FD_ZERO(&w_fdset);
		if (to_pgp >= 0) {
			FD_SET(to_pgp,&w_fdset);
		}

		if (pass && pass_in >= 0) {
			FD_SET (pass_in, &w_fdset);
		}

#ifndef SYSV
		fds_found = select(getdtablesize(),&r_fdset,&w_fdset,0,0);
#else
		fds_found = select(FD_SETSIZE,&r_fdset,&w_fdset,0,&t);
#endif

		if (fds_found > 0) {
			if (FD_ISSET(pgp_error,&r_fdset)) {
				size = read(pgp_error,buf,4096);

				if (size > 0) {
					add_to_error(buf,size);
				}
			}

			if (pass_in >= 0 && FD_ISSET (pass_in,&w_fdset)) {
				if (*pass) {
					size = write(pass_in,pass,strlen(pass));
					pass += size;
				}

				if (!*pass) {
					write (pass_in,"\n",1);
					close (pass_in);
					pass_in = (-1);
#ifdef MORE_SECURE
					destroy_passphrase(FALSE);
#endif

				}
			}

			if (to_pgp >= 0 && FD_ISSET(to_pgp,&w_fdset)) {
				size = write(to_pgp,message,msg_len);

				if (size>0) {
					message += size;
					msg_len -= size;
				}

				if (msg_len <= 0) {
					close(to_pgp);
					to_pgp = (-1);
				}
			}

			if (FD_ISSET(from_pgp,&r_fdset)) {
				size = read(from_pgp,buf,4096);
	
				if (size > 0) {
					add_to_std(buf,size);
				}
			}

		}
	}

	/* Just in case */

#ifdef MORE_SECURE
	if (pass)
		destroy_passphrase(FALSE);
#endif

	/* Read remaining stdout data */

	do {
		size = read (from_pgp,buf,4096);
		if (size > 0) {
			add_to_std(buf, size);
		}
	} while (size > 0);

	close (from_pgp);

	/* Read remaining error info */

	do {
		size = read (pgp_error,buf,4096);
		if (size>0) {
			add_to_error(buf, size);
		}
	} while (size > 0);

	close (pgp_error);

	/* Close remaining open files */

	if (to_pgp >= 0)
		close (to_pgp);

	if (pass_in >= 0)
		close (pass_in);

	return statusp;
}

#ifndef PGPTOOLS
static	char	*filter_argv[]={
	"pgp",
	"-f",
	"+batchmode",
	NULL
};
#endif

#define LINE_LENGTH	1024

#ifndef PGPTOOLS
static	char	fail_string[] = "\n\n******* DECRYPTION FAILED *******\n\n";
static	char	retstring[]="\n";
#else
static	char	rsa_failed [] = "\n\n******* DECRYPTION FAILED *******\n\nMessage can only be read by :\n\n\t";
static	char	retstring [] = "\n\t";
#endif

#ifdef PGPTOOLS
static	char	hex_val (i)

{
	if (i < 10)
		return '0' + i;

	return 'A' + i - 10;
}
#endif

int	decrypt_message (message,decrypted,signature,pass,flags)

BUFFER	*message;
BUFFER	*decrypted;
BUFFER	*signature;
char	*pass;
int	flags;

{
	int	ret_val, i;
#ifdef PGPTOOLS
#define STRING_SIZE	256
	char	buf [STRING_SIZE];
	byte	userid [STRING_SIZE];
	int	l, revoked;
	struct	fifo	*inf = NULL;
	struct	fifo	*temp = NULL;
	struct	fifo	*sig = NULL;
	struct	fifo	*keyring = NULL;
	struct	fifo	*mess = NULL;
	struct	fifo	*tmess = NULL;
	struct	pgp_seckey	secret;
	struct	pgp_pubkey	public;
	byte	sig_type, trust;
	time_t	timestamp, etimestamp;
	word32	length;
	byte	key [16], phrase_md5 [16];
	byte	idea_key [16];
	MD5_CTX	context;
	FILE	*skr;
	byte	got_a_key = FALSE;
	byte	got_a_sig = FALSE;
	byte	got_a_rsa = FALSE;
	int	text = FALSE;
	time_t	dummy_ts;
	byte	sig_md5 [16], mess_md5 [16];
	BUFFER	*cant_decrypt = NULL;
	byte	*s, *e;
	byte	*t, *m;

	if (flags & FL_ASCII) {

		s = message->message;
		l = message->length;

		/* Skip the white space */

		while (isspace(*s) && l) {
			l--;
			s++;
		}

		/* Now, is it signed ? */

		if (!strncmp (s, begin_signed, strlen(begin_signed) - 1)) {

			/* Yep, clearsigned - let's process it ! */

			/* Start by skipping header lines */

			for (;l && *s != '\n';s++, l--);
			for (s++;l && *s != '\n'; s++, l--);
			s++;

			temp = fifo_mem_create ();
			fifo_aput (s, l, temp);

			fifo_find ((byte *)"\n-----BEGIN PGP SIG", temp);
			fifo_find ((byte *)"Version", temp);
			fifo_find ((byte *)"\n\n", temp);

			sig = fifo_mem_create ();
			pgp_extract_armor (temp, sig);
			fifo_destroy (temp);

			/* Ok, now we have the sig. We now need to check
			   it matches the file */
			
			if ((fifo_rget (0, sig) & 0xFC) != PGP_SIG) {

			bad_sig_bad_file:

				/* Dunno what sort this is, return error */

				fifo_destroy (sig);
				add_to_buffer (decrypted, message->message,
					message->length);
				return DEC_BAD_FILE;
			}

			e = (byte *)strstr (s, begin_signature);

			if (!e)
				goto bad_sig_bad_file;

			pgp_get_keyid (sig, key);
			
			if (!get_public_key (key, userid, &public, &trust)) {

				/* Copy stuff over, then return error */

				add_to_buffer (decrypted, s, e - s);

				add_to_buffer (signature, no_key,
					strlen (no_key));

				for (i = 0; i < 8 ; i++) {
					buf [i*2] = hex_val ((key [i] & 0xF0) >> 4);
					buf [i*2+1] = hex_val (key [i] & 0xF);
				}
				buf[16] = '.';
				add_to_buffer (signature, buf, 17);

				fifo_destroy (sig);
				return SIG_NO_KEY;
			}

			if (!pgp_extract_sig (sig, sig_md5, &timestamp,
				&sig_type, &public)) 
				goto bad_sig_bad_file;

			/* Ok, we got the sig, now calculate the MD5 of the
			   message */

			fifo_destroy (sig);
			mess = fifo_mem_create ();

			/* OK, we have to loop through copying the data over,
			   and removing any '-' at the beginning of a line.
			   Blurgh ! */

			if (!strncmp("- ", s, 2))
				s+= 2;

			t = s;

			while (t < e) {
				m = (byte *)strstr (t, "\n- ");

				if (!m || m > e) {
					fifo_aput (t, (e - t), mess);
					break;
				}
				fifo_aput (t, (m - t) + 1, mess);
				t = m + 3;
			}

			if (sig_type) {
				tmess = fifo_mem_create ();
				pgp_textform (fifo_copy (mess), tmess, TRUE, TRUE);
			}

			fifo_to_buffer (mess, decrypted);
			fifo_destroy (mess);

			etimestamp = timestamp;
			endian (&etimestamp, 4);

			MD5Init (&context);
			pgp_md5 (tmess, &context);
			MD5Update (&context, &sig_type, 1);
			MD5Update (&context, (byte *) &etimestamp, 4);
			MD5Final (mess_md5, &context);

			bzero (&context, sizeof (context));
			add_to_random (mess_md5, 16);

			/* Now verify */

			if (memcmp (mess_md5, sig_md5, 16)) {
				add_to_buffer (signature, bad_sig,
					strlen (bad_sig));
				sprintf (buf, "%s\".", userid);
				add_to_buffer (signature, buf,
					strlen (buf));

				ret_val = SIG_BAD;
			}
			else {
				add_to_buffer (signature, good_sig,
					strlen (good_sig));

				sprintf(buf, "\"%s\".\nSignature made ",
					userid);
				s =(byte *) ctime (&timestamp);

				add_to_buffer (signature, buf, strlen (buf));
				add_to_buffer (signature, s, strlen (s) - 1);

				ret_val = SIG_GOOD;
			}

			/* Clear everything for luck */

			bzero (&public, sizeof (public));
			bzero (&secret, sizeof (secret));
			bzero (key, 16);
			bzero (mess_md5, 16);
			bzero (sig_md5, 16);
			bzero (userid, STRING_SIZE);

			timestamp = etimestamp = 0;

			return ret_val;
		}

		if (!strncmp (s, begin_armour, strlen (begin_armour))) {

			/* Copy to memory temporarily */

			temp = fifo_mem_create ();
			fifo_aput (s, l, temp);

			/* Ok, it's armored, process it */

			fifo_find ((byte *)"Version", temp);
			fifo_find ((byte *)"\n", temp);

			/* Extract the armored file */

			inf = fifo_mem_create ();

			pgp_extract_armor (temp, inf);
			fifo_destroy (temp);
		}
		else {
			inf = fifo_mem_create ();
			fifo_aput (message->message, message->length, inf);
		}
		
		/* Decrypt - this code is based on that in ptd.c in the
		   PGPTools distribution */

		ret_val = SIG_NONE;

		while (1) {

			switch (fifo_rget (0, inf) & 0xFC) {

				case PGP_CKE:

				/* IDEA packet */

				if (!got_a_key) {
					if (!pass) {
						fifo_destroy (inf);
						if (cant_decrypt)
							free_buffer (cant_decrypt);
						return DEC_BAD_PHRASE;
					}

					MD5Init (&context);
					MD5Update (&context, (byte *)pass, (unsigned)strlen (pass));
					MD5Final (idea_key, &context);

					bzero (&context, sizeof (context));
				}

				/* IDEA decrypt and verify */

				mess = fifo_mem_create ();
				if (!pgp_extract_idea (inf, mess, idea_key)) {
					fifo_destroy (inf);
					fifo_destroy (mess);
					bzero (idea_key, 16);

					if (!got_a_key && got_a_rsa) {
						add_to_buffer (decrypted,
							cant_decrypt->message,
							cant_decrypt->length);

						free_buffer (cant_decrypt);
						return DEC_NO_KEY;
					}

					if (cant_decrypt)
						free_buffer (cant_decrypt);

					return DEC_BAD_PHRASE;
				}

				if (cant_decrypt) {
					free_buffer (cant_decrypt);
					cant_decrypt = NULL;
				}

				/* Yay ! Decrypted it */

				bzero (idea_key, 16);

				/* Next time round the loop */

				fifo_destroy (inf);
				inf = mess;
				break;

				/* RSA block */

				case PGP_PKE:

				got_a_rsa = TRUE;

				mess = fifo_copy (inf);

				pgp_get_keyid (inf, key);
				skr = open_pgp_file ("secring.pgp", "rb");

				if (!skr) {
				failed_decrypt_no_key:
					bzero (key, 16);
					fifo_destroy (inf);
					if (cant_decrypt)
						free_buffer (cant_decrypt);

					return SIG_NO_KEY;
				}

				keyring = fifo_file_create (skr);

				if (!(temp = pgpk_findkey (keyring, key, FALSE))) {
					int32	length;
					byte	t;
					int	c;

					fifo_destroy (keyring);
					fclose (skr);

					if (!cant_decrypt) {
						cant_decrypt = new_buffer ();
						add_to_buffer (cant_decrypt,
							rsa_failed,
							strlen (rsa_failed));
					}

					if (get_public_key (key, userid,
						&public, &trust)) {
						add_to_buffer (cant_decrypt,
							userid, 
							strlen (userid));
						add_to_buffer (cant_decrypt,
							retstring,
							strlen (retstring));
					}
					else {
						strcpy (buf, "KeyID : ");
						c = strlen (buf);

						for (i = 0; i < 8 ; i++) {
							buf [c+i*2] = hex_val ((key [i] & 0xF0) >> 4);
							buf [c+i*2+1] = hex_val (key [i] & 0xF);
						}
						buf[c+16] = '\n';
						buf[c+17] = '\t';

						add_to_buffer (cant_decrypt,
							buf, 
							c+18);
					}

					bzero (key, 16);

					fifo_destroy (inf);
					inf = mess;

					/* Need to skip to next packet */

					pgp_examine_packet (inf, &t, &length);
					fifo_skipn (inf, length);

					break;
				}

				fifo_destroy (mess);

				if (pgpk_extract_key (temp, &public,
					&secret, NULL, NULL)) {

					if (!pass) {
						bzero (&secret, sizeof (secret));
						bzero (&public, sizeof (public));
						bzero (key, 16);
						fifo_destroy (temp);
						fifo_destroy (keyring);
						fifo_destroy (inf);
						fclose (skr);

						if (cant_decrypt)
							free_buffer (cant_decrypt);

						return DEC_BAD_PHRASE;
					}

					/* It's encrypted, so we need the passphrase */

					/* Calculate the MD5 of the passphrase */

					MD5Init (&context);
					MD5Update (&context, (byte *)pass, (unsigned)strlen (pass));
					MD5Final (phrase_md5, &context);

					bzero (context, sizeof (context));
					fifo_destroy (temp);

					/* Decrypt the secret key */

					if (!pgp_decrypt_sk (&secret, phrase_md5)) {

						/* Probably isn't neccesary, but just for luck ! */

						fifo_destroy (keyring);
						fclose (skr);

						bzero (phrase_md5, 16);
						bzero (&secret, sizeof (secret));
						bzero (&public, sizeof (public));
	
						if (cant_decrypt)
							free_buffer (cant_decrypt);
						return DEC_BAD_PHRASE;
					}

					/* Right, ready to go */

					bzero (phrase_md5, 16);
				}

				fifo_destroy (keyring);
				fclose (skr);

				if (!pgp_extract_rsa (inf, idea_key, &public,
					&secret)) {
					fifo_destroy (inf);

					bzero (idea_key, 16);
					bzero (&secret, sizeof (secret));
					bzero (&public, sizeof (public));

					if (cant_decrypt)
						free_buffer (cant_decrypt);

					return DEC_BAD_FILE;
				}

				bzero (&secret, sizeof (secret));
				bzero (&public, sizeof (public));

				got_a_key = TRUE;

				break;

				/* Signature */

				case PGP_SIG:

				pgp_get_keyid (inf, key);

				if (!get_public_key (key, userid, &public,
					&trust)) {
					int32	length;
					byte	t;

					ret_val = SIG_NO_KEY;

					/* Need to skip to next packet */

					pgp_examine_packet (inf, &t, &length);
					fifo_skipn (inf, length);

					break;
				}

				bzero (key, 16);

				if (!pgp_extract_sig (inf, sig_md5, &timestamp,
					&sig_type, &public)) {

					fifo_destroy (inf);
					bzero (idea_key, 16);
					bzero (sig_md5, 16);
					bzero (&timestamp, 4);
					bzero (&public, sizeof (public));

					if (cant_decrypt)
						free_buffer (cant_decrypt);

					return DEC_BAD_FILE;
				}

				got_a_sig = TRUE;
				bzero (&public, sizeof (public));

				break;

				case PGP_CMP:

				/* Compressed */

				temp = fifo_mem_create ();
				pgp_extract_zip (inf, temp);
				inf = temp;
				break;

				case PGP_PT:
				temp = fifo_mem_create ();
				pgp_extract_literal (inf, temp, &text, buf,
					&dummy_ts);
				fifo_destroy (inf);

				if (cant_decrypt) {
					free_buffer (cant_decrypt);
					cant_decrypt = NULL;
				}

				if (got_a_sig) {
					MD5Init (&context);
					pgp_md5 (fifo_copy (temp), &context);
					MD5Update (&context, &sig_type, 1);

					etimestamp = timestamp;
					endian (&etimestamp, 4);

					MD5Update (&context, (byte *) &etimestamp, 4);
					MD5Final (mess_md5, &context);

					bzero (context, sizeof (context));

					/* Verify signature and create return info */

					if (memcmp (mess_md5, sig_md5, 16)) {
						add_to_buffer (signature, bad_sig,
							strlen (bad_sig));
						sprintf (buf, "%s\".", userid);
						add_to_buffer (signature, buf,
							strlen (buf));

						ret_val = SIG_BAD;
					}
					else {
						add_to_buffer (signature, good_sig,
							strlen (good_sig));

						sprintf(buf, "\"%s\".\nSignature made ",
							userid);
						s =(byte *) ctime (&timestamp);

						add_to_buffer (signature, buf, strlen (buf));
						add_to_buffer (signature, s, strlen (s) - 1);

						ret_val = SIG_GOOD;
					}

					bzero (mess_md5, 16);
					bzero (sig_md5, 16);
				}

				if (text) {
					mess = fifo_mem_create ();
					pgp_textform (temp, mess, FALSE, TRUE);
				}
				else
					mess = temp;

				fifo_to_buffer (mess, decrypted);
				fifo_destroy (mess);

				return ret_val;
			}
		}
	}
#else
	char	*s,*e;
	char	line[LINE_LENGTH+1];
	int	j,c;
	int	sig_lines = 0;

	run_pgp(message->message,message->length,filter_argv,pass);

	ret_val = SIG_GOOD;

	if (error_messages.message && strstr((char *)error_messages.message,"WARNING")) {

		/* Ignore 'not certified' warnings, unless
		   accompanied by 'Bad signature' */

		if (!strstr((char *)error_messages.message,"not certified") ||
			strstr((char *)error_messages.message,
			"Bad signature, doesn't match"))
			ret_val = SIG_BAD;

		/* Oh, poo, we don't have the public key to check the
		   signature ! */

		if (strstr((char *)error_messages.message,"Can't find the right"))
			ret_val = SIG_NO_KEY;
	}

	i=0;
	j=0;

	/* Process the output, looking for the signature lines */

	do {
		c = line[j++] = error_messages.message[i++];
		if (c == '\n' || j == LINE_LENGTH || 
			i == error_messages.length) {
			line[j] = 0;

			/* Ignore warnings about low confidence */

			if (strstr(line,"ignature") && 
				!strstr(line,"onfidence") &&
				!strstr(line,"equired")) {
				add_to_buffer(signature,line,j);
				sig_lines ++;
			}
			j = 0;
		}
	} while (i<error_messages.length);

	/* If we didn't find any lines, it wasn't signed */

	if (!sig_lines)
		ret_val = SIG_NONE;

	/* Copy the decrypted message to the buffer */

	add_to_ret(stdout_messages.message,stdout_messages.length);

	/* Do we not have the secret key ? */

	if (strstr((char *)error_messages.message,"not have the secret")) {
		ret_val = DEC_NO_KEY;

		add_to_ret (fail_string, strlen(fail_string));

		/* Ok, let them know who the message is for ! */

		s = strstr ((char *)error_messages.message, "This message can only");
		if (s) {
			e = strstr (s, "\n\n");

			if (e) 
				add_to_ret (s, (e-s)+1);
		}
	}

	ret_messages.message[ret_messages.length] = 0;

	add_to_buffer(decrypted,ret_messages.message,ret_messages.length);

	if (strstr((char *)error_messages.message,"Bad pass")) {
		ret_val = DEC_BAD_PHRASE;
	}

	return ret_val;
#endif
}

/* Note - user is a list of users, not just one ! */

int	encrypt_message(user,message,encrypted,flags,pass,key_name)

char	**user;
BUFFER	*message;
BUFFER	*encrypted;
int	flags;
char	*pass;
char	*key_name;

{
#ifdef PGPTOOLS
	FILE	*skr;
	FILE	*pkr;
	struct	fifo	*keyring;
	struct	fifo	*secret_key;
	struct	fifo	*public_key;
	struct	fifo	*signature;
	struct	fifo	*armoured;
	struct	fifo	*mess;
	struct	fifo	*tmess;
	struct	fifo	*outmess;
	struct	pgp_seckey	secret;
	struct	pgp_pubkey	public;
	byte	phrase_md5 [16];
	byte	mess_md5[16];
	byte	key[16];
	MD5_CTX	context, sig_context;
	time_t	timestamp, etimestamp;
	byte	text = 0, signature_type = 0;
	int	i;
	int	revoked;

	/* If we're encrypting, open pubring.pgp */

	if (flags & FL_ENCRYPT) {
		pkr = open_pgp_file ("pubring.pgp", "rb");
		if (!pkr)
			return ERR_NO_KEY;
	}

	/* If we're going to have to sign anything, get the secret key */

	if (flags & FL_SIGN) {

		/* Oops, no passphrase */

		if (!pass)
			return ERR_BAD_PHRASE;

		/* Open the secret key file */

		skr = open_pgp_file ("secring.pgp", "rb");

		/* Check the file is there ! */

		if (!skr)
			return ERR_NO_SECRET_KEY;

		/* Read the secret key */

		keyring = fifo_file_create (skr);
		secret_key = pgpk_findkey (keyring, (byte *)key_name, (int)TRUE);
		fifo_destroy (keyring);

		/* Close the keyring */

		if (skr)
			fclose (skr);

		/* Did we get it ? */

		if (!secret_key) {
			return ERR_NO_SECRET_KEY;
		}

		/* Extract the key */

		if (pgpk_extract_key (secret_key, &public, &secret,
			NULL, NULL)) {

			/* It's encrypted, so we need the passphrase */

			/* Calculate the MD5 of the passphrase */

			MD5Init (&context);
			MD5Update (&context, (byte *)pass, (unsigned)strlen (pass));
			MD5Final (phrase_md5, &context);

			bzero (context, sizeof (context));

			/* Decrypt the secret key */

			if (!pgp_decrypt_sk (&secret, phrase_md5)) {

				/* Probably isn't neccesary, but just for luck ! */

				fifo_destroy (secret_key);

				bzero (phrase_md5, 16);
				bzero (&secret, sizeof (secret));
				bzero (&public, sizeof (public));
	
				return ERR_BAD_PHRASE;
			}

			/* Right, ready to go */

			bzero (phrase_md5, 16);
		}

		fifo_destroy (secret_key);
	}

	/* Copy input message into fifo */

	mess = fifo_mem_create ();
	fifo_aput (message->message, message->length, mess);

	/* Do encryption */

	if (flags & FL_ENCRYPT) {

		text = signature_type = 0;
		outmess = fifo_mem_create ();

		/* Do the timestamp */

		time (&timestamp);

		etimestamp = timestamp;
		endian (&etimestamp, 4);

		if (flags & FL_SIGN) {
			tmess = fifo_copy (mess);

			/* Create the MD5 for the signature */

			MD5Init (&sig_context);
			pgp_md5 (tmess, &sig_context);
			MD5Update (&sig_context, &signature_type, 1);
			MD5Update (&sig_context, (byte *)&etimestamp, 4);
			MD5Final (mess_md5, &sig_context);

			/* Add it to the random seed */

			add_to_random (mess_md5, 16);

			bzero (&sig_context, sizeof (sig_context));

			/* Create a signature in outmess */

			pgp_create_sig (outmess, mess_md5, timestamp, 
				signature_type, &public, &secret);
		}
		else {

			/* We need an md5, but don't have one, so create
			   it from the first few bytes of the message */

			i = message->length;
			if (i > 8192)
				i = 8192;

			MD5Init (&sig_context);
			MD5Update (&sig_context, message->message, i);
			MD5Update (&sig_context, (byte *)&etimestamp, 4);
			MD5Final (mess_md5, &sig_context);

			/* Add it to the random seed */

			add_to_random (mess_md5, 16);

			bzero (&sig_context, sizeof (sig_context));
		}

		pgp_create_literal (mess, outmess, text, "dev.null",
			timestamp);
		fifo_destroy (mess);

		/* Compress it, destroying outmess in the process */

		tmess = fifo_mem_create ();
		pgp_create_zip (outmess, tmess);

		/* Calculate the encryption key, using mess_md5 and
		   our_randombyte () */

		for (i = 0; i < 16; i++) 
			key [i] = our_randombyte() ^ mess_md5[i];

		/* Clear mess_md5 */

		bzero (mess_md5, 16);

		/* Uuurgh ! We can't use the hash table here ! */

		keyring = fifo_file_create (pkr);
		outmess = fifo_mem_create ();

		for (i = 0; user[i] != NULL; i++) {
			struct	fifo	*kring;

			/* We have to take a copy of keyring */

			kring = fifo_copy (keyring);
			public_key = pgpk_findkey (kring, (byte *)user[i], TRUE);
			fifo_destroy (kring);

			/* Did we find it ? */

			if (!public_key) {

				/* Oh no, no key ! */

				timestamp = etimestamp = 0l;
				bzero (key, 16);

				fifo_destroy (outmess);
				fifo_destroy (mess);
				fifo_destroy (tmess);
				fifo_destroy (keyring);
				fclose (pkr);

				return ERR_NO_KEY;
			}

			pgpk_extract_key (public_key, &public, 
				NULL, NULL, &revoked);

			pgp_create_rsa (outmess, key, &public);
		}

		/* Destroy keyring and close pubring.pgp */

		fifo_destroy (keyring);
		fclose (pkr);

		/* IDEA encrypt to outmess, destroys tmess */

		pgp_create_idea (tmess, outmess, key);

		/* Erase all records */

		timestamp = etimestamp = 0;
		bzero (key, 16);

		/* We now have the encrypted message in outmess, so return it */

		if (flags & FL_ASCII) {
			armoured = fifo_mem_create ();
			pgp_create_armor (outmess, armoured, 0);
			fifo_destroy (outmess);

			add_to_buffer (encrypted, begin_armour,
				strlen (begin_armour));
			add_to_buffer (encrypted, pgp_version, 
				strlen (pgp_version));
			add_to_buffer (encrypted, two_n, strlen (two_n));

			fifo_to_buffer (armoured, encrypted);
			add_to_buffer (encrypted, end_armour,
				strlen (end_armour));

			fifo_destroy (armoured);
		}
		else {
			fifo_to_buffer (outmess, encrypted);
			fifo_destroy (outmess);
		}

		return ERR_NONE;
	}

	/* Do we want a clearsigned message ? */

	else if (flags == (FL_ASCII | FL_SIGN)) {
		BUFFER	*tempb;
		byte	*t, *e, *s, *s2;
		
		/* Put begin signed message line to output */

		clear_output();
		add_to_buffer (encrypted, begin_signed, strlen(begin_signed));

		/* Set text mode flags */

		text = signature_type = 1;

		/* Create textified message in tmess */

		outmess = fifo_copy (mess);
		tmess = fifo_mem_create ();
		pgp_textform (mess, tmess, TRUE, TRUE);

		/* We have to prepend - to lines starting with F or - */

		tempb = new_buffer ();
		fifo_to_buffer (outmess, tempb);
		fifo_destroy (outmess);

		t = tempb->message;
		e = tempb->message + tempb->length;

		if (*t == 'F' || *t == '-')
			add_to_buffer (encrypted, "- ", 2);

		while (t < e) {
			s = (byte *)strstr (t, "\n-");
			s2 = (byte *)strstr (t, "\nF");

			if (!s || (s2 && s2 < s))
				s = s2;

			if (!s || s >= e) {
				add_to_buffer (encrypted, t, (e - t));
				t = e;
				break;
			}
			else {
				add_to_buffer (encrypted, t, (s - t));
				add_to_buffer (encrypted, "\n- ", 3);
				t = s + 1;
			}
		}

		free_buffer (tempb);
		
		/* Now that the message is in encrypted, add the begin 
		   signature line */

		add_to_buffer (encrypted, begin_signature, strlen(begin_signature));
		add_to_buffer (encrypted, pgp_version, strlen (pgp_version));
		add_to_buffer (encrypted, two_n, strlen (two_n));

		/* Do the timestamp */

		time (&timestamp);

		etimestamp = timestamp;
		endian (&etimestamp, 4);

		/* Create the MD5 for the signature */

		MD5Init (&sig_context);

		/* Remember, this call destroys tmess, so don't do it again ! */

		pgp_md5 (tmess, &sig_context);
		MD5Update (&sig_context, &signature_type, 1);
		MD5Update (&sig_context, (byte *)&etimestamp, 4);
		MD5Final (phrase_md5, &sig_context);

		/* Add it to the random seed */

		add_to_random (phrase_md5, 16);

		/* Create a signature */

		signature = fifo_mem_create();
		pgp_create_sig (signature, phrase_md5, timestamp, 
			signature_type, &public, &secret);

		/* Armour it */

		armoured = fifo_mem_create ();
		pgp_create_armor (signature, armoured, 0);	

		/* Copy armoured signature to output buffer */

		fifo_to_buffer (armoured, encrypted);

		/* Finally tag end-signature line on there */

		add_to_buffer (encrypted, end_signature, strlen(end_signature));

		/* Destroy remaining fifos */
	
		fifo_destroy (armoured);
		fifo_destroy (signature);

		/* Zero out encryption stuff */

		timestamp = 0l;
		etimestamp = 0l;

		/* YAY ! WE DID IT ! Drop through to the exit code */
	}

	else if (flags & FL_SIGN) {

		/* Here we're creating a signed, unencrypted message */

		text = signature_type = 0;

		tmess = fifo_copy (mess);
		outmess = fifo_mem_create ();

		MD5Init (&sig_context);
		pgp_md5 (tmess, &sig_context);
		MD5Update (&sig_context, &signature_type, 1);
		MD5Update (&sig_context, (byte *)&etimestamp, 4);
		MD5Final (mess_md5, &sig_context);

		add_to_random (mess_md5, 16);
		bzero (&sig_context, sizeof (sig_context));

		pgp_create_sig (outmess, mess_md5, timestamp, 
			signature_type, &public, &secret);

		bzero (mess_md5, 16);

		pgp_create_literal (mess, outmess, text, "dev.null",
			timestamp);
		fifo_destroy (mess);

		/* Outmess is destroyed when the zip is created */

		pgp_create_zip (outmess, tmess);

		fifo_to_buffer (tmess, encrypted);
		fifo_destroy (tmess);

		/* Drop through to tidyup and exit code */
	}

	else {
		/* Didn't ask us to do anything ! */

		fifo_destroy (mess);
		add_to_buffer (encrypted, message->message, message->length);
	}

	/* Clear it all in case we used it */

	timestamp = 0l;
	etimestamp = 0l;

	bzero (phrase_md5, 16);
	bzero (&secret, sizeof (secret));
	bzero (&public, sizeof (public));
	bzero (&context, sizeof (context));
	bzero (&sig_context, sizeof (sig_context));

	return ERR_NONE;
#else
	int	ret_val;
	char	args[3][32];
	char	**argv;
	int	argv_size = 10;
	int	arg = 2;

	strcpy(args[0],"-f");

	argv = (char **) malloc (argv_size * sizeof(char *));

	argv[0]="pgp";
	argv[1]=args[0];

	if (flags & FL_ENCRYPT) {
		strcat(args[0],"e");
	}
	if (flags & FL_ASCII) {
		strcat(args[0],"a");
	}
	if (flags & FL_SIGN) {
		strcat(args[0],"s");
		if (!(flags & FL_ENCRYPT)) {
			strcat(args[0],"t");
			argv[arg++] = "+clearsig=on";
		}
	}

	argv[arg++]="+batchmode";

	if (flags & FL_ENCRYPT) {
		char	**u;

		u = user;

		while (*u) {
			if (arg > (argv_size - 4)) {
				argv_size += 10;
				argv = (char **)realloc (argv, 
					argv_size * sizeof (char *));
			}

			argv[arg++] = *u++;	
		}
	}

	/* Make sure we have space */

	if (arg > (argv_size - 4)) {
		argv_size += 10;
		argv = (char **)realloc (argv, 
			argv_size * sizeof (char *));
	}

	/* If signing we have to pass the key name in too */

	if (flags & FL_SIGN) {
		argv [arg++] = "-u";
		argv [arg++] = key_name;
	}

	/* End it with a null */

	argv[arg++]=0;

	/* Zero return value */

	ret_val = ERR_NONE;

	/* And, finally, run the program */

	(void) run_pgp(message->message,message->length,argv,pass);

	add_to_buffer(encrypted,stdout_messages.message,
		stdout_messages.length);

	if (strstr((char *)error_messages.message,"Error") && 
		strstr((char *)error_messages.message,"Bad"))
		ret_val = ERR_BAD_PHRASE;

	if (strstr((char *)error_messages.message,"Signature error")&&
		strstr((char *)error_messages.message,"Keyring file")&&
		strstr((char *)error_messages.message,"not exist")) {
		ret_val = ERR_NO_SECRET_KEY;
	}

	if (strstr((char *)error_messages.message,"not found")) {
		ret_val = ERR_NO_KEY;
	}

	free (argv);
	
	return ret_val;
#endif
}

int	buffer_contains_key (b)

BUFFER	*b;

{
	char	*s;

	if (!b->message || !b->length)
		return FALSE;

	if (!strncmp ((char *)b->message, begin_key + 1, 
		strlen (begin_key) - 1))
		return TRUE;

	s = strstr ((char *)b->message, begin_key);

	if (s && s < (char *)(b->message + b->length))
		return TRUE;

	return FALSE;
}

