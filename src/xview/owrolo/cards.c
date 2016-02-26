/*
 *	Routines for manipulating cards
 */


/*
 * -------------------------------------------------------------------------
 *	ROLO - A Sun Tool to implement a Rolodex-style list of notes
 *
 *	This code manipulates "cards" in a visual manner approximating
 *	a rolodex file.  All the cards are stored in one real file, the
 *	cards are seperated by a ^L (form-feed).  The default path
 *	name is $HOME/.rolo.  A different pathname may be specified at
 *	startup on the command line.  The pathname is relative to the
 *	user's home directory.
 *
 *	Due to bugs in the 3.0 distribution, especially with text subwindows,
 *	this code is only guaranteed to compile and run properly with 3.2
 *	or greater.
 *
 *	This code is public domain, anyone and everyone is welcome to it.
 *	All I ask is that my name and this notice remain on it.  If Sun would
 *	like to bundle it with their product they are welcome to do so,
 *	I only ask that the sources be included in the binary distribution.
 *
 *	Please return any fixes, improvements, gripes, etc to me.
 *
 *	Ron Hitchens		ronbo@vixen.uucp
 *	March 1987 (V1.0)	hitchens@cs.utexas.edu
 *	August 1988 (V2.0)
 * -------------------------------------------------------------------------
 */



#include <stdio.h>
#include <sys/file.h>
#include <ctype.h>

#include "defs.h"


/* --------------------------- Exports ------------------------------------- */

int			need_save = FALSE;

struct card		*first = NULL_CARD, *last, *current;

struct card		*make_card (), *insert_card (), *undelete_card (),
			*pop_card ();

void			dispose_card (), push_card (), nuke_active_cards ();

char			*first_char (), *check_args ();


/* --------------------------- Imports ------------------------------------- */

extern char		*rolofile;

extern void		show_card (), set_slider_max ();

extern char		*malloc(), *realloc (), *calloc (), *getenv(),
			*strcpy(), *strcat(), *strncpy (), *index (),
			*sys_errlist [];

extern int		errno;


/* --------------------------- Locals -------------------------------------- */

/*static */ struct card	*dead;

static int		eof = FALSE;

static char		*dummy_card = DUMMY_CARD_CONTENTS;

static char		*trim (), *catbuf (), *get_card ();

static void		load_rolo (), write_err ();


/* ------------------------------------------------------------------------- */


/*
 *	Initialize the card list.  First, we cd to the home directory,
 *	then we try to open the named file.  If we don't find one then
 *	we create a new empty one and re-open it.  Once we have the file
 *	open, we try to load cards in.  If no cards are loaded by load_rolo(),
 *	then we create a dummy card out of thin air.
 */

void
init_rolo (filename)
	char	*filename;
{
	char	*c;
	int	fd;
	FILE	*f;

	/*
	(void) chdir (getenv ("HOME"));
	*/
	if ((f = fopen (filename, "r")) == NULL) {
		if ((fd = open (filename, O_CREAT, NEWMODE)) < 0) {
			(void)msg ("init_rolo: can't create %s: %s", filename,
				sys_errlist [errno]);
			goto skipload;
		}
		(void) close (fd);
		if ((f = fopen (filename, "r")) == NULL) {
			(void)msg ("init_rolo: can't reopen new %s\n",
				filename);
			goto skipload;
		}
	}
	load_rolo (f);
	(void) fclose (f);

skipload:
	if (first == NULL_CARD) {
		c = malloc (strlen (dummy_card) + 1);
		(void) strcpy (c, dummy_card);
		(void) insert_card (make_card (c), NULL_CARD);
		set_slider_max (renumber (first));
		insert_card_list(first); /* into Xview Panel card list */
	}

	show_card (first);
}


/*
 *	Concatenate the second string onto the first.  If the first string
 *	pointer is null, copy and return the second.
 */

static
char *
catbuf (c, p)
	char	*c, *p;
{
	if (c == NULL) {
		c = malloc (strlen (p) + 1);
		return (strcpy (c, p));
	} else {
		c = realloc (c, strlen (c) + strlen (p) + 1);
		return (strcat (c, p));
	}
}


/*
 *	Return the pointer to the first non-blank char in the given string.
 *	If the string is all white space, a pointer to the trailing null
 *	will be returned.
 */

char *
first_char (c)
	char	*c;
{
	while ((*c != '\0') && (isspace(*c)))
		c++;
	return (c);
}


/*
 *	Trim trailing blanks from a string.  The trimmed blanks, if any, are
 *	not freed.  Return the address of the string passed.  If a null
 *	pointer is passed, this is a no-op.
 */

static
char *
trim (c)
	char	*c;
{
	int	i, l;

	if (c == NULL) {
		return (c);
	}
	l = strlen (c) - 1;

	for (i = l; i >= 0; i--) {
		if ( ! isspace(c [i]))
			break;
	}

	c [++i] = '\0';
	return (c);
}


/*
 *	Get one card from the provided file.  Data is read, a line at a time,
 *	until a form-feed char is encountered.  The FF is expected to be
 *	on a line by itself, the cards are written out that way.  Blanks cards
 *	will be discarded.
 *	The #ifdef'ed code will trim off leading blank lines, it is disabled
 *	since users may wish to include leading white space in a card.  The
 *	routines which look at card values (such as for sorting) will skip
 *	leading white space anyway.
 */

static
char *
get_card (f)
	FILE	*f;
{
	char	*c;
	char	buf [1024];

	if (eof) {
		return (NULL);
	}
	c = NULL;
	while (fgets (buf, sizeof buf, f) != NULL) {
		if (buf [0] == '\f') {		/* card separator */
			if (c == NULL || strlen (c) == 0) {
				if (c != NULL) {
					free (c);
				}
				c = NULL;
				continue;
			}
			return (trim(c));
		}
#ifdef notdef
		if (c == NULL) {		/* drop leading blank lines */
			int	i, j, allblank;

			j = strlen (buf);
			allblank = TRUE;
			for (i = 0; i < j; i++) {
				if ( ! isspace(buf[i])) {
					allblank = FALSE;
					break;
				}
			}
			if (allblank) {
				continue;
			}
		}
#endif
		c = catbuf (c, buf);
	}
	eof = TRUE;
	return (trim(c));
}


/*
 *	Load all the cards from the provided file.
 */

static
void
load_rolo (f)
	FILE	*f;
{
	char	*c;
	struct	card *p;
	static	int start=1;

	eof = FALSE;
	first = last = current = NULL_CARD;
	if (start)
		dead = NULL_CARD;
	start = 0;
	while ((c = get_card (f)) != NULL) {
		(void) insert_card (make_card (c), last);
	}
	set_slider_max (renumber (first));
	for( p = first; p != NULL; p = p->c_next )	
		insert_card_list(p);	/* into the active list */

	need_save = FALSE;
}


/*
 *	Renumber the cards in the list, begnning with the card pointed to
 *	by p (this should normally be the first card in the list).  This must
 *	be done whenever the population of the list changes, such as adding
 *	or deleting a card.  The first card is number 1.  The card number
 *	is generally used for display to human types.  The number of cards
 *	in the list is returned.
 */

int
renumber (p)
	struct	card	*p;
{
	int	i;

	for (i = 0; p != NULL_CARD; p = p->c_next)
		p->c_num = ++i;

	return (i);
}


/*
 *	Make a new card, and place the text pointed to by c into it.  If
 *	the text pointer is null, allocate an empty string for the text,
 *	the text pointer in the card's c_text slot must be free-able later
 *	if the user modifies the text.  If a text pointer is provided it
 *	is assumed to have been malloc'ed and is not copied.  The address of
 *	the new card is returned, it is not inserted into the list.
 */

struct card *
make_card (c)
	char	*c;
{
	struct	card	*p;

	p = (struct card *) malloc ((int)sizeof (struct card));
	if (p == NULL_CARD) {
		(void)msg ("Can't allocate memory for new card");
		return (NULL);
	}

	p->c_next = p->c_prev = NULL_CARD;
	if (c == NULL) {
		p->c_text = malloc (1);
		*p->c_text = '\0';
	} else {
		p->c_text = c;
	}
	return (p);
}


/*
 *	Insert the card p into the list of cards after the card pointed to
 *	by after.  If after is null, then insert before the first card.  If
 *	the list is initially empty, after is ignored and p is made the only
 *	card in the list.  The card list is doubly linked, standard textbook
 *	stuff.  The global pointers first, last and current are set as
 *	side-effects by this function.  The address of the card being
 *	inserted is returned as a convenience to the caller (see undelete_card)
 */

struct card *
insert_card (p, after)
	struct	card	*p, *after;
{
	if (p == NULL_CARD) {
		return (p);		/* paranoid programming */
	}

	if (first == NULL_CARD) {	/* list is initially empty */
		p->c_next = p->c_prev = NULL;
		first = last = current = p;
		return (p);
	}

	if (after == NULL_CARD) {	/* at front of list */
		first->c_prev = p;
		p->c_next = first;
		p->c_prev = NULL_CARD;
		first = p;
		return (p);
	}

	p->c_next = after->c_next;
	p->c_prev = after;

	if (after == last) {
		last = p;		/* at end of list */
	} else {
		after->c_next->c_prev = p;
	}

	after->c_next = p;

	return (p);
}


/*
 *	Delete the card pointed to by p from the list.  More textbook stuff
 *	here.  The card is not destroyed, it is clipped out of the list and
 *	pushed onto a stack of deleted cards.
 */

void
delete_card (p)
	struct	card	*p;
{
	if (p == first) {
		first = p->c_next;
	} else {
		p->c_prev->c_next = p->c_next;
	}

	if (p == last) {
		last = p->c_prev;
	} else {
		p->c_next->c_prev = p->c_prev;
	}

	push_card (p);
}


/*
 *	Undelete a card.  The top card on the deleted stack is popped and
 *	inserted into the list after the card pointed to by after.
 */

struct card *
undelete_card (after)
	struct	card	*after;
{
	struct	card	*p;

	p = pop_card ();
	if (p == NULL_CARD) {
		return (NULL_CARD);
	}
	return (insert_card (p, after));
}


/*
 *	Destroy the entire list and all the cards in it.  The cards on the
 *	deleted stack are not disturbed.
 */

void
nuke_active_cards ()
{
	struct card	*p = first;

	while (p != NULL_CARD) {
		struct card	*q;

		remove_top_card_from_list();
		q = p->c_next;
		dispose_card (p);
		p = q;
	}

	first = last = current = NULL_CARD;
}


/*
 *	Destroy the card pointed to by p, and free its storage.  The pointer
 *	p is no longer valid after calling this function.
 */

void
dispose_card (p)
	struct	card	*p;
{
	free (p->c_text);
	free (p);
}


/*
 *	Push the card pointed to by p onto the stack of deleted cards. Double
 *	link the new node onto the front of the list so that any arbitrary
 *	card can be easily unlinked without needing to scan the list.
 */

void
push_card (p)
	struct	card	*p;
{
	if (strlen (first_char (p->c_text)) == 0) {
		dispose_card (p);
		del_insert_list(p,1); /*delete entry from active list*/
		return;			/* don't keep empty cards */
	}
				/* delete entry from active list */
	del_insert_list(p,0);   /* insert entry into undelete list */

	p->c_prev = NULL_CARD;		/* this guy will be first in the list */
	p->c_next = dead;		/* current #1 will be new #2 */
	if (dead != NULL_CARD) {	/* if list was non-empty, back-point */
		dead->c_prev = p;	/* the first one to the new one */
	}
	dead = p;			/* head of list is now the new one */
}


/*
 *	Pop the top card off the deleted stack and and return its address,
 *	or a nil pointer if the stack is empty.
 */

struct card *
pop_card ()
{
	struct	card	*p;

	if (dead == NULL_CARD) {
		return (NULL_CARD);	/* sorry, fresh out */
	}

	p = dead;			/* returning first one in the list */
	dead = p->c_next;		/* head of list is now second node */
	if (dead != NULL_CARD) {	/* If any nodes left in the list, */
		dead->c_prev = NULL_CARD; /* clear back pointer of first one */
	}

	return (p);			/* here's the top card, Bud */
}


/*
 *	Remove the card pointed to by p from the deleted stack.  The function
 *	is called by the action proc for the undeleted pullright menu.  The
 *	node pointed to may be any one of the cards on the stack.  The node
 *	is clipped out of the deleted list.
 */

void
unstack_card (p)
	struct	card	*p;
{
	if (p == dead) {		/* is it the first one? */
		dead = p->c_next;		/* head of list is now next */
	} else {
		/* point the upstream guy at the downstream guy */
		p->c_prev->c_next = p->c_next;
	}

	if (p->c_next != NULL_CARD) {
		/* point the downstream guy at the one upstream of me */
		p->c_next->c_prev = p->c_prev;
	}

	p->c_next = p->c_prev = NULL_CARD;	/* this guy is alone now */
}


/*
 *	Write the contents of all the cards out to disk.  Blank cards are
 *	not written out, and trailing white space is trimmed from the ones
 *	which are written.  The cards are seperated in the file by a
 *	form-feed (^L) on a line by itself.
 */

void
dump_rolo (p, fn)
	struct	card	*p;
	char	*fn;
{
	int	fd;

	fd = open (fn, O_WRONLY|O_TRUNC, NEWMODE);
	if (fd < 0) {
		(void)msg ("Can't open %s for save: %s", fn,
			sys_errlist [errno]);
		return;
	}

	for ( ; p != NULL_CARD; p = p->c_next) {
		if (strlen (trim (p->c_text)) == 0)
			continue;
		if (write (fd, p->c_text, strlen (p->c_text)) == -1) {
			write_err (fd, fn);
			return;
		}
		if (write (fd, "\n\f\n", 3) == -1) {
			write_err (fd, fn);
			return;
		}
	}

	(void) close (fd);
	need_save = FALSE;
}


/*
 *	Write the cards out to the named file.  If the file exists already,
 *	ask if it's ok to overwrite.  Check permissions.  Pre-create the
 *	file if it doesn't already exist.
 */

void
write_rolo (filename)
	char	*filename;
{
	int	fd;

	if (access (filename, F_OK) == -1) {
		if ((fd = open (filename, O_CREAT, NEWMODE)) < 0) {
			(void)msg ("write_rolo: can't create %s: %s", filename,
				sys_errlist [errno]);
			return;
		}
		(void) close (fd);
		goto do_dump;
	}

	if (access (filename, W_OK) == -1) {
		(void)msg ("Sorry, you don't have write access to %s", filename);
		return;
	}

	if (confirm ("%s exists, ok to overwrite?", filename) == FALSE) {
		return;
	}

do_dump:
	dump_rolo (first, filename);
	rolofile = filename;
	update_num_display (current->c_num);
}


/*
 *	Read in a new Rolo card file.  Check for readability before throwing
 *	away the old cards.
 */

void
read_rolo (filename)
	char	*filename;
{
	if (access (filename, F_OK) == -1) {
		(void)msg ("Sorry, %s doesn't exist.", filename);
		return;
	}

	if (access (filename, R_OK) == -1) {
		(void)msg ("Sorry, you don't have permission to read %s", filename);
		return;
	}

	nuke_active_cards ();
	init_rolo (filename);
	rolofile = filename;
	update_num_display (current->c_num);
}


/*
 *	Common error complaint routine used by dump_rolo above.
 */

static
void
write_err (fd, fn)
	int	fd;
	char	*fn;
{
	(void) close (fd);
	(void)msg ("Couldn't save to %s: %s", fn, sys_errlist [errno]);
}


/*
 *	Glue routine for the qsort compare routine.  Dereference the card
 *	pointers and call strcmp to compare the text of the cards, beginning
 *	with the first non-blank character in the card.
 */

static
int
sort_up (pp1, pp2)
	struct card	**pp1, **pp2;
{
	return (strcmp (first_char ((*pp1)->c_text),
		first_char ((*pp2)->c_text)));
}


/*
 *	Same as above, but for sort descending.  Strcmp is called with the
 *	string pointers in the opposite order to invert the sense of the
 *	comparison.
 */

static
int
sort_down (pp1, pp2)
	struct card	**pp1, **pp2;
{
	return (strcmp (first_char ((*pp2)->c_text),
		first_char ((*pp1)->c_text)));
}


/*
 *	Sort the cards in the list into alphabetical order (actually plain
 *	ascii order).  Space is allocated for an array of all the pointers
 *	to the cards in the list.  The addresses of all the cards are then
 *	placed in the array and qsort is called to sort them in order.  The
 *	list is then made empty by simply clearing the anchor pointers to
 *	the list.  Each card is then inserted back into the list in the order
 *	they now occupy in the sorted array of pointers.  The array is then
 *	freed.
 */

void
sort_cards (descend)
	int		descend;
{
	struct	card	**a, *p;
	int		i, cards = last->c_num;

	if (cards == 1) {
		(void)msg ("Only have one active card, no need to sort");
		return;
	}

	a = (struct card **) calloc (cards,(int) sizeof (struct card *));
	for (p = first, i = 0; p != NULL_CARD; p = p->c_next, i++)
		a [i] = p;

	if (descend == TRUE) {
		qsort ((char *)a, cards, sizeof (struct card *), sort_down);
	} else {
		qsort ((char *)a, cards, sizeof (struct card *), sort_up);
	}

	first = last = NULL_CARD;
	for (i = 0; i < cards; i++) {
		(a [i])->c_num = i + 1;			/* renumber as we go */
		(void) insert_card (a [i], last);
		update_card_list(a[i]);	/* update the entries in the
						active card list */
	}

	show_card (first);				/* display the first */

	free (a);
}


/*
 *	Check the arguments which SunView didn't consume to see if a filename
 *	was provided.
 */

char *
check_args (argc, argv)
	int	argc;
	char	**argv;
{
	/* only know about a filename argument */
	if (argc < 2) {
		return (NULL);
	}

	argv++;	argc--;
	if (**argv == '-') {		/* ignore a switch if given */
		argv++;	argc--;
	}

	if (argc == 0) {
		return (NULL);
	}

	return (*argv);
}

