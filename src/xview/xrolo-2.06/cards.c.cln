#ifndef lint
static char sccsid[] = "@(#)cards.c	2.2 8/14/88";
#endif

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
#include <xview/xview.h>
#include <sys/file.h>
#include <ctype.h>
#if !defined(sgi)
#include <alloca.h>
#endif
#include <pwd.h>
#include "defs.h"


/* --------------------------- Exports ------------------------------------- */

int			need_save = FALSE;

struct card		*first = NULL_CARD, *last, *current;

struct card		*make_card (), *insert_card (), *undelete_card (),
			*pop_card ();

void			dispose_card (), push_card (), nuke_active_cards ();

char			*first_char (), *check_args ();

caddr_t			undel_menu_card ();

char                    *exp_fname();

/* --------------------------- Imports ------------------------------------- */

extern char		*rolofile;

extern void		show_card (), set_slider_max ();

extern char		*malloc(), *realloc (), *calloc (), *getenv();
#if !defined(sgi)
extern char 		*strcpy(), *strcat(), *strncpy (), *index ();
#endif
extern char		*sys_errlist [];

extern int		errno;


/* --------------------------- Locals -------------------------------------- */

static struct card	*dead;

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

char *
get_home_dir()
{
   char    *home_dir = NULL; 
   struct passwd* pw;

   if ((home_dir = getenv("HOME")) == NULL) {
      /* $HOME is not defined */
      if ((pw = getpwuid(getuid())) == NULL) {
         msg( "Cannot get passwd entry");
         return "";
      }
      home_dir = pw->pw_dir;
   }
   return home_dir;
}
   
char *
exp_fname(filename)
   char *filename;
{
   static char exp_filename[BUFSIZ];
   struct passwd* pw;
   char    username[BUFSIZ];
   char    *user_dir = NULL;
   char    filebuf[BUFSIZ];
   char    *ptr, *uptr;
   
   if ((user_dir = get_home_dir()) == NULL) return "";
   if (filename[0] == '~') {
      /* expand tilde */
      if (filename[1] == '\0') {
         msg("Cannot write to directory");
         return "";
      }
      if (filename[1] == '/') { 
         ptr = &filename[1];
      }
      else {
         for (ptr = &filename[1],uptr = username;
              *ptr != '\0' && *ptr != '/' &&
              uptr < &username[BUFSIZ - 1];
              *uptr++ = *ptr++);
         *uptr = '\0';
         if (*ptr != '/') {
            if (*ptr == '\0') msg("Cannot write to directory");
            else msg("Username too long");
            return "";
         }
         if ((pw = getpwnam(username)) == NULL) {
            msg("Cannot find user %s", username);
            return "";
         }
         user_dir = pw->pw_dir;
      }
      strcpy(exp_filename, user_dir);
      return strcat(exp_filename, ptr);
   }
   else {
      if (filename[0] != '.' && filename[0] != '/') {
         strcat(strcpy(exp_filename,user_dir),"/");
         return strcat(exp_filename,filename);
      }
      return filename;
   }
}

void
init_rolo (filename)
	char	*filename;
{
	char	*c;
	int	fd;
	FILE	*f;
        char    *home_dir;
	char    filebuf[BUFSIZ];
	char    *fname;
                    
        if (filename[0] == '\0') goto skipload;
        if (filename[0] != '.' && filename[0] != '/') {
	   if ((filename = exp_fname(strcat(strcpy(filebuf,"~/"),filename)))
		== NULL)
              goto skipload;
        }           
	if ((f = fopen (filename, "r")) == NULL) {
		if ((fd = open (filename, O_CREAT, NEWMODE)) < 0) {
			msg ("init_rolo: can't create %s: %s", filename,
				sys_errlist [errno]);
			goto skipload;
		}
		(void) close (fd);
		if ((f = fopen (filename, "r")) == NULL) {
			msg ("init_rolo: can't reopen new %s\n",
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

	eof = FALSE;
	first = last = current = dead = NULL_CARD;
	while ((c = get_card (f)) != NULL) {
		(void) insert_card (make_card (c), last);
	}
	set_slider_max (renumber (first));
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

	p = (struct card *) malloc (sizeof (struct card));
	if (p == NULL_CARD) {
		msg ("Can't allocate memory for new card");
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
		return;			/* don't keep empty cards */
	}

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
		msg ("Can't open %s for save: %s", fn,
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
			msg ("write_rolo: can't create %s: %s", filename,
				sys_errlist [errno]);
			return;
		}
		(void) close (fd);
		goto do_dump;
	}

	if (access (filename, W_OK) == -1) {
		msg ("Sorry, you don't have write access to %s", filename);
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
		msg ("Sorry, %s doesn't exist.", filename);
		return;
	}

	if (access (filename, R_OK) == -1) {
		msg ("Sorry, you don't have permission to read %s", filename);
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
	msg ("Couldn't save to %s: %s", fn, sys_errlist [errno]);
}


/*
 *	Glue routine for the qsort compare routine.  Dereference the card
 *	pointers and call strcmp to compare the text of the cards, beginning
 *	with the first non-blank character in the card.
 */

static int case_insensitive_sort;
static int sort_on_field;

static void tolowerstr(s1, s2)
char *s1, *s2;
{
	while ( *s2 != '\0' ) {
		*s1 = isupper(*s2) ? tolower(*s2) : *s2;
		s1++;
		s2++;
	}
}

static int strcaseinscmp(s1,s2)
char *s1, *s2;
{
	int retval;
	char *p1, *alloca();
	char *p2;

	if ( *s2 == '\0' && *s1 == '\0') 
	  return(0);
	if (*s1 == '\0' )
	  return(-1);
	if (*s2 == '\0' )
	  return(1);
#if defined(sgi)
	p1 = malloc(strlen(s1)+1);
	p2 = malloc(strlen(s2)+1);
#else
	p1 = alloca(strlen(s1)+1);
	p2 = alloca(strlen(s2)+1);
#endif

	tolowerstr(p1, s1);
	tolowerstr(p2, s2);
	retval = strcmp(p1, p2);
#if defined(sgi)
	free(p1);
	free(p2);
#endif
	return( retval );
}

char *find_field(c)
char *c;
{
	int field_no = 0;
    char *tp = c;
	while( field_no != sort_on_field && *tp != '\0' ) {
		if ( *tp++ == '\n' ) field_no++;
	}
	return( first_char(tp) );
}

static int sort_up (pp1, pp2)
	struct card	**pp1, **pp2;
{
	char *p1 = (*pp1)->c_text;
	char *p2 = (*pp2)->c_text;
	char *c1 = find_field (p1);
	char *c2 = find_field(p2);
	if ( case_insensitive_sort )
	  return (strcaseinscmp (c1, c2) ); 
	else
	  return (strcmp ( c1, c2)); 

}


/*
 *	Same as above, but for sort descending.  Strcmp is called with the
 *	string pointers in the opposite order to invert the sense of the
 *	comparison.
 */

static int sort_down (pp1, pp2)
	struct card	**pp1, **pp2;
{
	char *p1 = (*pp1)->c_text;
	char *p2 = (*pp2)->c_text;
	char *c1 = find_field (p1);
	char *c2 = find_field(p2);
	if ( case_insensitive_sort )
	  return (strcaseinscmp (c2,c1));
	else
	  return (strcmp (c2,c1));

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
sort_cards (descend, file_menu, field)
int		descend;
Menu_item file_menu;
int field;
{
	struct	card	**a, *p;
	int		i, cards = last->c_num;

	if (cards == 1) {
		msg ("Only have one active card, no need to sort");
		return;
	}

	sort_on_field = field;
	case_insensitive_sort = xv_get(file_menu, MENU_CLIENT_DATA);
	a = (struct card **) calloc (cards, sizeof (struct card *));
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
	}

	show_card (first);				/* display the first */

	free (a);
}


/*
 *	Menu item gen proc for the undelete pullright item.  Simply
 *	checks to see if the delete stack is empty.  If so, the item
 *	is disabled, else it is enabled.
 *	Due to the exceedingly brain-damaged way that notification is
 *	done for pullright menus, it is necesssary to do some rather
 *	silly tap-dancing to make everything work.  One such kludge is
 *	to use the client data slot in the base menu on the delete menu
 *	as a temporary storage area for the selected item from the pullright.
 *	So, when this function is called with MENU_DISPLAY, we clear the
 *	client data slot of our parent, since we are beginning a new menu
 *	cycle.  Any snide remarks about the convolution of the following
 *	three menu functions will be cheerfully ignored until and unless
 *	you can show me some better code that implements the same
 *	functionality and gets all the combinations right.  It's much
 *	trickier that it seems on the surface.  Sun really blew it on
 *	this one, it's basically impossible to properly manage dynamically
 *	generated menus with the notification scheme as it is.
 */

Menu_item
check_stack (menu_item, operation)
	Menu_item	menu_item;
	Menu_generate	operation;
{
	if (operation == MENU_DISPLAY) {
		if (dead == NULL_CARD) {
			xv_set (menu_item, MENU_INACTIVE, TRUE, 0);
		} else {
			xv_set (menu_item, MENU_INACTIVE, FALSE, 0);
		}
	}

	return (menu_item);
}

void undel_before_proc(menu, menu_item)
Menu menu;
Menu menu_item;
{
	undel_menu_card(menu_item, 1);
}

void undel_proc(menu, menu_item)
Menu menu;
Menu_item menu_item;
{
	undel_menu_card(menu_item, 0);
}

Menu gen_undelete_menu(proc)
void *proc;
{
	/*
	 * The user has just pulled right on the item, so now we need
	 * to build the menu which will be displayed to the right.
	 * First we create a menu.  Then we walk the deleted list
	 * and make a menu item for each card on the stack and append
	 * it to the new menu.
	 */
	Menu_item menu_item;
	Menu menu = xv_create(NULL, MENU, 
						  MENU_NOTIFY_PROC, proc,
						  NULL);
	struct card	*p;
	
	for (p = dead; p != NULL_CARD; p = p->c_next) {
		char	*q, buf [MAX_MENU_LEN + 1];
		
		q = first_char (p->c_text);
		if (strlen (q) == 0) {
			/* unlikely, push_card filters empty cards */
			continue;
		}

		/* yank and chop text from the card */
		(void) strncpy (buf, q, MAX_MENU_LEN);
		buf [MAX_MENU_LEN] = '\0';
		q = index (buf, '\n');
		if (q != NULL) {
			*q = '\0';
		}

		/* allocate and copy the string for use by the menu */
		q = malloc (strlen (buf) + 1);
		(void) strcpy (q, buf);

		menu_item = xv_create (NULL, MENUITEM,
							   MENU_STRING,		q,
							   MENU_VALUE,		p, 
							   /* menu_destroy() will free() name string */
							   MENU_RELEASE_IMAGE, 
							   /* goes away when parent menu does */
							   MENU_RELEASE,
							   0);

		xv_set (menu, MENU_APPEND_ITEM, menu_item, 0);
	}
	return(menu);
}


Menu
gen_undelete (menu_item, operation)
	Menu_item	menu_item;
	Menu_generate	operation;
{
	static Menu menu;
	switch (operation) {
	  case MENU_DISPLAY:
		return (gen_undelete_menu(undel_proc));

	  case MENU_DISPLAY_DONE:
		xv_destroy_safe(menu);
		return (MENU_NULL);
	}
	return (MENU_NULL);
}

Menu
gen_undelete_before (menu_item, operation)
	Menu_item	menu_item;
	Menu_generate	operation;
{
	static Menu menu; 
	switch (operation) {
	  case MENU_DISPLAY:
		return (menu=gen_undelete_menu(undel_before_proc));

	  case MENU_DISPLAY_DONE:
		xv_destroy_safe(menu);
		return (MENU_NULL);
	}
	return(MENU_NULL);
}


/*
 *	Action proc for the undelete pullright menu item.  This
 *	proc removes the card from the stack and inserts it back into the
 *	active list.  The pointer to the card selected from the menu is
 *	the value of the menu item we're being called with.  The menu
 *	that this action proc is called from, the args passed to the proc,
 *	is the dummy menu created in the MENU_NOTIFY step of the gen
 *	proc above.  Since this proc actually does the undelete, we want
 *	to clear the menu value so that the event func which trapped the
 *	right button event and brought the menu up in the first place
 *	doesn't fake a button click after we've finished.
 */

/*ARGSUSED*/
caddr_t
undel_menu_card (menu_item, before)
	Menu_item		menu_item;
	int before;
{
	struct card	*p;
	save_card (current);

	p = (struct card *) xv_get (menu_item, MENU_VALUE);

	if (p == NULL_CARD) {
		p = dead;
	}
	unstack_card (p);
	(void) insert_card (p, (before == TRUE) ? current->c_prev : current);
	need_save = TRUE;
	set_slider_max (renumber (first));
	show_card (p);
	return ((caddr_t)0);
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
