/* opts.c */

/* Author:
 *	Steve Kirkendall
 *	16820 SW Tallac Way
 *	Beaverton, OR 97006
 *	kirkenda@jove.cs.pdx.edu, or ...uunet!tektronix!psueea!jove!kirkenda
 */


/* This file contains the code that manages the run-time options -- The 
 * values that can be modified via the "set" command.
 */

#include "config.h"
#include "vi.h"
#ifndef NULL
#define NULL (char *)0
#endif
extern char	*getenv();

/* These are the default values of all options */
char	o_autoindent[1] =	{FALSE};
char	o_autowrite[1] = 	{FALSE};
#ifndef NO_CHARATTR
char	o_charattr[1] =		{FALSE};
#endif
char	o_columns[3] =		{80, 32, 255};
char	o_directory[30] =	TMPDIR;
char	o_errorbells[1] =	{TRUE};
char	o_exrefresh[1] =	{TRUE};
#ifndef NO_SENTENCE
char	o_hideformat[1] =	{FALSE};
#endif
char	o_ignorecase[1] =	{FALSE};
#ifndef NO_EXTENSIONS
char	o_inputmode[1] =	{FALSE};
#endif
char	o_keytime[3] =		{2, 0, 5};
char	o_keywordprg[80] =	KEYWORDPRG;
char	o_lines[3] =		{25, 2, 50};	/* More lines? Enlarge kbuf */
char	o_list[1] =		{FALSE};
#ifndef NO_MAGIC
char	o_magic[1] =		{TRUE};
#endif
#ifndef NO_SENTENCE
char	o_paragraphs[30] =	"PPppPApa";
#endif
#if MSDOS
char	o_pcbios[1] =		{TRUE};
#endif
char	o_readonly[1] =		{FALSE};
char	o_redraw[1] =		{TRUE};
char	o_report[3] =		{5, 1, 127};
char	o_scroll[3] =		{12, 1, 127};
#ifndef NO_SENTENCE
char	o_sections[30] =	"SEseSHsh";
#endif
char	o_shell[60] =		"/bin/sh";
char	o_shiftwidth[3] =	{8, 1, 255};
#ifndef	NO_SHOWMODE
char	o_showmode[1] =		{FALSE};
#endif
char	o_sidescroll[3] =	{8, 1, 40};
char	o_sync[1] =		{FALSE};
char	o_tabstop[3] =		{8, 1, 40};
char	o_term[30] =		"?";
char	o_vbell[1] =		{TRUE};
char	o_warn[1] =		{TRUE};
char	o_wrapmargin[3] =	{0, 0, 255};
char	o_wrapscan[1] =		{TRUE};


/* The following describes the names & types of all options */
#define BOOL	0
#define	NUM	1
#define	STR	2
#define SET	0x01	/* this option has had its value altered */
#define CANSET	0x02	/* this option can be set at any time */
#define RCSET	0x06	/* this option can be set in a .exrc file only */
#define MR	0x40	/* does this option affect the way text is displayed? */
struct
{
	char	*name;	/* name of an option */
	char	*nm;	/* short name of an option */
	char	type;	/* type of an option */
	char	flags;	/* boolean: has this option been set? */
	char	*value;	/* value */
}
	opts[] =
{
	/* name			type	flags	redraw	value */
	{ "autoindent",	"ai",	BOOL,	CANSET	,	o_autoindent	},
	{ "autowrite",	"aw",	BOOL,	CANSET	,	o_autowrite	},
#ifndef NO_CHARATTR
	{ "charattr",	"ca",	BOOL,	CANSET	| MR,	o_charattr	},
#endif
	{ "columns",	"co",	NUM,	SET	,	o_columns	},
	{ "directory",	"dir",	STR,	RCSET	,	o_directory	},
	{ "errorbells",	"eb",	BOOL,	CANSET	,	o_errorbells	},
	{ "exrefresh",	"er",	BOOL,	CANSET	,	o_exrefresh	},
#ifndef NO_SENTENCE
	{ "hideformat",	"hf",	BOOL,	CANSET	| MR,	o_hideformat	},
#endif
	{ "ignorecase",	"ic",	BOOL,	CANSET	,	o_ignorecase	},
#ifndef NO_EXTENSIONS
	{ "inputmode",	"im",	BOOL,	CANSET	,	o_inputmode	},
#endif
	{ "keytime",	"kt",	NUM,	CANSET	,	o_keytime	},
	{ "keywordprg",	"kp",	STR,	CANSET	,	o_keywordprg	},
	{ "lines",	"ls",	NUM,	SET	,	o_lines		},
	{ "list",	"li",	BOOL,	CANSET	| MR,	o_list		},
#ifndef NO_MAGIC
	{ "magic",	"ma",	BOOL,	CANSET	,	o_magic		},
#endif
#ifndef NO_SENTENCE
	{ "paragraphs",	"pa",	STR,	CANSET	,	o_paragraphs	},
#endif
#if	MSDOS
	{ "pcbios",	"pc",	BOOL,	SET	,	o_pcbios	},
#endif
	{ "readonly",	"ro",	BOOL,	CANSET	,	o_readonly	},
	{ "redraw",	"rd",	BOOL,	CANSET	,	o_redraw	},
	{ "report",	"re",	NUM,	CANSET	,	o_report	},
	{ "scroll",	"sc",	NUM,	CANSET	,	o_scroll	},
#ifndef NO_SENTENCE
	{ "sections",	"se",	STR,	CANSET	,	o_sections	},
#endif
	{ "shell",	"sh",	STR,	CANSET	,	o_shell		},
#ifndef	NO_SHOWMODE
	{ "showmode",	"sho",	BOOL,	CANSET	,	o_showmode	},
#endif
	{ "shiftwidth",	"sw",	NUM,	CANSET	,	o_shiftwidth	},
	{ "sidescroll",	"ss",	NUM,	CANSET	,	o_sidescroll	},
	{ "sync",	"sy",	BOOL,	CANSET	,	o_sync		},
	{ "tabstop",	"ts",	NUM,	CANSET	| MR,	o_tabstop	},
	{ "term",	"te",	STR,	SET	,	o_term		},
	{ "vbell",	"vb",	BOOL,	CANSET	,	o_vbell		},
	{ "warn",	"wa",	BOOL,	CANSET	,	o_warn		},
	{ "wrapmargin",	"wm",	NUM,	CANSET	,	o_wrapmargin	},
	{ "wrapscan",	"ws",	BOOL,	CANSET	,	o_wrapscan	},
	{ NULL, NULL, 0, CANSET, NULL }
};


/* This function initializes certain options from environment variables, etc. */
initopts()
{
	char	*val;
	int	i;

	/* set some stuff from environment variables */
#if	ANY_UNIX || TOS
	if (val = getenv("SHELL")) /* yes, ASSIGNMENT! */
	{
		strcpy(o_shell, val);
	}
	if (val = getenv("TERM")) /* yes, ASSIGNMENT! */
	{
		strcpy(o_term, val);
	}
#endif
#if	MSDOS
	if (val = getenv("COMSPEC")) /* yes, ASSIGNMENT! */
	{
		strcpy(o_shell, val);
	}
	if ((val = getenv("TERM")) /* yes, ASSIGNMENT! */
		&& strcmp(val, "pcbios"))
	{
		strcpy(o_term, val);
		o_pcbios[0] = 0;
	}
	else
	{
		strcpy(o_term, "pcbios");
		o_pcbios[0] = 1;
	}
#endif
#if	MSDOS || TOS
	if ((val = getenv("TMP")) /* yes, ASSIGNMENT! */
	||  (val = getenv("TEMP")))
		strcpy(o_directory, val);
#endif

	*o_scroll = LINES / 2 - 1;

	/* disable the vbell option if we don't know how to do a vbell */
	if (!has_VB)
	{
		for (i = 0; opts[i].value != o_vbell; i++)
		{
		}
		opts[i].flags &= ~CANSET;
		*o_vbell = FALSE;
	}
}

/* This function lists the current values of all options */
dumpopts(all)
	int	all;	/* boolean: dump all options, or just set ones? */
{
	int	i;
	int	col;
	char	nbuf[4];

	for (i = col = 0; opts[i].name; i++)
	{
		/* if not set and not all, ignore this option */
		if (!all && !(opts[i].flags & SET))
		{
			continue;
		}

		/* align this option in one of the columns */
		if (col > 52)
		{
			addch('\n');
			col = 0;
		}
		else if (col > 26)
		{
			while (col < 52)
			{
				qaddch(' ');
				col++;
			}
		}
		else if (col > 0)
		{
			while (col < 26)
			{
				qaddch(' ');
				col++;
			}
		}

		switch (opts[i].type)
		{
		  case BOOL:
			if (!*opts[i].value)
			{
				qaddch('n');
				qaddch('o');
				col += 2;
			}
			qaddstr(opts[i].name);
			col += strlen(opts[i].name);
			break;

		  case NUM:
			sprintf(nbuf, "%-3d", UCHAR(*opts[i].value));
			qaddstr(opts[i].name);
			qaddch('=');
			qaddstr(nbuf);
			col += 4 + strlen(opts[i].name);
			break;

		  case STR:
			qaddstr(opts[i].name);
			qaddch('=');
			qaddch('"');
			qaddstr(opts[i].value);
			qaddch('"');
			col += 3 + strlen(opts[i].name) + strlen(opts[i].value);
			break;
		}
		exrefresh();
	}
	if (col > 0)
	{
		addch('\n');
		exrefresh();
	}
}

/* This function saves the current configuarion of options to a file */
saveopts(fd)
	int	fd;	/* file descriptor to write to */
{
	int	i;
	char	buf[256], *pos;

	/* write each set options */
	for (i = 0; opts[i].name; i++)
	{
		/* if unset or unsettable, ignore this option */
		if (!(opts[i].flags & SET) || !(opts[i].flags & CANSET))
		{
			continue;
		}

		strcpy(buf, "set ");
		pos = &buf[4];
		switch (opts[i].type)
		{
		  case BOOL:
			if (!*opts[i].value)
			{
				*pos++='n';
				*pos++='o';
			}
			strcpy(pos, opts[i].name);
			strcat(pos, "\n");
			break;

		  case NUM:
			sprintf(pos, "%s=%-3d\n", opts[i].name, *opts[i].value & 0xff);
			break;

		  case STR:
			sprintf(pos, "%s=\"%s\"\n", opts[i].name, opts[i].value);
			break;
		}
		twrite(fd, buf, strlen(buf));
	}
}


/* This function changes the values of one or more options. */
setopts(assignments)
	char	*assignments;	/* a string containing option assignments */
{
	char	*name;		/* name of variable in assignments */
	char	*value;		/* value of the variable */
	char	*scan;		/* used for moving through strings */
	int	i, j;

	/* for each assignment... */
	for (name = assignments; *name; )
	{
		/* skip whitespace */
		if (*name == ' ' || *name == '\t')
		{
			name++;
			continue;
		}

		/* find the value, if any */
		for (scan = name; *scan >= 'a' && *scan <= 'z'; scan++)
		{
		}
		if (*scan == '=')
		{
			*scan++ = '\0';
			if (*scan == '"')
			{
				value = ++scan;
				while (*scan && *scan != '"')
				{
					scan++;
				}
				if (*scan)
				{
					*scan++ = '\0';
				}
			}
			else
			{
				value = scan;
				while (*scan && *scan != ' ' && *scan != '\t')
				{
					scan++;
				}
				if (*scan)
				{
					*scan++ = '\0';
				}
			}
		}
		else
		{
			if (*scan)
			{
				*scan++ = '\0';
			}
			value = NULL;
			if (name[0] == 'n' && name[1] == 'o')
			{
				name += 2;
			}
		}

		/* find the variable */
		for (i = 0;
		     opts[i].name && strcmp(opts[i].name, name) && strcmp(opts[i].nm, name);
		     i++)
		{
		}

		/* change the variable */
		if (!opts[i].name)
		{
			msg("invalid option name \"%s\"", name);
		}
		else if ((opts[i].flags & CANSET) != CANSET)
		{
			msg("option \"%s\" can't be altered", name);
		}
		else if ((opts[i].flags & RCSET) != CANSET && nlines >= 1L)
		{
			msg("option \"%s\" can only be set in a %s file", name, EXRC);
		}
		else if (value)
		{
			switch (opts[i].type)
			{
			  case BOOL:
				msg("option \"[no]%s\" is boolean", name);
				break;

			  case NUM:
				j = atoi(value);
				if (j == 0 && *value != '0')
				{
					msg("option \"%s\" must have a numeric value", name);
				}
				else if (j < opts[i].value[1] || j > (opts[i].value[2] & 0xff))
				{
					msg("option \"%s\" must have a value between %d and %d",
						name, opts[i].value[1], opts[i].value[2] & 0xff);
				}
				else
				{
					*opts[i].value = atoi(value);
					opts[i].flags |= SET;
				}
				break;

			  case STR:
				strcpy(opts[i].value, value);
				opts[i].flags |= SET;
				break;
			}
			if (opts[i].flags & MR)
			{
				mustredraw = TRUE;
			}
		}
		else /* valid option, no value */
		{
			if (opts[i].type == BOOL)
			{
				*opts[i].value = (name[-1] != 'o');
				opts[i].flags |= SET;
				if (opts[i].flags & MR)
				{
					mustredraw = TRUE;
				}
			}
			else
			{
				msg("option \"%s\" must be given a value", name);
			}
		}

		/* move on to the next option */
		name = scan;
	}
}
