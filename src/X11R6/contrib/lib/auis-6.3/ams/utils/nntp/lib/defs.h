/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

/*
 * defs.h - defines for news-related programs.
 *
 * If you remove any lines here or in your Makefile, make the change
 * to localize.sh so you won't have to redo it for each news release.
 *
 * If TMAIL is undefined, the -M option will be disabled.
 *
 * By convention, the version of the software you are running is taken
 * to be news_version below.
 */

/*	@(#)defs.dist	2.47	6/6/85	*/

#define NEWS_VERSION   "B 2.10.3 4.3bsd-beta 6/6/85"

#define DAYS	(60L*60L*24L)
#define WEEKS	(7*DAYS)
/* Things that very well may require local configuration */
#ifndef HOME
#define	ROOTID	1633	/* uid of person allowed to cancel anything	*/
#endif /* HOME */
#define	N_UMASK 022	/* mask for umask call, 022 for secure system	*/
#define DFLTEXP	2*WEEKS	/* default no. of seconds to expire in		*/
#define HISTEXP	4*WEEKS	/* default no. of seconds to forget in		*/
#define DFLTSUB "general,all.general,all.announce"	/* default subscription list	*/
#define TMAIL	"/usr/ucb/Mail"	/* Mail program that understands -T	*/
#define	ADMSUB	""	/* Mandatory subscription list	*/
#define PAGE	"/bin/cat"	/* Default pager		*/
#define NOTIFY	"netnews"	/* Tell him about certain ctl messages	*/
				/* Default xmit command - remove -z if	*/
#define DFTXMIT	"uux - -r -z %s!rnews < %s" /* your uux can't do it	*/
/* #define UXMIT	"uux -r -z -c %s!rnews '<' %s" /* If uux -c is ok	*/
#define DFTEDITOR "emacs"	/* Default editor, see also postnews.*/
#define UUPROG "euuname"	/* omit for uuname, put in LIBDIR	*/
#define MANUALLY		/* Don't execute rmgroups, just notify.	*/
/* #define NONEWGROUPS		/* Don't create new groups, just notify.*/
#define BATCH "/usr/netnews/lib/news2.10/unbatch"		/* name of unbatcher 		*/
/* #define LOCALNAME 		/* There is no full name database. 	*/
#define INTERNET		/* Internet mail works locally		*/
#define MYDOMAIN ""		/* Local domain				*/
/* #define CHEAP		/* don't chown files to news		*/
#define OLD			/* Add extra headers for old neighbors	*/
/* #define UNAME		/* If uname call returns your nodename  */
/* #define GHNAME		/* If gethostname call is available.	*/
#define V7MAIL			/* Local mail format is V7 ("From ")	*/
#define SORTACTIVE		/* if you want news presented in the order of the .newsrc */
#define ZAPNOTES		/* if you want old style notes headers moved into the headers */
#define DIGPAGE			/* allow digestifying in vnews */
/* #define DOXREFS		/* Generate xref line for rn to use */
/* #define MULTICAST		/* If you want to be able to multicast news */
#define BSD4_2		/* If you are running 4.2 BSD		*/
/* #define BSD4_1C		/* If you are running 4.1C BSD		*/
/* #define SENDMAIL "/usr/lib/sendmail" /* command line to run "sendmail" if you have it	*/
/* #define MMDF	"/usr/mmdf/submit"	/* command line to run mmdf if you have it */
#define MYORG "Carnegie-Mellon University"		/* My organization.  Please	*/
				/* include your city (and state, and	*/
				/* country, if not obvious) in MYORG,	*/
				/* and please keep it short.		*/
/* #define HIDDENNET "frooz"	/* if you have a local network and want */
				/* The mail address to look like it came */
				/* from one machine */
/* #define NICENESS	4	/* does a nice(NICENESS) in rnews */
/* #define FASCIST	"all,!all.all"	/* only permit posting to certain groups */
				/* see installation guide for details */
/* #define SMALL_ADDRESS_SPACE	/* If your machine can't address > 32767 */

/* Things you might want to change */
#define NEWSRC  ".newsrc"	/* name of .newsrc file (in home dir)	*/
#define LINES	512	/* maximum no. of lines in .newsrc		*/
#define NEGCHAR	'!'	/* newsgroup negation character			*/
#define DEADTIME 45	/* no. of seconds to wait on deadlock		*/
#define FMETA	'%'	/* file meta-character for c option		*/
#if defined(pdp11) || defined(SMALL_ADDRESS_SPACE)
#define BUFLEN	128	/* standard buffer size				*/
#else /* defined(pdp11) || defined(SMALL_ADDRESS_SPACE) */
#define BUFLEN	256	/* standard buffer size				*/
#endif /* defined(pdp11) || defined(SMALL_ADDRESS_SPACE) */
#define LBUFLEN 1024	/* big buffer size				*/
#define	SBUFLEN 32	/* small buffer size (for system names, etc)	*/
#define SYSPATH	"PATH=/bin:/usr/bin:/usr/netnews/bin"	/* default, secure, vanilla path */
#define LNCNT	14	/* Articles with > LNCNT lines go through pager */

/* Things you probably won't want to change */
#define PATHLEN 512	/* length of longest source string		*/
#define	DATELEN	64	/* length of longest allowed date string	*/
#define	NAMELEN	64	/* length of longest possible message ID	*/
#define	SNLN	8	/* max significant characters in sysname	*/
#define	PROTO	'A'	/* old protocol name				*/
#define NETCHRS	"!:.@^%"/* Punct. chars used for various networks	*/
#define	TRUE	1	/* boolean true					*/
#define	FALSE	0	/* boolean false				*/
#define	PERHAPS	2	/* indeterminate boolean value			*/
#define AFSIZ  5000	/* legal newsgroup file size			*/
#define	NGDELIM	','	/* delimit character in news group line		*/
