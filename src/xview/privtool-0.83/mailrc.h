

/*
 * @(#)mailrc.h	1.3 6/29/94
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

typedef struct _entry {

	struct	_entry	*next;
	struct	_entry	*prev;

	char	*name;
	char	*value;

	int32	flags;

} MAILRC;

#define	MAILRC_PREFIXED	0x0001
#define MAILRC_OURPREF	0x0002

extern	MAILRC	*new_mailrc();
extern	void	free_mailrc();
extern	char	*find_pgpkey();
extern	char	*find_alias();
extern	char	*find_mailrc();

typedef struct {

	MAILRC	*start;
	MAILRC	*end;

} LIST;

