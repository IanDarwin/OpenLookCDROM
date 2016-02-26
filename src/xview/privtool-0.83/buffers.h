
/*
 * @(#)buffers.h	1.6 6/29/94
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

typedef struct {

	byte	*message;
	int32	length;
	int32	size;

} BUFFER;

/* We allocate space in units of QUANTA bytes to try to avoid too much
   heap fragmentation */

#define QUANTA	128

extern	BUFFER	*new_buffer();

