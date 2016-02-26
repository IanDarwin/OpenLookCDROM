
/*
 * @(#)buffers.c	1.8 9/10/94
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

#include <malloc.h>

#include "def.h"
#include "buffers.h"

void	add_to_buffer(buffer,mess,len)

BUFFER	*buffer;
byte	*mess;
int	len;

{
	if (!buffer->size) {
		if (buffer->message = (byte *)malloc(QUANTA))
			buffer->size = QUANTA;
	}

	if (buffer->length + len >= buffer->size) {
		buffer->size = (buffer->size + len + QUANTA + 1) / QUANTA;
		buffer->size *= QUANTA;
		buffer->message = (byte *)realloc(buffer->message, 
			buffer->size);
	}

	memcpy(buffer->message + buffer->length, mess, len);
	buffer->length += len;

	buffer->message[buffer->length] = 0;
}

BUFFER	*new_buffer()

{
	BUFFER	*b;

	b = (BUFFER *)malloc(sizeof(BUFFER));

	b->message = 0;
	b->size = 0;
	b->length = 0;

	return b;
}

free_buffer(b)

BUFFER	*b;

{
	if (b->message && b->size) {
		bzero (b->message, b->size);
		free (b->message);
	}

	free(b);
}

clear_buffer(b)

BUFFER	*b;

{
	if (b->message && b->size)
		bzero (b->message, b->size);
	b->length = 0;
}

reset_buffer(b)

BUFFER	*b;

{
	if (b->message && b->size) {
		bzero (b->message, b->size);
		free (b->message);
	}

	b->size = 0;
	b->length = 0;
}

