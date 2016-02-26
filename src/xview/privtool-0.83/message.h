
/*
 *	@(#)message.h	1.21 12/16/94
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

typedef struct _message {

	/* Linked list of all messages */

	struct	_message	*next;
	struct	_message	*prev;

	/* Linked list of deleted messages */

	struct	_message	*dnext;
	struct	_message	*dprev;

	/* Contents of message */

	BUFFER	*body;
	BUFFER	*header;
	BUFFER	*signature;
	BUFFER	*decrypted;

	/* To support file access to messages */

	long	offset;
	int	data_type;

	/* Info read from header */

	char	*email;
	char	*sender;
	char	*subject;
	char	*date;
	char	*description;
	char	*message_id;
	char	*header_date;

	/* Flags */

	word16	flags;
	word16	status;

	/* Misc information */

	int32	lines;
	int32	size;
	int32	number;
	int32	list_pos;
	int32	time_secs;

	word32	unique_id;

} MESSAGE;

extern	MESSAGE	*new_message();

#define MESS_SIGNED		0x0001
#define MESS_ENCRYPTED		0x0002
#define MESS_VERIFIED		0x0004
#define MESS_BAD		0x0008
#define MESS_SELECTED		0x0010
#define MESS_DIGEST		0x0020
#define MESS_CFEED		0x0040
#define MESS_DELETED		0x0080

#define DT_NONE		0x0000
#define DT_MEM		0x0001
#define DT_FILE		0x0002

#define MSTAT_NONE	0
#define MSTAT_UNREAD	1
#define MSTAT_READ	2

typedef struct {

	MESSAGE	*start;
	MESSAGE	*end;
	int	number;
	int	new;
	int	encrypted;
	int	unread;

} MESSAGE_LIST;

extern	MESSAGE	*message_from_message();
extern	BUFFER	*message_contents();
