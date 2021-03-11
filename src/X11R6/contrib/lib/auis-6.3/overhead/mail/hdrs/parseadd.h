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
		Include file for address parsing.
*/

/* Error returns from parsing routines */
#define PA_OK		0
#define PA_SYNTAX_ERROR	-1
#define PA_NULL_POINTER	-2
#define PA_UNK_ADDRESS	-3
#define PA_BAD_LEN	-4
#define PA_BAD_INDEX	-5
#define PA_PARTIAL	-6
#define PA_TOO_LONG	-7
#define PA_NO_MEM	-8

/* Unparsing modes */
#define UP_NO_COMMENTS		1
#define UP_SPACES_TO_DOTS	2

typedef enum { SIMPLE_ADDRESS, GROUP_ADDRESS, DUMMY_ADDRESS } ADDRESS_KIND;

typedef struct _HOST_ {
    char		*Name;		/* Host name */
    struct _HOST_	*Next;		/* For chaining together in ADDRESS */
    struct _HOST_	*Prev;
} ADDRESS_HOST;

typedef struct _COMMENT_ {
    char		*Text;
    struct _COMMENT_	*Next;
} ADDRESS_COMMENT;

typedef struct _ADDRESS_ {
    ADDRESS_KIND	Kind;		/* Simple name, group or empty */
    char		*LocalPart;	/* Local part, or group name */
    ADDRESS_HOST	*Hosts;		/* Chain of hosts in address */
    struct _ADDRESS_	*Members;	/* Start of group members */
    char		*RoutePhrase;	/* Comments that occur before a route */
    ADDRESS_COMMENT	*Comments;	/* () Comments chained together */
    struct _ADDRESS_	*Next;		/* For use in chaining addresses */
    struct _ADDRESS_	*Prev;		/* For use in chaining addresses */
    struct MailDom		*MD;		/* Characteristics of outermost mail domain */
    struct MailDom		*Extra;		/* for involved address handlers */
} PARSED_ADDRESS;

/* Macros for walking stuff */

#define FOR_ALL_GROUP_MEMBERS(var, addr, body)\
	{\
	    register PARSED_ADDRESS *var, *_NEXT_;\
	    for (var=(addr)->Members->Next, _NEXT_=var->Next;\
		 var->Kind!=DUMMY_ADDRESS;\
		 var=_NEXT_, _NEXT_=var->Next)\
		body\
	}

#define FOR_ALL_ADDRESSES(var, list, body)\
	{\
	    register PARSED_ADDRESS *var, *_NEXT_;\
	    for (var=(list)->Next, _NEXT_=var->Next;\
		 var->Kind!=DUMMY_ADDRESS;\
		 var=_NEXT_, _NEXT_=var->Next)\
		body\
	}

#define FOR_ALL_REVERSE_HOSTS(var, addr, body)\
	{\
	    register ADDRESS_HOST *var, *_HEAD_, *_PREV_;\
	    _HEAD_ = (addr)->Hosts;\
	    for (var=_HEAD_->Prev, _PREV_=var->Prev;\
		 var!=_HEAD_;\
		 var=_PREV_, _PREV_=var->Prev)\
		body\
	}
