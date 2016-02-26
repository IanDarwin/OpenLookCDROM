#ident	"@(#)notice.h	26.12	93/06/28 SMI"

/*
 *      (c) Copyright 1989 Sun Microsystems, Inc.
 */

/*
 *      Sun design patents pending in the U.S. and foreign countries. See
 *      LEGAL_NOTICE file for terms of the license.
 */

#ifndef _OLWM_NOTICE_H
#define _OLWM_NOTICE_H

#define NOTICE_CANCEL	(-1)

#define NOTICE_BUTTON_COUNT(b)	sizeof((b))/sizeof(char *)

typedef struct _noticeBox {
	int	numButtons;	/* number of buttons */
	int	defaultButton;	/* index into buttonText array */
	Text	**buttonText;	/* array of strings for button text */
	Text	*msgText;
	int	boxX;		/* box origin (-1 =use default/centered) */
	int	boxY;		/* box origin (-1 =use default/centered) */
} NoticeBox;

/* function declarations */
extern int UseNoticeBox();

#endif /* _OLWM_NOTICE_H */
