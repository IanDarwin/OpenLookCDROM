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

#ifndef CHAMP_DEFINED
struct gregoriandatespec {
    int year, month, day, hour, min, wkday, wkdayselector;
};

struct hebrewdatespec {
    int year, month, day;
};

struct ecclesiasticaldatespec {
    int year, landmark, offset, hour, min;
};

#define CALSYS_GREGORIAN 0
#define CALSYS_HEBREW 1
#define CALSYS_ECCLESIASTICAL 2

struct datespec {
    int calsys; /* one of CALSYS constants above */
    union {
	struct gregoriandatespec gd;
	struct hebrewdatespec hd;
	struct ecclesiasticaldatespec ed;
    } sys;
};

struct eventnode {
    struct datespec ds;
    char *event;
    int flagged;
    struct eventnode *next;
};

extern struct eventnode *RootEventNode;

#define CHAMPERR_NOERR 0
#define CHAMPERR_BADFORMAT 1
#define CHAMPERR_BADCALSYS 2
#define CHAMPERR_UNIMPLEMENTED 3
#define CHAMPERR_NOMATCH 4
#define CHAMP_DEFINED
#endif /* CHAMP_DEFINED */
