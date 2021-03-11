/* user code begins here for HeaderInfo */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 
/* user code ends here for HeaderInfo */
class helpcon { 
classprocedures :
        InitializeClass() returns boolean;
/* user code begins here for classprocedures */
/* user code ends here for classprocedures */
data:
	struct text *topics;
	struct cltextview *topicsView;
	struct text *choice;
	struct cltextview *choiceView;
	struct value *topicschoice;
	struct stringV *topicschoiceView;
	struct text *body;
	struct textview *bodyView;
	struct value *choicelabel;
	struct stringV *choicelabelView;
/* user code begins here for classdata */
	struct text *historytext;
	int ShowHistory;
	char CurrentName[256];
	char CurrentType[256];
	struct dirent **dl;
	long count;
/* user code ends here for classdata */
	struct view *v;
	struct arbiterview *arbv;
	struct helpcon *next;
};

