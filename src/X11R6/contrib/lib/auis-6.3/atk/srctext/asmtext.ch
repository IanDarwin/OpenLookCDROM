/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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


/* asmtext.ch: Text subclass specialized for dealing with Assembly code. */

#include <mark.ih>
#include <envrment.ih>

#define MAX_BANGCHARS 32 /* arbitrary limit for how many chars can be bang-comment-starters */
#define MAX_TABSTOPS  80 /* arbitrary number for how many Tab Stops can be set in preferences */

class asmtext: srctext {

  overrides:
    Indent(struct mark *range) returns long;
    Keywordify(char *buff, boolean checkforceupper) returns char *;
    RedoStyles();
    SetAttributes(struct attributes *atts);

  methods:

  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct asmtext *self) returns boolean;

  macromethods:
    SetCComments(boolean newValue) (((self)->CComments) = (newValue))
    UseCComments() ((self)->CComments)
    HasReindentFilter() ((self)->reindentFilterName!=NULL && *((self)->reindentFilterName)!='\0')
    ReindentFilterName() ((self)->reindentFilterName)

  data:
    boolean CComments;
    char bangComments[MAX_BANGCHARS+1];
    char *reindentFilterName;
};

