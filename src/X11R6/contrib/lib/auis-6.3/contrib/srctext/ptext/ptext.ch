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


 

/*
 * ptext.ch:  Text subclass specialized for dealing with Pascal Code
 */

#include <mark.ih>
#include <envrment.ih>

#define idstyle_PLAIN   0
#define idstyle_UPPER   1
#define idstyle_LOWER   2
#define idstyle_CAPIT   3

class ptext: text {
  overrides:
    SetAttributes(struct attributes *atts);
    Clear();
    Read(FILE *file, long id) returns long;
  methods:
    Indent(struct mark *range) returns long;
    ReindentLine(long pos);
    ReverseBalance(long pos) returns long;
    RedoStyles();
    TabAndOptimizeWS(long pos, int inc) returns long;
    /* Added by Rob Ryan 10/88 */
    InsertNewStyle(long pos,long len,struct style *style,
        boolean begflag, boolean endflag);
    GetEnvironment(long pos) returns struct environment *;
    GetStyle(long pos) returns struct style *;
    BackwardCheckWord(long pos,long first);
    StyleLine(long pos);
  data:
    struct style *comment_style, *keyword_style, *root_style;
};
