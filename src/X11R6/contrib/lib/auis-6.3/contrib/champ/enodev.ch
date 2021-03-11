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

#define BUTT_SYS 0
#define BUTT_YEAR 1
#define BUTT_MON 2
#define BUTT_DAY 3
#define BUTT_WKDAY 4
#define BUTT_WKDAYSELECT 5
#define BUTT_HR 6
#define BUTT_MIN 7
#define BUTT_ADD 8
#define BUTT_DEL 9
#define NUMBUTTS 10 /* one plus previous */

class enodeview[enodev]:lpair {
    classprocedures:
      InitializeObject(struct enodeview *self) returns boolean;
    overrides:
      ObservedChanged (struct thisobject *changed, long value);
    methods:
      SetChimpview(struct chimpview *);
    data:
      struct butter *butts[NUMBUTTS];
      struct chimpview *mychimpview;
};
