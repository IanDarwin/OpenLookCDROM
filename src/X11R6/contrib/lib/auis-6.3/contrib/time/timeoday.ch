/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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

#include <fontdesc.ih>
#include <event.ih>

class timeoday: dataobject[dataobj] {
    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct timeoday *self) returns boolean;
      FinalizeObject(struct timeoday *self);
    overrides:
      Read (FILE *fp, long id) returns long;
      Write (FILE *fp, long id, int level) returns long;
    methods:
      SetFormat(char *format);
      SetFont(struct fontdesc *f);
      UpdateTime();
      FormatTime();
      WriteDataPart(FILE *fp);
      ReadDataPart(FILE *fp) returns long;
      InitializeDefaults() returns boolean;
    macromethods:
      GetTod() (self->tod)
      GetFont() (self->myfontdesc)
      GetTime() (self->now)
      GetFormat() (self->format)
      GetEvent() (self->ev)
      SetTime(thetime) (self->now = (thetime))
    data:
      char *tod, *format;
      long now;
      long epoch;
      struct event *ev;
      struct fontdesc *myfontdesc;
};

