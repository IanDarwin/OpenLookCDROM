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

/* #include <time.h> */
#include <fontdesc.ih>
#include <event.ih>

struct clock_time {
  double hours, minutes, seconds;
};

enum border_shapes {circle = 0, square = 1};

struct clock_options {
  long timestamp;
  int hours_width, minutes_width, seconds_width, border_width;
  int hours_length, minutes_length, seconds_length; /* -100 -> 100 */
  enum border_shapes border_shape;
  int major_ticks, minor_ticks;
  int tick_length;
  int num_labels;
  char **labels;
  char *fontfamily;
  int fontface;
};

class clock: dataobject[dataobj] {
    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct clock *self) returns boolean;
      FinalizeObject(struct clock *self);
    overrides:
      Read (FILE *fp, long id) returns long;
      Write (FILE *fp, long id, int level) returns long;
    methods:
      SetOptions(struct clock_options *options);
    macromethods:
      ReadClock() (&(self->clockface))
      GetOptions() (&(self->options));
    data:
      struct clock_time clockface;
      struct clock_options options;
      long now;
      long epoch;
      struct event *ev;
};

