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


/*
  Ooops.  I screwed up.

  It is the insets responsibility to read through their \enddata's.
  The old version of this code didn't.  This one does.
  Problem is, I changed the datastream representation that tickles this
  bug in the old version of this code.

  This means that if I were to release this code, with the new
  datastream, now, I will cause old code to break (badly) when reading
  documents created with the new code.

  So, for patch level 8, which the world will get (particularly CMU
  campus), the nifty color stuff has no UI, and the old datastream is
  written.  (But both the old and the new datastreams will be supported.)

  For patch level 9, undefine "PL8", and the UI to the new features will
  be available, and the new datastream will be written.  This means that
  people running PL8 will be able to read documents created using PL9 
  features.

  NOTE: users who save PL9-created documents under PL8-code will
  keep information, since the PL8 code will write the new datastream,
  if it reads an inset with the new datastream!

  So, remember inset designers, slurp up those \enddata's!
*/
/* #define PL8 */ /* enable color */

/*
  Appearances for pushbutton:

  Style  Look
    0    plain, in line text
    1    a boxed rectangle
    2    a motif-ish 3-Dish button
    3    A mac-ish rounded rectangle box
    4    a single rectangle
*/
#define pushbutton_PLAIN 0
#define pushbutton_BOXEDRECT 1
#define pushbutton_THREEDEE 2
#define pushbutton_ROUNDRECT 3
#define pushbutton_PLAINBOX 4
#define pushbutton_MOTIF 5

class pushbutton[pshbttn]: dataobject[dataobj] {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct pushbutton *self) returns boolean;
    FinalizeObject(struct pushbutton *self);
  overrides:
    Read (FILE *fp, long id) returns long;
    Write (FILE *fp, long id, int level) returns long;
  methods:
    SetText(char *txt);
    SetStyle(int stylecode);
    SetButtonFont(struct fontdesc *f);
    SetFGColor(char *name, int red, int green, int blue); /* if name is NULL, use RGB */
    SetBGColor(char *name, int red, int green, int blue);
    GetFGColor(unsigned char *rgb_vect) returns char *;
    GetBGColor(unsigned char *rgb_vect) returns char *;
    ParseRGB(char *rgb_string, unsigned char rgb_vect);
  macromethods:
    GetText() (self->text)
    GetStyle() (self->style)
    GetButtonFont() (self->myfontdesc)
  data:
    char *text;
    int style;
    struct fontdesc *myfontdesc;
    char *foreground_name, *background_name;
    unsigned char foreground_color[3], background_color[3];
#ifdef PL8
    int new_DS;
#endif /* PL8 */

};

