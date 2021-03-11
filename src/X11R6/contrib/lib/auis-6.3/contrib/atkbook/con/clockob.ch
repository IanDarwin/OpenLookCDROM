/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
/* possible values for clockpart field of clockob structure */
#define CP_SEC 0
#define CP_MIN 1
#define CP_HR 2
#define CP_HRMIL 3
#define CP_MDAY 4
#define CP_WDAY 5 
#define CP_MON 6
#define CP_YEAR 7
#define CP_YDAY 8
#define CP_NUMPARTS 9 /* one plus greatest value above */

class clockob:conob {
    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct clockob *self) returns boolean;
      FinalizeObject(struct clockob *self);
    methods:
      SetClockPart(int cp);
    overrides:
      GetStringToDisplay(char *Buf, int len, boolean IsClick);
      WriteState(FILE *fp);
      HandleDataLine(char *line);
      ObservedChanged(struct contimer *ct, long code);
    macromethods:
      GetClockPart() (self->clockpart)
    data:
      int clockpart;
};
