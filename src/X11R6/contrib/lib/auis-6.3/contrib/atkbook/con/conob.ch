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
class conob: dataobject [dataobj] {
    classprocedures:
      InitializeObject(struct conob *self) returns boolean;
      FinalizeObject(struct conob *self);
    overrides:
      Read(FILE *fp, long id) returns long;
      Write(FILE *fp, long writeID, int level) returns long;
      ViewName() returns char *;
    methods:
      SetNumval(long num);
      SetStrval(char *str);
      /* SetStrval doesn't make a new copy of the string */
      SetDisplayTemplate(char *displaytemplate);
      /* SetDisplayTemplate makes a new copy of the string */
      SetDisplayMin(int min);
      SetDisplayMax(int max);
      SetBoxed(boolean Boxed);
      GetStringToDisplay(char *Buf, int len, boolean IsClick);
      HandleDataLine(char *line);
      WriteState(FILE *fp);
    macromethods:
      GetNumval() (self->numval)
      GetStrval() (self->strval)
      GetDisplayTemplate() (self->DisplayTemplate)
      GetDisplayMin() (self->displaymin)
      GetDisplayMax(self->displaymax)
      GetBoxed() (self->Boxed)
    data:
      long numval;
      char *strval;
      int displaymin, displaymax;
      /* (The min & max for things like EKG graphs) */
      boolean Boxed; /* Draw a box or not? */
      char *DisplayTemplate;
      /* Tells what to say for indicators & clicks */
};
