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

struct switchee {
    struct dataobject *d;
    char *viewname;
    char *label;
    struct switchee *next; 
};

class switcher : dataobject [dataobj] {
    classprocedures:
      InitializeObject(struct switcher *self) returns boolean;
      FinalizeObject(struct switcher *self);
    overrides:
      Read (FILE *file, long id) returns long;
      Write (FILE *file, long writeid, int level) returns long;
      ViewName() returns char *;
    methods:
      AddObject(struct dataobject *d, char *label,
		 char *viewname) returns boolean;
      DeleteObject(struct dataobject *d) returns boolean;
      SetNowPlaying(struct dataobject *d) returns boolean;
    data:
      struct switchee *FirstSwitchee, *NowPlaying;
};
