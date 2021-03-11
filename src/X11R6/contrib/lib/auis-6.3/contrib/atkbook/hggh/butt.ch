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
class butt: view {
    overrides:
      FullUpdate(enum view_UpdateType type, long left, long top,
		  long width, long height);
      Update();
      Hit (enum view_MouseAction action, long x, long y,
	    long numberOfClicks) returns struct view *;
    classprocedures:
      InitializeObject(struct butt *self) returns boolean;
      FinalizeObject(struct butt *self);
    methods:
      SetText(char *txt);
      SetButtonFont(struct fontdesc *f);
      SetHitFunction(procedure HitFunction);
      SetRocks(char *r1, int r2);
    data:
      char *text;
      int (*HitFunction)();
      /* The four parameters to the HitFunction are
	long rock1, long rock2, struct butt * self,
	and enum view_MouseAction action */
      long rock1, rock2;
      struct fontdesc *myfontdesc;
      struct cursor *mycursor;
};

