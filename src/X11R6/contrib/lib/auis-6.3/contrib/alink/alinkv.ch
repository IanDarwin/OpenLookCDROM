/* ********************************************************************** *\
 * Copyright (c) AT&T Bell Laboratories	1990  -	All Rights Reserved	  *
\* ********************************************************************** */

class alinkview[alinkv]: pushbuttonview[pshbttnv] {
 classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct alinkview *self) returns boolean;
  FinalizeObject(struct alinkview *self);
 methods:
  RecordAudio();
 overrides:	
  PostMenus(struct menulist *ml);
 data:
  struct menulist *ml;
};
