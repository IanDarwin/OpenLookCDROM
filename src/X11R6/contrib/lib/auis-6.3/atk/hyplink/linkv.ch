/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
class linkview[linkv]: pushbuttonview[pshbttnv] {
 classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct linkview *self) returns boolean;
  FinalizeObject(struct linkview *self);
 overrides:	
  PostMenus(struct menulist *ml);
 methods:
  Link();
  AutoLink();
  LinkFile(char *dest);
 data:
  struct menulist *ml;
};

