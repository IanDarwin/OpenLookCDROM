/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

class stroffetview[stroffetv] : iconview
{
 overrides:
    Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
    PostMenus(struct menulist *menulist);
 methods:
 classprocedures:
    InitializeObject(struct thisobject *self) returns boolean;
    InitializeClass() returns boolean;
 data:
	struct menulist *menus;
};
