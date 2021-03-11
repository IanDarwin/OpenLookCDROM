/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

class readscr {
methods:
classprocedures:
    Begin(struct text *t, int pos, int len, int purge, char *version, int GetTemplate) returns struct text *;
    PrintFile(char *filename, struct textview *tv, struct text *t, char *version, int TrashWhenDone) returns int;
data:
};
