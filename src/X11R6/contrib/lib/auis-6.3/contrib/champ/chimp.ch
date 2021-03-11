/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
struct comment {
    char *line;
    struct comment *next;
};

class chimp: chlist {
    overrides:
      Read(FILE *file, long id) returns long;
      Write(FILE *file, long writeID, int level) returns long;
    methods:
      AddNew(struct eventnode *en);
    data:
      struct enode *en;
      struct comment *comment;
};
