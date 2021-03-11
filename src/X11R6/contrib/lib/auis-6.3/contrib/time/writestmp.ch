/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* #include <time.h> */
#include <fontdesc.ih>
#include <event.ih>

class writestamp[writestmp]: timeoday {
    classprocedures:
      InitializeClass() returns boolean;
      InitializeObject(struct writestamp *self) returns boolean;
      FinalizeObject(struct writestamp *self);
    overrides:
      WriteDataPart(FILE *fp);
      ReadDataPart(FILE *fp) returns long;
      UpdateTime();
};

