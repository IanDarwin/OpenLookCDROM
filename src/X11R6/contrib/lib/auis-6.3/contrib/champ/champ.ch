/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
#include "champ.h"

package champ {
    classprocedures:
      ReadDatesFromChampPath(char *path) returns int;
      ClearAllFlaggedEvents();
      FlagEventsMatchingDate(struct tm *thisdate) returns int;
      IterateFlaggedEvents(procedure proc, long rock);
      IncrementDate(struct tm *thisdate);
      ReadDateIntoEventNode(char *str) returns struct eventnode *;
};

