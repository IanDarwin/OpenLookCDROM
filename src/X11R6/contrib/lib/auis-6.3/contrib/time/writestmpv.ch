/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
#include <stdio.h>
#include <menulist.ih>
#include <cursor.ih>

class writestampview[writestmpv]: timeodayview[timeodayv] {
 classprocedures:
  InitializeClass() returns boolean;
  InitializeObject(struct writestampview *self) returns boolean;
  FinalizeObject(struct writestampview *self);
};

