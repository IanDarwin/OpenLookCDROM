/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

class circlepi[circpi]: dataobject[dataobj] {
  classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct circlepi *self) returns boolean;
    FinalizeObject(struct circlepi *self);
  methods:
    SetDepth(int limit);
  macromethods:
    GetDepth() (self->depth_limit)
  data:
    int depth_limit;
};

