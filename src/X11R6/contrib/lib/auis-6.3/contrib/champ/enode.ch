/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
#include "champ.h"

class enode:dataobject[dataobj] {
    methods:
      SetEvent(struct eventnode *event);
      SetChimp(struct chimp *chimp);
    macromethods:
      GetEvent() (self->event)
    data:
      struct eventnode *event;
      struct chimp *mychimp;
};
