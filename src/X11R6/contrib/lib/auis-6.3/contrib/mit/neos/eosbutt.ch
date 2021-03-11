/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosbutt.ch,v 1.3 1993/05/17 16:51:11 susan Exp $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosbutt.ch,v $ */

#ifndef lint
static char *rcsideosbutt = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/eosbutt.ch,v 1.3 1993/05/17 16:51:11 susan Exp $";
#endif /* lint */

/*
 * eosbutt.ch
 *
 * Package for creating lists of buttons.
 */

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 ************************************************************ */

#include <mit-copyright.h>
#include "environ.ih"
#include "lpair.ih"
#include "pshbttn.ih"
#include "newbttnv.ih"

/* Structure is linked list of buttons */     
struct buttonList {
  struct buttonList *next;
  struct pushbutton *butt;
  struct newbuttonview *buttv;
};

struct lplist {
    struct lplist *next;
    struct lpair *lp;
};

package eosbutton[eosbutt] {
    classprocedures:
      MakeButton (struct buttonList *blist,char *text, void (*function)(), struct view *object) returns struct buttonList *;
      MakeLpair (struct lplist *lpl) returns struct lplist *;
};
