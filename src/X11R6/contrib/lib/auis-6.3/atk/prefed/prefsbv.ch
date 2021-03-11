/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University All rights Reserved. */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/prefsbv.ch,v 1.1 1992/09/15 21:30:18 rr2b R6tape $ */
/* $ACIS:eza.ch 1.4$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/prefsbv.ch,v $ */

class prefsbv: sbuttonv {
  classprocedures:
    InitializeObject(struct sbutton *self) returns boolean;
    FinalizeObject(struct sbutton *self);
  overrides:
    Touch(int ind, enum view_MouseAction act) returns boolean;
 };

