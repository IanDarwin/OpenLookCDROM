/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalbv.ch,v 1.2 1992/09/16 23:26:24 rr2b R6tape $ */
/* $ACIS:eza.ch 1.4$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalbv.ch,v $ */

class pvalbv : pvalsbv {
classprocedures:
    InitializeObject(struct pvalbv *self) returns boolean;
    FinalizeObject(struct pvalbv *self);
overrides:
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    UpdateSButton();
data:
    int activated;
};

