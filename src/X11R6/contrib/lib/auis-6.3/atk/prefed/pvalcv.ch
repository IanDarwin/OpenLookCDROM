/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalcv.ch,v 1.1 1992/09/15 21:30:18 rr2b R6tape $ */
/* $ACIS:eza.ch 1.4$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalcv.ch,v $ */

class pvalcv : pvalsbv {
classprocedures:
    InitializeObject(struct pvalcv *self) returns boolean;
    FinalizeObject(struct pvalcv *self);
overrides:
    UpdateSButton();
};

