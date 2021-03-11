/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalsbv.ch,v 1.1 1992/09/15 21:30:18 rr2b R6tape $ */
/* $ACIS:eza.ch 1.4$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalsbv.ch,v $ */

class pvalsbv : wrapv {
classprocedures:
    InitializeObject(struct pvalsbv *self) returns boolean;
    FinalizeObject(struct pvalsbv *self);
overrides:
    ObservedChanged(struct observable *changed, long val);
    SetDataObject(struct dataobject *prefval);
macromethods:
    GetSButtonView() ((struct sbuttonv *)((struct wrapv *)self)->tv)
    GetSButton() ((struct sbutton *)((struct wrapv *)self)->t)
methods:
    UpdateSButton();
};

