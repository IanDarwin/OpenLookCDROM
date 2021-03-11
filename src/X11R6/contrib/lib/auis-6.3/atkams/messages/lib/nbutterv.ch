/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/nbutterv.ch,v 1.1 1991/09/27 20:36:16 rr2b R6tape $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/nbutterv.ch,v $ */

class nbutterv : sbuttonv {
overrides:
    Touch(int ind, enum view_MouseAction action) returns boolean;
};

