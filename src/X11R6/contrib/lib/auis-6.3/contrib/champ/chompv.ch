/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
class chompview[chompv]: view {
    classprocedures:
      InitializeObject(struct chompview *cv) returns boolean;
      FinalizeObject(struct chompview *cv);
    overrides:
      FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
      DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes;
      Print(FILE *file, char *processor, char *finalFormat, boolean topLevel);
      Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
      LinkTree(struct view *parent);
    data:
      struct lpair *toplpair, *lp1;
      struct textview *tv;
      struct month *m1, *m2;
      struct monthview *mv1, *mv2;
      struct scroll *s;
};
