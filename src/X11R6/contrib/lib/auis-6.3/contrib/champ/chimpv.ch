/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
class chimpview[chimpv]:lpair {
    classprocedures:
      InitializeObject(struct chimpview *self) returns boolean;
    overrides:
      SetDataObject(struct observable *v);
      LinkTree(struct view *parent);
    data:
      struct chlistview *lv;
      struct enodeview *env;
};
