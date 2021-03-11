/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

class foldertreev[fldtreev]:orgv {
  classprocedures:
    InitializeClass( struct classheader *classID ) returns boolean;
    InitializeObject( struct classheader *classID, struct foldertreev *self ) returns boolean;
    FinalizeObject( struct classheader *classID, struct foldertreev *self );
  overrides:
    FullUpdate( enum view_UpdateType type, long left, long top, long width, long height ) returns void;
    PostMenus( struct menulist ) returns void;
    PostKeyState( struct keystate *keystate ) returns void;
  data:
    struct menulist	*menulist;
    struct keystate	*keystate;
};
