#include <TkWWWCmds.h>
#include <TkWWWBitmap.h>
#include <tcl.h>
#include <tk.h>

extern char binary_tcl[], annotate_tcl[], bookmarks_tcl[], dialog_tcl[];
extern char edit_anchor_tcl[], edit_generate_tcl[], edit_modified_tcl[];
extern char edit_selection_tcl[], edit_tcl[], editwin_tcl[];
extern char file_tcl[], font_tcl[];
extern char grpan_tcl[], help_tcl[], history_tcl[];
extern char hypertext_tcl[], image_tcl[], init_tcl[];
extern char main_tcl[], menu_tcl[], msgdialog_tcl[], navigate_tcl[];
extern char output_tcl[], pan_tcl[];
extern char parse_args_tcl[], server_tcl[], telnet_tcl[], version_tcl[];

/*
 * The following variable is a special hack that allows applications
 * to be linked using the procedure "main" from the Tk library.  The
 * variable generates a reference to "main", which causes main to
 * be brought in from the library (and all of Tk and Tcl with it).
 */

extern int main();
int *tclDummyMainPtr = (int *) main;

#define EXEC_STRING(s) if ((result=Tcl_Eval(interp,s)) != TCL_OK) \
    return (result);

int 
Tcl_AppInit(interp) 
     Tcl_Interp *interp;
{
  Tk_Window main;
  int result;

  if (WWW_AppInit(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (WWW_BitmapInit(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  /*The following must be executed first */
  EXEC_STRING(binary_tcl);
  EXEC_STRING(init_tcl);

  /* Load the modules */
  EXEC_STRING(annotate_tcl);
  EXEC_STRING(bookmarks_tcl);
  EXEC_STRING(dialog_tcl);
  EXEC_STRING(edit_anchor_tcl);
  EXEC_STRING(edit_generate_tcl);
  EXEC_STRING(edit_modified_tcl);
  EXEC_STRING(edit_selection_tcl);
  EXEC_STRING(edit_tcl);
  EXEC_STRING(editwin_tcl);
  EXEC_STRING(file_tcl);
  EXEC_STRING(font_tcl);
  EXEC_STRING(grpan_tcl);
  EXEC_STRING(help_tcl);
  EXEC_STRING(history_tcl);
  EXEC_STRING(hypertext_tcl);
  EXEC_STRING(image_tcl);
  EXEC_STRING(menu_tcl);
  EXEC_STRING(msgdialog_tcl);
  EXEC_STRING(navigate_tcl);
  EXEC_STRING(output_tcl);
  EXEC_STRING(pan_tcl);
  EXEC_STRING(parse_args_tcl);
  EXEC_STRING(server_tcl);
  EXEC_STRING(telnet_tcl);
  EXEC_STRING(version_tcl);

  /*execute this last */
  EXEC_STRING(main_tcl);

  return (TCL_OK);
}
