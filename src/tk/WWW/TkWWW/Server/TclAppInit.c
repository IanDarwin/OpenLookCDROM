#include <TkWWWCmds.h>
#include <TkWWWBitmap.h>
#include <tcl.h>
#include <tk.h>

int 
Tcl_AppInit(interp) 
     Tcl_Interp *interp;
{
  Tk_Window main;

  main = Tk_MainWindow(interp);

  if (Tcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (Tk_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (WWW_AppInit(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (WWW_BitmapInit(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  return (TCL_OK);
}
