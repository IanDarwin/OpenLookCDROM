#include <TkWWWCmds.h>
#include <tcl.h>

int Tcl_AppInit(interp) 
     Tcl_Interp *interp;
{
  if (Tcl_Init(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  if (WWW_AppInit(interp) == TCL_ERROR) {
    return TCL_ERROR;
  }

  return (TCL_OK);
}
