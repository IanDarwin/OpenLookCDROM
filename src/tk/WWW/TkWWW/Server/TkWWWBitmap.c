#include <tk.h>
#include <logo.h>

int 
WWW_BitmapInit(interp)
     Tcl_Interp *interp;
{
  /* Create logo */
  Tk_DefineBitmap(interp, Tk_GetUid("HtLogoBitmap"), 
		  logo_bits, logo_width, logo_height);
  return (TCL_OK);
}

