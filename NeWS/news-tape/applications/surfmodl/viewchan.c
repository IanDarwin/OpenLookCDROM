#include "surfmodl.h"
/* Local variables, static */

boolean Viewchanged()
{
  boolean view;
  if ( (Xelast == Xeye) && ( Yelast== Yeye) && (Zelast == Zeye) &&
     ( Xflast==Xfocal ) && (Yflast ==Yfocal ) && (Zflast ==Zfocal ) &&
     (Vtlast == Viewtype) && (Maglast == Magnify) 
     && (Znearlast == Zcutnear))
    view =  FALSE;
  else
    view =  TRUE;

   Xelast= Xeye;
   Yelast= Yeye;
   Zelast= Zeye;
   Xflast= Xfocal;
   Yflast= Yfocal;
   Zflast= Zfocal;
   Vtlast = Viewtype;
   Maglast = Magnify;
   Znearlast = Zcutnear;
   return view;
} /* function Viewchanged */
