/*                                                          Initialisation routines in libwww
                                  INITIALISATION MODULE
                                             
   This module resisters all the plug & play software modules which will be used in the
   program.  This is for a browser.
   
   To override this, just copy it and link in your version befoe you link with the
   library.
   
   Implemented by HTInit.c by default.
   
 */
#include "HTUtils.h"
#include "HTList.h"

extern void HTFormatInit PARAMS((HTList * conversions));
extern void HTFormatInitNIM PARAMS((HTList * conversions));
extern void HTFileInit NOPARAMS;





/*

    */
