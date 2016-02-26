/* (C) Universitaet Passau 1986-1991 */
#include "std.h"
#include "sgraph.h"
#include "slist.h"
#include "graphed.h"

extern void Springemb (/* Sgraph_proc_info */);

char *Springembed (menu, menu_item)
  char *menu, *menu_item;
  
  {
   return call_sgraph_proc (Springemb);
  }
