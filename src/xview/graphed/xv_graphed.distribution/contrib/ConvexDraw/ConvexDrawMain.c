/* (C) Universitaet Passau 1986-1991 */
/*1990-91 von Stefan Jockenhoevel und Gerd Nienhaus.*/

/*****************************************************************************************/
/*****************************************************************************************/
/**                                                                                     **/
/**                   MODULE ConvexDrawMain.c                                           **/
/**                                                                                     **/
/*****************************************************************************************/
/*****************************************************************************************/

#include "paths.h"
#include STDH
#include SGRAPHH
#include SLISTH
#include GRAPHEDH
#include "PossibleConvexDraw.h"
#include "ConvexTest.h"
#include "DrawConvex.h"

extern	int	IsConnected       ();
extern	int	IsStrongConnected ();
extern	int	IsBiConnected     (); 
  
/********************************************************************************/                                                  


void DrawGraphConvexStructur(info)
Sgraph_proc_info  info;


  {
     Sgraph  TheGraph;

     TheGraph = info->sgraph;

     if (TheGraph == empty_sgraph) {
     	/* skip */
     } else if (TheGraph->directed) {
	if (!IsStrongConnected(TheGraph)) {
	    error ("Graph is not strong connected.\n");
	    return;
	}
     } else {
	if (!IsConnected(TheGraph)) {
	    error ("Graph is not connected.\n");
	    return;
	} else if (!IsBiConnected(TheGraph)) {
	    error ("Graph is not biconnected.\n");
	    return;
	}
     }

     if(DrawConvexPossible(TheGraph))
       {
       
        info->recompute = TRUE;

        ExtendFacialCycle(TheGraph,ConvexityTest(TheGraph),FALSE);
        
       } 

    info->recenter = TRUE;
  }


/********************************************************************************/


void DrawGraphConvexEditable(info)
Sgraph_proc_info  info;


  {
     Sgraph  TheGraph;

     TheGraph = info->sgraph;

     if (TheGraph == empty_sgraph) {
     	/* skip */
     } else if (TheGraph->directed) {
	if (!IsStrongConnected(TheGraph)) {
	    error ("Graph is not strong connected.\n");
	    return;
	}
     } else {
	if (!IsConnected(TheGraph)) {
	    error ("Graph is not connected.\n");
	    return;
	} else if (!IsBiConnected(TheGraph)) {
	    error ("Graph is not biconnected.\n");
	    return;
	}
     }

     if(DrawConvexPossible(TheGraph))
       {
        
        info->recompute = TRUE;
        
        ExtendFacialCycle(TheGraph,ConvexityTest(TheGraph),TRUE);
        
       } 

    info->recenter = TRUE;
           
  }
  
  
/********************************************************************************/


char *DrawConvexStructur_menu_callback_proc(menu,menuitem)
char *menu,*menuitem;

  {
    return call_sgraph_proc(DrawGraphConvexStructur);
  }


/********************************************************************************/

char *DrawConvexEditable_menu_callback_proc(menu,menuitem)
char *menu,*menuitem;

  {
    return call_sgraph_proc(DrawGraphConvexEditable);
  }



  
