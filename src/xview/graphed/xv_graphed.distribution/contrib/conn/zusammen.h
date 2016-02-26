/* (C) Universitaet Passau 1986-1991 */

/*-------------------------------------------------------------------------------------*/
/* 
       Carl Frerichs  WS'88
       
       ZUSAMMENHANG          bei ungerichteten Graphen
       STARKER ZUSAMMENHANG  bei gerichteten Graphen
       ZWEIFACH-ZUSAMMENHANG bei zusammenhaengenden ungerichteten Graphen 

       BIBLIOTHEK: /monique/ih88046/libzus.a   [ cc .... -lzus -L/monique/ih88046 ]					           	                                       */ 
/*-------------------------------------------------------------------------------------*/
/*     				    KNOTENLISTE  	           		       */
/*-------------------------------------------------------------------------------------*/

typedef struct nodelist { struct nodelist *next;
                          Snode            elem;
                        }
                         *Nodelist;



#define   empty_nodelist   ((Nodelist) NULL)      

extern Nodelist MakeNodelist();		
extern void AppNode();				
extern void DelNode();      			
extern Snode FirstNode();	
extern Nodelist RestNodelist();		
extern Nodelist CopyNodelist();		
extern void FreeNodelist();		


/*-------------------------------------------------------------------------------------*/
/*  			        KOMPONENTENLISTE                  		       */
/*-------------------------------------------------------------------------------------*/

typedef struct complist  {  struct complist  *next;
   			    Nodelist          elem;
			 }
			   *Complist;


#define   empty_complist   ((Complist) NULL)

extern Complist MakeComplist();	
extern void AppComp();
extern Nodelist FirstComp();
extern Complist RestComplist();	
extern void FreeComplist();
extern int SingleComp();


/*-------------------------------------------------------------------------------------*/
/*  			      ZUSAMMENHANGSTESTS            		      	       */
/*-------------------------------------------------------------------------------------*/

extern Complist GetStrongConComps();
extern int IsStrongConnected();
extern Complist GetConComps();
extern int IsConnected();
extern Complist GetBiconComps();
extern int IsBiConnected();
