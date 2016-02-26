/* (C) Universitaet Passau 1986-1991 */
#include "termgraph_decl.h"
/*************************************************************************
**									**
**	MODULNAME:	termgraph_petrinet.c				**
**									**
**************************************************************************
**									**
**	ERSTELLUNG:	Rudolf Seisenberger, 	1990			**
**									**
**	AUFGABEN:	Das Modul enthaelt die Funktionen, die das 	**
**			Zeichnen eines Petrinetzes steuern und 		**
**			berechnen. Insbesondere werden die verschie-	**
**			denen Agentenoperationen und ihr Zusammen-	**
**			spiel untereinander in Konstruktionen umge-	**
**			setzt.						**
**									**
*************************************************************************/


/*************************************************************************
**									**
**	        EXTERNE FUNKTIONEN UND GLOBALE VARIABLEN		**
**									**
*************************************************************************/
extern Snode	insert_node_in_Sgraph_list();
extern int	insert_edge_in_Sgraph_list();
extern int	add_to_y(), add_to_x();
extern int 	sema_name_char();
extern int 	max_x_in_tree();
extern int 	min_x_in_tree();
extern pnode	*find_critical();
extern int	the_two_criticals();
extern int 	make_sema_construction ();

extern int	termgraph_edgeline_type;
extern int	termgraph_basic_length;
extern int	ELEMENT_LENGTH;
extern int 	CORX;
extern int 	CORY;
extern char 	flat_string[2 * MAXBUF];
extern int	termgraph_nodes_sum;


/*************************************************************************
**									**
**				FUNKTIONEN				**
**									**
*************************************************************************/
int		itoa();
pnode		*(next_edge_to_op());
int		lift_lower_node_y ();
pnode		*createpnode();
Edgeline 	(make_rec_itoa_string());
Snode		make_node_and_edge_for_parallel_places();
Edgeline 	termgraph_make_edgeline_angle();
int		operation_sema_edges();
int		operation_or_edges();
int		operation_parallel_construct();
int		graph_edges_top_down();
int		graph_nodes_top_down();
int 		recursion_under_construction ();
char		*init_graph_file();
pnode		*next_edge_to_op();
int 		graph_synchron_top_down();


int graph_edges_top_down(pgraph, p_input, num)
Sgraph	pgraph;
pnode	*p_input;
int	*num;
{
	int 		nsy, nsx, nti, nfi, nlp, nei, nlv, ncol, ecol, npx, npy,
				eti, efi, elv, al;
	float		aa;
	int		i, lactiv, ractiv, nx, ny, x1,x2,y1,y2;
	char 		step_char, np[MAXBUF], node_name[MAXNAME]; /*willkuerliche Obergrenzen*/
	pnode		*hlp, *psuc, *p;
	Snode		num_Snod, num2_Snod;
	Edgeline	el;
	
	if (p_input==NULL) return 0;
	
	p = p_input;	/* once more for edges, which can find now their targets */
	  
	if ((p->activ <= -1)){ 
		graph_edges_top_down( pgraph, p->left, num);
		graph_edges_top_down( pgraph, p->right, num);
		return 0;
	}
	
	npx = p->x;
	npy = p->y;
				
	if ( (p->right == NULL) ) {    /* no more subsequent edges */ 
	
	    if ( p->succ == NULL ) return 0;
	    if ( strcmp(p->name, AGENT_SKIP_NAME) == 0 ) return 0; /* ist ja ein Baumblatt */
	    
	    hlp = p->succ;
	    
	    switch (hlp->name[0])
	    {
	    case'|':
	    case'$':
	   	/*** zeichne Kante von Transition p->Snod zu Platz num_Snod ***/
	    	num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			*num++,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+ npx, 
			CORY- (npy - ELEMENT_LENGTH), 
			p->name);
		el = termgraph_make_edgeline_angle ('e', 
			npx, 	npy, 
			0, 	0, 
			npx, 	(npy - ELEMENT_LENGTH));
		insert_edge_in_Sgraph_list( p->Snod, num_Snod,
			*num++, 0,P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
			
		/*** sammelnder Abschluss der Parallelkonstruktion mit einer Hilfstransition ***/
		hlp->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			p->cardinal,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
			T_NCOL,
			CORX+ hlp->x,
			CORY- ( hlp->y ), 
			THETA_HELP_NAME);
						
		/*** aendere Position von hlp->succ->Snod: Nachfolger der Hilfstransition ***/
		if ( hlp->succ->activ > -1 ) {
			switch (hlp->succ->name[0])
			{
			case'$':  
			case'|':  
				num2_Snod = insert_node_in_Sgraph_list(pgraph, NULL,
					0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
					CORX+ hlp->x,
					CORY- (hlp->y - ELEMENT_LENGTH), 
					" ");
				el = termgraph_make_edgeline_angle ('e', 
					hlp->x,		hlp->y,
					0,		0,
					hlp->x, 	hlp->y - ELEMENT_LENGTH);
				insert_edge_in_Sgraph_list( hlp->Snod, num2_Snod,
					0, 0, P_ECOL,P_ETI,P_EFI,P_ELV, P_AL, P_AA, el,np," ");
				break;

			default:
				/*hlp->succ->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
					0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
					CORX+ hlp->succ->x,
					CORY- hlp->succ->y, 
					hlp->succ->name);
				el = termgraph_make_edgeline_angle ('h', 
					hlp->x, 	hlp->y, 
					0,		0,
					hlp->succ->x, 	hlp->succ->y ); 
				insert_edge_in_Sgraph_list( hlp->Snod, hlp->succ->Snod,
					0, 0, P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, 
					P_AA/3,el,np," ");*/
				break;
			} /*switch*/
		}

		/*** zeichne Kante von Platz num_Snod zu Transition hlp->Snod ***/
		el = termgraph_make_edgeline_angle ('h', 
			npx, 		npy - ELEMENT_LENGTH,
			0, 		0, 
			hlp->x,		hlp->y );
		insert_edge_in_Sgraph_list( num_Snod, hlp->Snod,
			*num++, 0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
		break;
		
	    case'<':
		/*** zeichne Kante zu dieser Hilfstransition ***/
		el = termgraph_make_edgeline_angle('h',npx, npy, 0,0, hlp->x, hlp->y);
	   	insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
	    		0, 0, P_ECOL, P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
	    	break;
	    	
	    case'%':
	    	psuc = hlp;
	    	if (hlp->succ != NULL) {
	    		if(0)
	        	hlp = hlp->succ;  /* `or` doesn`t end with its own place */
	        }
	        
	        hlp = psuc;
	        	        
	        /*** Abschluss von Nachfolgekonstruktion (Knoten hlp) abhaengig ***/
	        switch (hlp->succ->name[0])
	        {
	        case'|':
	        case'$':
	        case':': num_Snod = insert_node_in_Sgraph_list(pgraph, hlp->succ,
				0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ hlp->x,
				CORY- ( hlp->y ), 
				hlp->name);
			el = termgraph_make_edgeline_angle ('h', 
				hlp->x, 	hlp->y, 
				0,		0,
				hlp->succ->x, 	hlp->succ->y ); 
				/* after the 'or'-construct */
				
			/*** aendere hlp->succ->Snod, weil noch nicht an dieser Stelle da: ***/
			hlp->succ->Snod = insert_node_in_Sgraph_list(pgraph, hlp->succ->Snod, 0,
				THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,
				T_NFI,T_NLP,T_NEI,P_NLV,T_NCOL,
				CORX+ hlp->succ->x,
				CORY- ( hlp->succ->y ), 
				THETA_HELP_NAME);
				
			insert_edge_in_Sgraph_list( num_Snod, hlp->succ->Snod,
				*num++, 0, P_ECOL,P_ETI,P_EFI,P_ELV,3*P_AL/2,P_AA/3,el,np," ");
				
			el = termgraph_make_edgeline_angle('h',
				npx, 		npy, 
				0,		0, 
				hlp->x, 	hlp->y );
	   		insert_edge_in_Sgraph_list( p->Snod, num_Snod,
	    			0, 0, P_ECOL, P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
		  	break;
		  	
		case'<': hlp->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ hlp->x,
				CORY- hlp->y, 
				"<final>");  /* nomally just existing */
			el = termgraph_make_edgeline_angle('h',
				npx, 		npy, 
				0,		0, 
				hlp->x, 	hlp->y );
	   		insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
	    			0, 0, P_ECOL, P_ETI,P_EFI,P_ELV, P_AL, P_AA, el,np," ");
	    		break;	
	    		
	    	case';': hlp->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ hlp->succ->x,
				CORY- hlp->succ->y, 
				hlp->name);  /* target is ';'-node */
			hlp->x = hlp->succ->x;
			hlp->y = hlp->succ->y;
			el = termgraph_make_edgeline_angle('h',npx, npy, 0,0, hlp->x, hlp->y);
	   		insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
	    			0, 0, P_ECOL, P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
	    		break;
	    		
	    	case'%': hlp->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ hlp->x,
				CORY- ( hlp->y ), 
				hlp->name);
			el = termgraph_make_edgeline_angle('h',npx, npy, 0,0, hlp->x, hlp->y);
	   		insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
	    			0, 0, P_ECOL, P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
	    		
	    		/*** Leider sind Hilfsstelle (hlp->Snod) 
	    		und -transition (num_Snod) notwendig: ***/
	    		num_Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,T_NCOL,
				CORX+ hlp->x,
				CORY- ( hlp->y - ELEMENT_LENGTH),
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle ('e',
				hlp->x,		hlp->y,
				0,		0,
				hlp->x, 	hlp->y - ELEMENT_LENGTH);
			insert_edge_in_Sgraph_list( hlp->Snod, num_Snod,
				0, 0, P_ECOL,P_ETI,P_EFI,P_ELV, P_AL, P_AA, el,np," ");
				
			/*** Falls es diesen noch nicht gibt: ***/
			hlp->succ->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ 	hlp->succ->x,
				CORY-	hlp->succ->y, 
				hlp->succ->name);
			el = termgraph_make_edgeline_angle ('h', 
				hlp->x, 	hlp->y - ELEMENT_LENGTH, 
				0,		0,
				hlp->succ->x, 	hlp->succ->y ); 
			insert_edge_in_Sgraph_list( num_Snod, hlp->succ->Snod,
				0, 0, P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
	 	   	break;
	    		
		default: hlp->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ hlp->x,
				CORY- ( hlp->y ), 
				hlp->name);
			el = termgraph_make_edgeline_angle('h',npx, npy, 0,0, hlp->x, hlp->y);
	   		insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
	    			0, 0, P_ECOL, P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
	    		break; 
	    	}
	    	break;
	    	
	    case':':  /* Rekursion */
	    	el = make_rec_itoa_string(hlp, p, np);
	    	p->Snod = insert_node_in_Sgraph_list( pgraph, p,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,T_NCOL,
			CORX+	 p->width, 
			CORY-	 p->y, 
			THETA_HELP_NAME);
	    	num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+	 hlp->width, 
			CORY-	 ( hlp->height - ELEMENT_LENGTH ), 
			hlp->name);
		insert_edge_in_Sgraph_list( p->Snod, num_Snod,
			0,0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np, " ");
	    	break;
	    	
	    default:
	    	el = termgraph_make_edgeline_angle('h',npx, npy, 0,0, hlp->x, hlp->y);
	   	insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
	    		p->cardinal, 0, P_ECOL, P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
	    	break;
	    } /*switch*/
	    
	} else { 
	switch (p->name[0])
	{
	case'%':
		if (p->pred->name[0] == '<') {
			insert_node_in_Sgraph_list (
				pgraph, p , 0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ p->x, 
				CORY- p->y, 
				p->name );
			operation_or_edges ( pgraph, p, P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,P_NEI,
				P_NLV,P_ECOL,npx, npy, P_ETI,P_EFI,P_ELV,P_AL,P_AA, nx, ny, el, np );
 		} else 
 		if (p->pred->name[0] == ';') {
			operation_or_edges ( pgraph,p,P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_ECOL,
 				p->pred->x, p->pred->y, P_ETI,P_EFI,P_ELV,P_AL,P_AL, nx, ny, el, np );
		}
				
 		break;
			
	case'|':
		operation_parallel_construct ( pgraph, p, P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,P_NEI,
				P_NLV,P_ECOL,npx, npy, P_ETI,P_EFI,P_ELV,P_AL,P_AA, nx, ny, el, np );
		break;
		
	case'$': operation_sema_edges(  pgraph, p, num,P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,nei,P_NLV,
			P_NCOL,P_ECOL,npx,npy,P_ETI,P_EFI,P_ELV,P_AL,P_AA, el, np); 
		break;
				
	case'/':
		break;
	
	case'<':
		if ( (VALa(p->right, '/', name[0]) == ';') && (p->right->succ->name[0] == '%') ) {
			
			p->right->succ->Snod = 
			insert_node_in_Sgraph_list(pgraph, NULL,
				p->cardinal,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ p->width,
				CORY- p->height,
				p->name);
			p->right->succ->y = p->height;  /* adjust height */
			operation_or_edges ( pgraph, p->right->succ,
				P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_ECOL,
 				p->right->succ->x, p->right->succ->y, 
 				P_ETI,P_EFI,P_ELV,P_AL,P_AL, nx, ny, el, np );
		}

		break;
		
	case'.':
		el = make_rec_itoa_string(hlp, p, np);
		insert_edge_in_Sgraph_list( p->Snod, hlp->Snod, 0, 
			0, P_ECOL,P_ETI, P_EFI, 1/*P_ELV*/, P_AL,P_AA/3,el,np, NAMENODE(p)->name );
		break;
			
	case':':
		num_Snod = p->Snod;
		x1 = p->succ->name[0] == '%' ? (p->x = p->succ->width) : p->x;
		p->Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+ x1, /* Ausnahme wegen evtl. 'or' */
			CORY- ( p->height - ELEMENT_LENGTH ),
			p->name);
		el = termgraph_make_edgeline_angle ( 's',
			p->width, 	p->height, 
			0, 		0, 
			p->x, 		p->height - ELEMENT_LENGTH );
		p->width = p->x;
		insert_edge_in_Sgraph_list( num_Snod, p->Snod, 0,
				0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np, p->name);

		hlp = p->succ;
		switch (hlp->name[0])
		{
		case '%':
			hlp->Snod = p->Snod;
			hlp->height = ( p->height - ELEMENT_LENGTH );
			operation_or_edges ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,
				P_ECOL, npx, npy, P_ETI,P_EFI,P_ELV,P_AL,aa, npx, npy, el, np);
			break;
		
		default :
			el = termgraph_make_edgeline_angle ( 's',
				p->width,	p->height - ELEMENT_LENGTH,
				0,		0,
				VAL(hlp,x),	VAL(hlp,y) );
			insert_edge_in_Sgraph_list( p->Snod, hlp->Snod, 0,
				0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
			break;
		}
		
		break;
			
	case';':			
		if (p->right->name[0]=='%') {
			/*operation_or_edges ( pgraph,p->right,P_NSY,P_NSX,P_NTI,
			P_NFI,P_NLP,P_NEI,P_NLV,P_ECOL, npx, npy, P_ETI,
			P_EFI,P_ELV,P_AL,aa, nx, ny, el, np);*/
		} else if (p->right->name[0]==';') {
			if (p->right->succ->name[0] == '%') {
				/*operation_or_edges ( pgraph, p->right->succ, 
					P_NSY,P_NSX,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_ECOL, 
					npx, npy, P_ETI,P_EFI,P_ELV,P_AL,P_AA, nx,ny,el,np);*/
			} else if (p->right->name[0] == '.') {
				
			} else {
				el = termgraph_make_edgeline_angle('e', p->x,  p->y, 0, 0, 
					VALa(p->right->succ, npx, x),VALa(p->right->succ, npy, y));
				insert_edge_in_Sgraph_list( p->Snod, p->right->succ->Snod, 0, 
					0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
			}
		} else {
			el = termgraph_make_edgeline_angle('s', p->x,  p->y, 0, 0, 
				VALa(p->right, npx, x),VALa(p->right, npy, y));
			insert_edge_in_Sgraph_list( p->Snod, p->right->Snod,p->cardinal, 
				p->right->cardinal, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
		}
		
		break;
			
	default:
		if (p->left->name[0]==';'){
			hlp = (p->left->succ->name[0]==':')?
				(p->left->succ):p->left->succ;
			el = termgraph_make_edgeline_angle('s', 
				npx,npy,0,ELEMENT_LENGTH/2,VAL(hlp,x),VAL(hlp,y));
			insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
				p->cardinal, 0, P_ECOL,P_ETI, P_EFI, P_ELV,
				P_AL,P_AA,el,np,"na1");
		}else if (p->left->name[0]=='@'){
			hlp = p->left->succ;
			while ((hlp->succ != NULL) && (hlp!=p->left->succ))
				hlp = hlp->succ;
			el = termgraph_make_edgeline_angle('s', npx,npy,
				0,ELEMENT_LENGTH/2,VAL(hlp,x),VAL(hlp,y));
			insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
				p->cardinal, 0, P_ECOL, P_ETI, P_EFI, P_ELV, 
				P_AL,P_AA,el,np," ");
		}else if (p->left->name[0]=='.'){
			
		}else if (!((p->name[0]=='$') && (p->left->name[0]=='%'))){
			el = termgraph_make_edgeline_angle('s',npx, npy, 
				0,ELEMENT_LENGTH/2, p->left->x, p->left->y);
			insert_edge_in_Sgraph_list( p->Snod, p->left->Snod,
				0,0, P_ECOL, P_ETI, P_EFI, P_ELV, P_AL, P_AA, el,np, " ");
		}
		if (p->right->name[0]==';'){
			hlp = (p->right->succ->name[0]==':')?
				(p->right->succ):p->right->succ;
			el = termgraph_make_edgeline_angle('s', npx,npy,
				0,ELEMENT_LENGTH/2,VAL(hlp,x),VAL(hlp,y));
			if (p->right->succ->name[0]=='.')
			insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
				p->cardinal, hlp->cardinal, P_ECOL, P_ETI,P_EFI,P_ELV, 
				P_AL,P_AA,el,np,"na2");
		}else if (p->right->name[0]=='@'){
			hlp = p->right->succ;
			while ((hlp->succ != NULL) && (hlp!=p->right->succ))
				hlp = hlp->succ;
			el = termgraph_make_edgeline_angle('s', npx,npy,
				0,ELEMENT_LENGTH/2,VAL(hlp,x),VAL(hlp,y));
			insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
				0,0, P_ECOL, P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
		}else if (p->right->name[0]=='.'){
			insert_edge_in_Sgraph_list( p->Snod, hlp->Snod,
				0,0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np,p->right->name+1);
		}else if (!((p->name[0]=='$') && (p->right->name[0]=='%'))){
			el = termgraph_make_edgeline_angle('s', npx,npy,
				0,ELEMENT_LENGTH/2,p->right->x,p->right->y);
			insert_edge_in_Sgraph_list( p->Snod, p->right->Snod,
				0,0, P_ECOL, P_ETI, P_EFI, P_ELV, P_AL, P_AA,el,np," ");
		}
		break;
	} /*switch*/	
	} /*else*/
	
	if (p->left!=NULL) graph_edges_top_down( pgraph, p->left, num);
	if (p->right!=NULL) graph_edges_top_down( pgraph, p->right, num);
	
	return 0;
}  /* graph_edges_top_down */

int graph_nodes_top_down(pgraph, p_input, num)
Sgraph	pgraph;
pnode	*p_input;
int		*num;
{
	int 		nsy,nsx,nti,nfi,nlp,nei, nlv,ncol,ecol,npx,npy,
					eti,efi,elv,al;
	float		aa;
	int			i, lactiv, ractiv, nx, ny, x1, y1, x2, y2;
	Edgeline	el;
	char 		step_char, np[MAXBUF], node_name[MAXNAME]; /*willkuerliche Obergrenzen*/
	pnode		*hlp, *p;
	Snode		num_Snod, num2_Snod;
	
	if (p_input==NULL) return 0;
	
	p = p_input;
		  
	if ((p->activ <= -1)){
		graph_nodes_top_down( pgraph, p->left, num);
		graph_nodes_top_down( pgraph, p->right, num);
		return 0;
	}
	
	/*force_repainting();
	fscanf(stderr,"%c", &step_char);*/	
	
	npx = p->x;
	npy = p->y;
	
	switch(p->name[0])
	{
	case '$':
	case '|':
	case '.':
		insert_node_in_Sgraph_list( pgraph, p,
			p->cardinal,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,
			P_NLV/*!*/, T_NCOL, CORX+ npx, CORY- npy, THETA_HELP_NAME);
		break;
		
	case ':':
		insert_node_in_Sgraph_list( pgraph, p,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,
			P_NLV/*!*/, T_NCOL, CORX+ p->width, CORY- p->height, THETA_HELP_NAME);
		break;
		
	case '/':
		break;
		
	case '%':		
		if ((p->pred->name[0] == ';')) {
			insert_node_in_Sgraph_list(pgraph, p,
				0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ p->pred->x,
				CORY- p->pred->y,
				p->name);
		} 
							
		break;
	
	case '<':			
		if (p->right == NULL) {
			break;
		}
		
		if ((p->right->name[0] == ';') && (p->right->succ->name[0] == '%')) {
			/*p->Snod = p->right->left->Snod;
			p->x = p->right->left->x;
			p->height = p->right->left->height*/ 
			
		} 
		if (p->right->name[0] == '%') {
			p->activ = -1; /* there is no need to draw the "<final>"-node */
			
			p->right->Snod = p->Snod;
			p->right->x = p->x;
			p->right->height = p->height;
			p->right->y = p->y; /* prepares for `operation_or_edges` */
 		} else {
 			if (p->istarget == 1) {
 				insert_node_in_Sgraph_list (pgraph, p,
					p->cardinal,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,
					P_NLV,P_NCOL,CORX+npx,CORY-npy,"<final>");
			}
			
				
			if (p->right==NULL) {
				y1 = npy + ELEMENT_LENGTH;
			} else if ((p->right->name[0] == ';') 
				&& (p->right->succ->name[0] == '%')) {
				y1 = p->right->succ->height;
			} else if (p->right->name[0] == ';') {
				y1 = p->right->succ->height + ELEMENT_LENGTH;
			} else {
				y1 = p->right->height + ELEMENT_LENGTH;
			}
				
			num_Snod = insert_node_in_Sgraph_list (
				pgraph,NULL, 0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ npx, CORY- y1, p->name );
		
			el = termgraph_make_edgeline_angle ('e', npx, npy, 0, 0, npx, y1);
			hlp = p->right->name[0]==';' ? 
				VALa(p->right, p->right->left, succ) : p->right;
			num2_Snod = NULL;
			switch (hlp->name[0])
			{
			case'|':
			case'.':
			case':':
			case'$': num2_Snod = insert_node_in_Sgraph_list( pgraph,NULL, 
					0,THETA_HELP_NSX,
					THETA_HELP_NSY,	T_NTI,T_NFI,T_NLP,T_NEI,P_NLV,T_NCOL,
					CORX+ hlp->width,
					CORY- hlp->height,
					THETA_HELP_NAME); 
				insert_edge_in_Sgraph_list( num_Snod, num2_Snod, 0,
					0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");	
				break;
				
			case'%':
				hlp->x = p->x;
				hlp->height = p->height;
				hlp->y = p->y; /* prepare for `operation_or_edges` */
				num2_Snod = insert_node_in_Sgraph_list (pgraph,NULL, 
					0, P_NSX,P_NSY,
					P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
					CORX+ hlp->width, 
					CORY- hlp->height, 
					p->name ); 
				break;
			
			default:  num2_Snod = insert_node_in_Sgraph_list (pgraph,NULL, 
					0, P_NSX,P_NSY,
					P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
					CORX+ hlp->width, 
					CORY- hlp->height, 
					p->name );
					
				if (!( (p->right != NULL) && ( ((p->right->name[0] == ';') &&
					(strcmp(p->right->succ->name, AGENT_SKIP_NAME) == 0))
					|| (strcmp(p->right->name, AGENT_SKIP_NAME) == 0) ) )) {
					
					insert_edge_in_Sgraph_list( num_Snod, num2_Snod, 0,
						0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
				}	
				break;
			}
		}
		return 1; /*break;*/
		
	case ';': insert_node_in_Sgraph_list(pgraph, p,
			0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+npx,CORY-npy,p->name);
			
		if ( (p->right->name[0] == ';') && ( p->right->succ->name[0] == '%') ) {
			p->right->succ->Snod = p->Snod;
			p->right->succ->x = p->x;
			p->right->succ->height = p->y;
			p->right->succ->y = p->y; /* prepare for `operation_or_edges` */
		} 
		else if (p->right->name[0] == '%') {
			p->right->Snod = p->Snod;
			p->right->x = p->x;
			p->right->height = p->y;
			p->right->y = p->y; /*  prepares for `operation_or_edges` */
		}
		break;
		
	default:
		if (p->succ->name[0] == ':') {
			insert_node_in_Sgraph_list( pgraph, p,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,
				P_NLV/*!*/, T_NCOL, CORX+ npx, CORY- npy, THETA_HELP_NAME);
		} else if (strcmp(p->name, SEQUENCE_HELP_TRANSITION_NAME) == 0 /*e.g. "@SEQ"*/) {
			insert_node_in_Sgraph_list( pgraph, p,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,
				P_NLV, T_NCOL, CORX+ npx, CORY- npy, SEQUENCE_HELP_TRANSITION_NAME);
		} else if (strcmp(p->name, AGENT_SKIP_NAME) != 0) { /* do not draw a skip */
			insert_node_in_Sgraph_list( pgraph, p,
				0,T_NSX,T_NSY,T_NTI,T_NFI,T_NLP,T_NEI,T_NLV,T_NCOL,
				CORX+npx, CORY-npy, p->name);
		}
		break;
	}
	
	if (p->left!=NULL) graph_nodes_top_down( pgraph, p->left, num);
	if (p->right!=NULL) graph_nodes_top_down( pgraph, p->right, num);
	return 0;
}  /* graph_nodes_top_down */

int operation_parallel_construct ( pgraph, p, op, nsy,nsx,nti,nfi,nlp,nei,nlv,ecol,npx,npy,
	eti,efi,elv,al,aa, nx, ny, eline,np)
Sgraph 		pgraph;
pnode		*p;
char		op;
int		nsy,nsx,nti,nfi,nlp,nei,nlv,ecol,npx,npy, eti,efi,elv,al  , nx, ny;
float		aa;
Edgeline	eline;
char 		np[MAXBUF]; /*willkuerliche Obergrenzen*/
	
{
	int 		i, delta_y,lactiv, ractiv, *num=&(p->cardinal), insert;
	pnode		*hlp, *psuc;
	Snode		num_Snod, num2_Snod;
	Edgeline	el;
	
	i=0; 
	nx = p->width;
	ny = p->height;
	p->y = p->succ->y + LIFT_NODE( p->succ );	/* coordinates for the lower help-transition */
		/* schon hier, wegen Bedarf bei Aufruf von operation_or_edges */
	num2_Snod = p->Snod;
	p->Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+ p->width, 
			CORY- p->height, 
			p->name);
	lactiv = VALa(p->left,'/',name[0])=='/' ? -1 : VALa(p->left, 0, activ); 
	ractiv = VALa(p->right,'/',name[0])=='/' ? -1 : VALa(p->right, 0, activ);
	while (i != 2) {
			i=2;
			if ((hlp = next_edge_to_op('|',p->left)) != NULL) {
				if ( ( hlp->succ != NULL ) && (hlp->name[0] == ';') ) {
					hlp = hlp->succ;
				}
				insert = 1;
				if (hlp->name[0] == '%') {
					hlp->y = hlp->height =
						MIN( hlp->height + LIFT_NODE(hlp) - ELEMENT_LENGTH,
						p->height - ELEMENT_LENGTH );
					num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
						*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
						CORX+ hlp->x, 
						CORY- hlp->height,
						p->name);
					hlp->Snod = num_Snod;	
					operation_or_edges ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,
						P_NLP,P_NEI,P_NLV,P_ECOL, hlp->x, hlp->height,
						P_ETI,P_EFI,P_ELV,P_AL,aa, nx, ny,el, np);
					el = termgraph_make_edgeline_angle('s',
						nx,	ny,
						0, 	0,
						hlp->x,	SETY_NODE( hlp->height, p->height ) );
						
				} else if (strcmp(hlp->name, AGENT_SKIP_NAME) == 0) {
					insert = 0;
				} else {
					num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
						*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
						CORX+ hlp->x, 
						CORY- SETY_NODE( hlp->height, p->height ),
						p->name);
					el = termgraph_make_edgeline_angle('s',
						hlp->x,		SETY_NODE( hlp->height, p->height ),
						0,		0,
						hlp->x,		hlp->height);
					insert_edge_in_Sgraph_list( num_Snod, hlp->Snod,
						0,0,P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
					el = termgraph_make_edgeline_angle('s',
						nx,	ny,
						0, 	0,
						hlp->x,	SETY_NODE( hlp->height, p->height ) );
				}
				*num +=1;
				
				if (insert == 1) {
					insert_edge_in_Sgraph_list(p->Snod, num_Snod,
						0, 0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
				}	
				i--;
			}
			if ((hlp = next_edge_to_op('|',p->right)) != NULL) {
				if ( (hlp->succ != NULL) && (hlp->name[0] == ';') ) {
					hlp = hlp->succ;
				}
				insert = 1;
				if (hlp->name[0] == '%') {
					hlp->y = hlp->height =
						MIN( hlp->height + LIFT_NODE(hlp) - ELEMENT_LENGTH,
						p->height - ELEMENT_LENGTH );
					num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
						*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
						CORX+ hlp->x, 
						CORY- hlp->height,
						p->name);
					hlp->Snod = num_Snod;	
					operation_or_edges ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,
						P_NLP,P_NEI,P_NLV,P_ECOL, hlp->x, hlp->height,
						P_ETI,P_EFI,P_ELV,P_AL,aa, nx, ny,el, np);
					el = termgraph_make_edgeline_angle('s',
						nx,	ny,
						0,	0,
						hlp->x,	SETY_NODE( hlp->height, p->height ) );
						
				} else if (strcmp(hlp->name, AGENT_SKIP_NAME) == 0) {
					insert = 0;
				} else {
					num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
						*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
						CORX+ hlp->x, 
						CORY- SETY_NODE( hlp->height, p->height ),
						p->name);
					el = termgraph_make_edgeline_angle('s',
						hlp->x,		SETY_NODE( hlp->height, p->height) ,
						0,		0,
						hlp->x,		hlp->height);
					insert_edge_in_Sgraph_list( num_Snod, hlp->Snod,
						0,0,P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
					el = termgraph_make_edgeline_angle('s',
						nx,	ny,
						0,	0,
						hlp->x,	SETY_NODE( hlp->height, p->height) );
				}
				*num += 1;
				
				if (insert == 1) {
					insert_edge_in_Sgraph_list( p->Snod, num_Snod,
					0, 0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
				}
				i--;	
			}
		}
		if (p->left!=NULL) p->left->activ = lactiv;
		if (p->right!=NULL) p->right->activ = ractiv;
		p->x = nx;
				
		if (p->istarget == 1 /*TRUE*/) {   /* any concurrent construction expected ? */

		  switch (p->succ->name[0])
		  {
		  case '<':
		  case ';':
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( p->Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3,el,np," ");
			break;
		 case '%': /* the same? */
		 	p->y = p->succ->y + ELEMENT_LENGTH;
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( p->Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3,el,np," ");
			break;
		  case '|':
		  case '$':
			p->y += ELEMENT_LENGTH/2;
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ p->x, 
				CORY- ( p->y - ELEMENT_LENGTH),
				p->name);
			el = termgraph_make_edgeline_angle('e',
				p->x,		p->y,
				0,		0,
				p->x,		p->y - ELEMENT_LENGTH);
			insert_edge_in_Sgraph_list( p->Snod, num_Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");	
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y - ELEMENT_LENGTH,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( num_Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3 ,el,np," ");
			break;
		  default:
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( p->Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3 ,el,np," ");
			break;
		  } /*switch*/
		} /*if*/
	return 1;
}  /* operation_parallel_construct */

int operation_or_edges( pgraph, p, nsy,nsx,nti,nfi,nlp,nei,nlv,ecol,npx,npy, 
			eti,efi,elv,al,aa, nx, ny, el,np)
Sgraph 		pgraph;
pnode		*p;
int			nsy,nsx,nti,nfi,nlp,nei,nlv,ecol,npx,npy, eti,efi,elv,al, nx, ny;
float		aa;
Edgeline	el;
char 		np[MAXBUF]; /*willkuerliche Obergrenzen*/
	
{
	int 	i, delta_y,lactiv, ractiv, insert;
	pnode	*hlp, *psuc;
	Snode	num_Snod, p_Snod;
	
	i=0; 
	p_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
		0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
		CORX+ p->width, 
		CORY- p->height, 
		p->name);
	nx = p->width;
	ny = p->height;
	lactiv = VALa(p->left,'/',name[0])=='/' ? -1 : VALa(p->left, 0, activ); 
	ractiv = VALa(p->right,'/',name[0])=='/' ? -1 : VALa(p->right, 0, activ);
	while(i != 2){
		i=2;
		if ((hlp = next_edge_to_op('%',p->left)) != NULL) {
			if ((hlp->succ!=NULL)&&(hlp->left!=NULL)){
				if (hlp->name[0] == ';') {
					hlp = hlp->succ;
				}
			}
			insert = 1;
			if (hlp->name[0] == '%') {
				hlp->Snod = p_Snod;
				hlp->width = nx;
				hlp->height = ny;
				operation_or_edges
				 ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,
					P_NLP,P_NEI,P_NLV,P_ECOL, hlp->x, hlp->height,
					P_ETI,P_EFI,P_ELV,P_AL,aa, nx, ny,el, np);
					
			} else if (strcmp(hlp->name, AGENT_SKIP_NAME) == 0) {
				insert = 0;
			} else {
				delta_y = (p->activ/2)*(ELEMENT_LENGTH*3)/4;
				el = termgraph_make_edgeline_angle('s',nx,ny,0, 0, hlp->x, hlp->height);
			}
			
			if ((hlp->name[0] != '%') && (insert == 1)) {
				insert_edge_in_Sgraph_list( p_Snod, hlp->Snod,
					0, 0, P_ECOL, P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
			}
			i--;
		}
		if ((hlp = next_edge_to_op('%',p->right)) != NULL) {
			if ((hlp->succ!=NULL)&&(hlp->left!=NULL)){
				if (hlp->name[0] == ';') {
					hlp = hlp->succ;
				}
			}
			insert = 1;
			if (hlp->name[0] == '%') {
				hlp->Snod = p_Snod;
				hlp->width = nx;
				hlp->height = ny;
				operation_or_edges ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,
					P_NLP,P_NEI,P_NLV,P_ECOL, hlp->x, hlp->height,
					P_ETI,P_EFI,P_ELV,P_AL,aa, nx, ny,el, np);
					
			} else if (strcmp(hlp->name, AGENT_SKIP_NAME) == 0) {
				insert = 0;
			} else {
				delta_y = (p->activ/2)*(ELEMENT_LENGTH*3)/4;
				el = termgraph_make_edgeline_angle('s',nx,ny,0, 0, hlp->x, hlp->height);
			}
			
			if ((hlp->name[0] != '%') && (insert == 1)) {
				insert_edge_in_Sgraph_list( p_Snod, hlp->Snod,
					0, 0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");	
			}
			i--;	
		}
	}
	if (p->left!=NULL) p->left->activ = lactiv;
	if (p->right!=NULL) p->right->activ = ractiv;
	p->y = p->succ->y + LIFT_NODE( p->succ );

 	if (p->istarget == 1 /*TRUE*/) {   /* any concurrent construction expected ? */

	  switch (p->succ->name[0])
	  {
	  case '%':
		p->y += ELEMENT_LENGTH/2;
		p->Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+ p->x, 
			CORY- p->y, 
			p->name);
		num_Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,
			P_NLV, T_NCOL, 
			CORX+ p->x, 
			CORY- ( p->y -  ELEMENT_LENGTH), 
			THETA_HELP_NAME);
		el = termgraph_make_edgeline_angle('e',
			p->x,		p->y,
			0,		0,
			p->x,		p->y - ELEMENT_LENGTH);
		insert_edge_in_Sgraph_list( p->Snod, num_Snod,
			0,0,P_ECOL,P_ETI,P_EFI,P_ELV, P_AL, P_AA, el,np," ");
		el = termgraph_make_edgeline_angle('h',
			p->x,		p->y - ELEMENT_LENGTH,
			0,		0,
			p->succ->x,	p->succ->y );
		if (VALa(p->succ->pred,'@',name[0]) == '<') {
			p->succ->Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ p->succ->x, 
				CORY- p->succ->y, /* = 0 */
				p->succ->name);
		}
		insert_edge_in_Sgraph_list( num_Snod, p->succ->Snod,
			0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
		break;
		
	  case ';':
	  	p->y = p->succ->y;
	  	p->x = p->succ->x;
	  	p->Snod = p->succ->Snod;	
	  	break;
	  	
	  case '|':
	  case '$':
		p->Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+ p->x, CORY- p->y, p->name); 
			
		if ((p->succ->succ->name[0] == '|') || (p->succ->succ->name[0] == '$')
			&& ( recursion_under_construction(p, p->succ) != 1 )) {
			delta_y = ELEMENT_LENGTH / 3 ;
		} else {
			delta_y = 0;
		}
		
		el = termgraph_make_edgeline_angle('h',
			p->x,		p->y,
			0,		0,
			p->succ->x,	p->succ->y + delta_y);
		num_Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV,
			T_NCOL,
			CORX+ p->succ->x,
			CORY- ( p->succ->y + delta_y ) ,
			THETA_HELP_NAME); 

		insert_edge_in_Sgraph_list( p->Snod, num_Snod,
			0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3, el,np," ");
		break;	
		
	  case '<':
	  	p->y = p->succ->y;
	  	p->x = p->succ->x; 
	 	p->Snod = insert_node_in_Sgraph_list( pgraph, NULL,
			0,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
			CORX+ p->x, CORY- p->y, "<final>");
		break;
	  default:
		break;
	  } /*switch*/
	} /*if*/
	
	return 0;
} /* operation_or_edges */

int recursion_under_construction (p, f)
pnode *p, *f;
{
	int return_flag = 0;
	
	while (p != NULL) {
		if (p->pred == f) {
			p = NULL;
		} else if (p->name[0] == ':') {
			return_flag = 1;
			p = NULL;
		}
		p = p->pred;
	}
	return return_flag;
} /* recursion_under_construction */

int operation_sema_edges( pgraph, p, num, nsy,nsx,nti,nfi,nlp,nei,nlv,ncol,ecol,
			npx,npy,eti,efi,elv,al,aa, eline, np)
Sgraph		pgraph;
pnode		*p;
int			*num, nsy,nsx,nti,nfi,nlp,nei,nlv,ncol,ecol,npx,npy, eti,efi,elv,al;
float		aa;
Edgeline	eline;
char 		np[MAXBUF]; /*willkuerliche Obergrenzen*/
	
{
	int 		i, lactiv, ractiv, nx, ny, insert;
	pnode		*hlp, *psuc;
	Snode		num_Snod, num2_Snod;
	Edgeline	el;
	
	nx = p->x;
	ny = p->y;
	p->y = p->succ->y + LIFT_NODE( p->succ ); 	/* coordinates for the lower help-transition */
	lactiv=(p->left->name[0]=='/')?-1:VAL(p->left,activ); 
	ractiv=(p->right->name[0]=='/')?-1:VAL(p->right,activ);
	if ((p->left) != NULL) {
		hlp = (p->left->name[0] == ';'? p->left->succ: p->left);
		insert = 1;
		if (hlp->name[0] == '%') {
			hlp->height = hlp->y = 
				MIN( hlp->height + LIFT_NODE(hlp) - ELEMENT_LENGTH, 
				p->height - ELEMENT_LENGTH );
			num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+	 hlp->x,
				CORY-	 hlp->height,
				p->name);
			hlp->Snod = num_Snod;
			operation_or_edges ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,
				P_NLP,P_NEI,P_NLV,P_ECOL, hlp->x, hlp->height,
				P_ETI,P_EFI,P_ELV,P_AL,aa, nx, ny,el, np);
			el = termgraph_make_edgeline_angle('s',
				nx,		ny,
				0,		0,
				hlp->x,		SETY_NODE( hlp->height, p->height ) );
				
		} else if (strcmp(hlp->name, AGENT_SKIP_NAME) == 0) {
			insert = 0;
		} else {
			num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+	 hlp->x,
				CORY-	 SETY_NODE( hlp->height, p->height ),
				p->name);
			el = termgraph_make_edgeline_angle('s',
				hlp->x,		SETY_NODE( hlp->height, p->height ),
				0,		0,
				hlp->x,		hlp->height );
			insert_edge_in_Sgraph_list( num_Snod, hlp->Snod,
				0, 0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
			el = termgraph_make_edgeline_angle('s',
				p->x,		p->height,
				0,		0,
				hlp->x,		SETY_NODE( hlp->height, p->height ) );	
		}
		*num +=1;
		
		if (insert == 1) {
			insert_edge_in_Sgraph_list( p->Snod, num_Snod, 
				*num, 0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
		}
			
	}
	if ((p->right) != NULL) {
		hlp = (p->right->name[0] == ';'? p->right->succ: p->right);
		insert = 1;
		if (hlp->name[0] == '%') {
			hlp->height = hlp->y = 
				MIN( hlp->height + LIFT_NODE(hlp) - ELEMENT_LENGTH, p->height - ELEMENT_LENGTH );
			num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+	 hlp->x,
				CORY-	 hlp->height,
				p->name);
			hlp->Snod = num_Snod;
			operation_or_edges ( pgraph, hlp, P_NSY,P_NSX,P_NTI,P_NFI,
				P_NLP,P_NEI,P_NLV,P_ECOL, hlp->x, hlp->height,
				P_ETI,P_EFI,P_ELV,P_AL,aa, nx, ny,el, np);
			el = termgraph_make_edgeline_angle('s',
				nx,		ny,
				0,		0,
				hlp->x,		SETY_NODE( hlp->height, p->height ) );
		} else if (strcmp(hlp->name, AGENT_SKIP_NAME) == 0) {
			insert = 0;
		} else {
			num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				*num,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+	 hlp->x,
				CORY-	 SETY_NODE( hlp->height, p->height ),							p->name);
			el = termgraph_make_edgeline_angle('s',
				hlp->x,		SETY_NODE( hlp->height, p->height ),
				0,		0,
				hlp->x,		hlp->height );
			insert_edge_in_Sgraph_list( num_Snod, hlp->Snod,
				0, 0, P_ECOL,P_ETI, P_EFI, P_ELV, P_AL,P_AA,el,np," ");
			el = termgraph_make_edgeline_angle('s',
				p->x,		p->height,
				0,		0,
				hlp->x,		SETY_NODE( hlp->height, p->height ) );	
		}
		*num +=1;
		
		if (insert == 1) {
			insert_edge_in_Sgraph_list( p->Snod, num_Snod, 
				*num, 0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
		}
		
	}

	if (p->left!=NULL) p->left->activ = lactiv;
	if (p->right!=NULL) p->right->activ = ractiv;
	p->Snod = num2_Snod;
	p->x = nx;
	/* p->y = p->succ->y + LIFT_NODE( psuc ); 	see upon */
	
	if (p->istarget == 1 /*TRUE*/) {   /* any concurrent construction expected ? */
		switch (p->succ->name[0])
		{
		case '<':
		case ';':
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( p->Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3,el,np," ");
			break;
		case '%': /* the same? */
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( p->Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3,el,np," ");
			break;
		case '|':
		case '$':
			p->y += ELEMENT_LENGTH/2;
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			num_Snod = insert_node_in_Sgraph_list( pgraph, NULL,
				0, P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,P_NLV,P_NCOL,
				CORX+ p->x, 
				CORY- ( p->y - ELEMENT_LENGTH/*2/3*/ ),
				p->name);
			el = termgraph_make_edgeline_angle('e',
				p->x,		p->y,
				0,		0,
				p->x,		p->y - ELEMENT_LENGTH/*2/3*/ );
			insert_edge_in_Sgraph_list( p->Snod, num_Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");	
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y - ELEMENT_LENGTH/*2/3*/,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( num_Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3 ,el,np," ");
			break;
		default:
			p->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
				0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
				T_NCOL,
				CORX+ p->x,
				CORY- p->y, 
				THETA_HELP_NAME);
			el = termgraph_make_edgeline_angle('h',
				p->x,		p->y,
				0,		0,
				p->succ->x,	p->succ->y);
			insert_edge_in_Sgraph_list( p->Snod, p->succ->Snod,
				0,0,P_ECOL,P_ETI,P_EFI,P_ELV, 3*P_AL/2, P_AA/3 ,el,np," ");
			break;
		} /*switch*/
	} /*if*/
	
	return 0;
}  /* operation_sema_edges */

Edgeline termgraph_make_edgeline_angle(mode, px, py, dx, dy, qx, qy)
char		mode;
int 		px, qx, dx, dy, py, qy;
{
	char		m;
	int 		mult, i, j, a1,a2,a3,a4, x1,x2,y1,y2,y3;
	Edgeline	el, first_el;
	
	i = MAX(PLACEX/2,TRANSX/2) +1;	/* minimum distance to put an angle near */
	j = MAX(PLACEY/2,TRANSY/2) +1;	/* a node icon */
	m = ((ABS(px-qx)<=i) || ( ABS(py-qy)<=j)) ||
		((2 /*pixel as a secure distance*/ +ABS(px-qx))*ABS(py-qy) <  j*i)  ? 
		(mode=='s'? 'n':'e')  :  mode;
	if ( (px == qx) && (m == 'n') ) {
		m = 'e';
	}
					
	switch (m)
	{
	case 'v':	a1 = MIN (ABS(qy-py)-2*ARROW_LENGTH -dy -PLACEY/2, ABS(qx-px)) / 10;
			x1 = px<qx? qx-dx-(qx-px)/10 : qx+dx+(px-qx)/10;
			y1 = py<qy? py+ a1 : py- a1;
		
			first_el = new_edgeline (CORX+px, CORY-qx);
			el = add_to_edgeline (first_el, CORX+x1, CORY-(py-dy));
			el = add_to_edgeline (el, CORX+qx, CORY-(y1-dy));
			el = add_to_edgeline (el, CORX+qx, CORY-qy);
			break; /* variation: side first, symmetric line corners  */
					
	case 's':	x1 = px<qx? qx-ABS(qx-px)/10 : qx+ABS(px-qx)/10;
			a2 = MIN( 6*ELEMENT_LENGTH/10,  ABS(qy-py) - j );
			y2 = py<qy? py + a2 : py - a2;
			a1 = MIN( 4*ELEMENT_LENGTH/10,  ABS(qy-py) - j );
			y1 = py<qy? py + a1 : py - a1;	
						
			first_el = new_edgeline (CORX+px, CORY-py);
			el = add_to_edgeline (first_el, CORX+x1, CORY-y1);
			if (a1 < ABS(qy-py) - j) {
				if (termgraph_edgeline_type != 0) {
					a3 = (x1-qx)/3;
					y3 = MIN(y1, y2) + ABS(y1 - y2)/2;
					el = add_to_edgeline (el, CORX+ qx + a3, CORY-y3);
				
				}
				el = add_to_edgeline (el, CORX+qx, CORY-y2);
			}
			el = add_to_edgeline (el, CORX+qx, CORY-qy);
			break; /* side first, symmetric line corners  */
			
	case 'S':	x1 = px<qx? qx-ABS(qx-px)/10 : qx+ABS(px-qx)/10;
			a2 = MIN( 3 * ABS(qy-py)/10,  ABS(qy-py) - j );
			y2 = py<qy? py + a2 : py - a2;
			a1 = MIN( 2 * ABS(qy-py)/10,  ABS(qy-py) - j );
			y1 = py<qy? py + a1 : py - a1;	
						
			first_el = new_edgeline (CORX+px, CORY-py);
			el = add_to_edgeline (first_el, CORX+x1, CORY-y1);
			if (termgraph_edgeline_type != 0) {
				a3 = (x1-qx)/3;
				y3 = MIN(y1, y2) + ABS(y1 - y2)/2;
				el = add_to_edgeline (el, CORX+ qx + a3, CORY-y3);
			}
			el = add_to_edgeline (el, CORX+qx, CORY-y2);
			el = add_to_edgeline (el, CORX+qx, CORY-qy);
			break; /* side first, symmetric line corners  */		
			
	case 'n':	y1 = MIN(qy, py) + j +ELEMENT_LENGTH/3 + 0* (ABS(py - qy))/2;
			first_el = new_edgeline (CORX+px, CORY-py);
			el = add_to_edgeline (first_el, CORX+qx, CORY-y1);
			el = add_to_edgeline (el, CORX+qx, CORY-qy);
			break; /* side first, close neighbourhood  */
						

	case 'h':	x1 = px<qx? px+ABS(qx-px)/10 : px-ABS(px-qx)/10;
			a1 = MIN( 6*ELEMENT_LENGTH/10,  ABS(qy-py) - j );
			y1 = py<qy? qy - a1 : qy + a1;
			a2 = MIN( 4*ELEMENT_LENGTH/10,  ABS(qy-py) - j );
			y2 = py<qy? qy - a2 : qy + a2;
			
			first_el = new_edgeline (CORX+px, CORY-py);
			el = add_to_edgeline (first_el, CORX+px, CORY-y1);
			if (a1 < ABS(qy-py) - j) {
				if (termgraph_edgeline_type != 0) {
					a3 = (x1-px)/3;
					y3 = MIN(y1, y2) + ABS(y1 - y2)/2;
					el = add_to_edgeline (el, CORX+ px + a3, CORY-y3);
				}
				el = add_to_edgeline (el, CORX+x1, CORY-y2);
			}
			el = add_to_edgeline (el, CORX+qx, CORY-qy);
			break; /* height first */
				
	case 'e':	first_el = new_edgeline (CORX+px, CORY-py);
			add_to_edgeline (first_el, CORX+qx, CORY-qy);
			break; /* straight along Euclidean Distance */
	}
	return (first_el);
} /* termgraph_make_edgeline_angle */


Edgeline (make_rec_itoa_string(hlp_out, p, np))
pnode		*hlp_out;
pnode		*p;
char 		np[MAXBUF];
{
	Edgeline	first_el, el;
	pnode 		*pre, *rec, *nam;
	int 		px,py,rx,ry,nx,ny;
		
	rec = p;
	pre = rec->succ;
	nam = NAMENODE( pre );
	rx = VALa(rec, CORX, x);
	ry = VALa(rec, CORY, y);
	nx = VALa(nam, CORX, x);
	px = VALa(pre, CORX, x);
	py = VALa(pre, CORY, height);
	
	first_el = new_edgeline (
				CORX+ rx,                       CORY- ry);
				
	if (termgraph_edgeline_type != 0) { /* fine edgeline */
		el = add_to_edgeline (first_el, 
				CORX+ MIN(nx,rx)+ ( rx < nx ? ABS(rx-nx)/4 : 3*ABS(rx-nx)/4 ),
				CORY- (ry - 2*ELEMENT_LENGTH/5) );
		el = add_to_edgeline (el, 
				CORX+ MIN(nx,rx)+ ABS(rx-nx)/2, 
				CORY- (ry - ELEMENT_LENGTH/2) );
		el = add_to_edgeline (el, 
				CORX+ MIN(nx,rx)+ ( rx > nx ? ABS(rx-nx)/4 : 3*ABS(rx-nx)/4 ), 
				CORY- (ry - 2*ELEMENT_LENGTH/5) );
		el = add_to_edgeline (el,
				CORX+ nx,  			CORY- ry);
		el = add_to_edgeline (el,
				CORX+ nx,  			CORY- (py - (ELEMENT_LENGTH + ELEMENT_LENGTH/2)) );
		if (pre->activ <= 2) {
			el = add_to_edgeline (el,
				CORX+ MIN(px,nx)+ ( px > nx ? ABS(px-nx)/6 : 5*ABS(px-nx)/6 ),
				CORY- (py - (ELEMENT_LENGTH + ELEMENT_LENGTH/4) ) );
			el = add_to_edgeline (el,
				CORX+ MIN(px,nx)+ ABS(px-nx)/2,    CORY- (py - ELEMENT_LENGTH ) );	
		}
	} else { /* standard hard edgeline */
		el = add_to_edgeline (first_el, 
				CORX+ MIN(nx,rx)+ ABS(rx-nx)/2, 
				CORY- (ry - ELEMENT_LENGTH/2) );
		el = add_to_edgeline (el,
				CORX+ nx,  			CORY- ry);
		el = add_to_edgeline (el,
				CORX+ nx,  			CORY- (py - (ELEMENT_LENGTH + ELEMENT_LENGTH/2)) );
		if (pre->activ <= 2) {
			el = add_to_edgeline (el,
				CORX+ MIN(px,nx)+ ABS(px-nx)/2,    CORY- (py - ELEMENT_LENGTH ) );
		}
	}
	
	el = add_to_edgeline (el,
				CORX+ px,                       CORY- ( py - ELEMENT_LENGTH ));
	                            
	hlp_out = pre;
	return first_el;
} /* make_rec_itoa_string */

Snode make_node_and_edge_for_parallel_places( pgraph,number,nsx,nsy,nti,nfi,nlp,
	nei,nlv,ncol,ecol,npx,npy, eti, efi, elv, al,aa,p,psuc)
Sgraph	pgraph;
int 	*number,nsx,nsy,nti,nfi,nlp,nei,nlv,ncol,ecol,npx,npy,eti, efi, elv, al;
float	aa;
pnode	*p,*psuc;
{
	char 		np[MAXBUF];
	int			x1;
	Snode 		pnod;
	Edgeline 	el;
	
	x1 = psuc->x;
	pnod = insert_node_in_Sgraph_list(pgraph, NULL,
		*number,P_NSX,P_NSY,P_NTI,P_NFI,P_NLP,P_NEI,
		P_NLV,P_NCOL, CORX+x1,CORY-(npy+psuc->y)/2,p->name);
	el = termgraph_make_edgeline_angle('e',x1,npy,0,0,VAL(psuc,x),VAL(psuc,y));
	insert_edge_in_Sgraph_list(pnod, psuc->Snod,
		*number,psuc->cardinal,P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
	*number +=1;
	return pnod;
} /* make_node_and_edge_for_parallel_places */

pnode *(find_node_for_next_edge_to_op(op,p))
char 	op;
pnode	*p;
{
	pnode *hlp;
	char half_activ[2];
	
	if (p==NULL) {
		return NULL;
	}
	half_activ[0] = '/';
	half_activ[1] = op;

	if ((p->activ==-1) && (p->name[0] == op)) {
		return NULL;
	}
	if ((p->activ==0) && (p->name[0] != op) && (strncmp(p->name,half_activ,2)!=0)) { 
		return NULL;
	}
	
	if ((p->activ!=0) && (strncmp(p->name,half_activ,2)!=0)){
		p->activ = 0;
		return p;
	}
	
	if ((p->activ==1) && (strncmp(p->name,half_activ,2)==0)){
		if ((hlp = next_edge_to_op(op, p->right)) != NULL) {
			return hlp;
		} else {
			p->name[0] = op;
			p->activ = -1;
			return NULL;
		}
	}
	if ((p->activ==0) && (strncmp(p->name,half_activ,2)==0)) {
		if ((hlp = next_edge_to_op(op, p->left)) != NULL) {
			return hlp;
		} else {
			p->activ = 1;
			return (next_edge_to_op(op, p));
		}
	}
	return NULL; /* never reached */
} /* find_node_for_next_edge_to_op */

pnode	*next_edge_to_op(op,p)
char 	op;
pnode	*p;
{
	pnode *phelp;
	
	if ((phelp = find_node_for_next_edge_to_op(op,p)) != NULL) {
		/*phelp->y = p->y - ELEMENT_LENGTH/2;*/
		/*add_to_y (phelp, -ELEMENT_LENGTH);*/ /* layout modification */
	}
	return phelp;
} /* next_edge_to_op */
	
int  check_local_successor_activity(op,pnumber,px,py,psuc)
/*** if psuc->name[0]==op then erase its identity and repeat node(pnumber) in psuc ***/

char	op;
int 	pnumber, px, py;
pnode	*psuc;
{
	if (VALa(psuc,'/',name[0])==op) {
		psuc->x = px;
		psuc->y = py;  /* edges of psuc have to take "p" as source */
		psuc->cardinal = pnumber;
	}
	return 0;
} /* check_local_successor_activity */

int lift_lower_node_y (node)
pnode *node;
{
	int y;
	
	if ((node->succ == NULL) || (node->name[0] == '<')) {
		y = node->y;
	} else {
		y = node->succ->y + LIFT_NODE( node );
	}
	return y;
}  /* lift_lower_node_y */

int reverse(s)
char s[MAXBUF];
{
	int i,j;
	char c;
	
	for (i=0, j=strlen(s)-1; i<=j; i++, j--){
		c = s[i];
		s[i] = s[j];
		s[j] = c;
	}
	s[i] = '\0';
	return 0;
} /*reverse*/

int itoa(n,s)
int n;
char s[];
{	
	int i,sign;
	
	if ((sign = n) <0)
		n = -n;
	i = 0;
	do {
		s[i] = n % 10 + '0';
		i++;
	} while (( n /= 10 ) > 0);
	if (sign < 0){
		s[i] = '-';
		i++;
	}
	s[i] = '\0';
	reverse(s);
	
	return i;
}   /*itoa*/

int termgraph_draw_petrinet( pgraph, filep, p, num, argc, argv, term)
Sgraph	pgraph;
FILE	*filep;
pnode	*p;
int	*num, argc;
char	*argv[], *term;
{
	char				*name;
	FILE				*fip;
	int 			    	i, tlength, maxlength=0, y, ntimes, ncol= 15/*black*/;
	int				wid, hei;
	Edgeline			el;
	char 				np[MAXBUF], str[MAXBUF];
	Graphed_node			gpnod;
	Snode				pnod;
	Attributes			pattrs;
		
	termgraph_nodes_sum = 0; /* reset */
	
	if ((p != NULL) && (p->right != NULL) && 
		(p->right->name[0] != '\0')) { /* there is be a graph to draw */
		graph_nodes_top_down( pgraph, p->right, num );
		graph_nodes_top_down( pgraph, p, num );	/* only to draw nodes */
		graph_edges_top_down( pgraph, p, num );
		graph_synchron_top_down( pgraph, p->right );
	}
	
	y = 40; /* Kastenhoehe im Beschriftungsfeld */
	
	if (strncmp(term,"*** ",4)==0){  /* this string might be a message */
		bell();
		ncol = 1; /* red instead of black */
		message ("\nTermGraph%% %s\n", term);
		insert_node_in_Sgraph_list (pgraph, NULL, 0, 20+9*strlen(term), y,
			0,4,0,4,1,ncol,CORX- ELEMENT_LENGTH, CORY+ PLACEY+ELEMENT_LENGTH,term);
			/* this node shows readable the input term */
	} else {
		wid = VALa(p, 0, width);
		hei = VALa(p, -termgraph_basic_length, height) + termgraph_basic_length;
		if (wid * hei != 0) {
			message ("\nTermGraph%% Built Petri-Net from input term:\n           %s\n", 
				term);
			message ("TermGraph%% Graph size results:\n");
			message ("           Screen area/[pixel]: %d*%d = %d\n", wid, hei, wid * hei);
			wid = VALa(p, 0, width)/ termgraph_basic_length;
			hei = 1 + VALa(p, -termgraph_basic_length, height)/ termgraph_basic_length;
			message ("           Squares(%d pixel/side): %d*%d = %d\n",
				termgraph_basic_length, wid, hei, wid * hei);
			message ("           Nodes/squares: %d/%d = %1.3f\n\n", termgraph_nodes_sum, 
				wid * hei, (float)(termgraph_nodes_sum / (float)(wid * hei)) );
			if (termgraph_nodes_sum > 200) {
				bell();
			} /* Ende fuer den Algorithmus */
		} else {
			message ("\nTermGraph%% Correct input, but no Petri-Net from:\n           %s\n",term);
		}
			
		i = tlength = 0;
		while (term[i + tlength] != '\0') {
			if (term[i + tlength] == '.') {
				maxlength = MAX( maxlength, tlength-1 );
				i += tlength;
				tlength = 0;
			}
			tlength++;
		}
		for (i = 0, ntimes = 1; term[i] != '\0'; ntimes++) {
		
			for ( tlength=0; (term[i + tlength] != '\0') && (term[i + tlength] != '.'); tlength++ );
			if (tlength != 0) {
				strncpy (str, term + i, tlength);
			}
			i += tlength + 1 /* wegen "." */;
			while (term[i] == '.') {
				i++;
			}
			for ( ; tlength <= maxlength; tlength++ ) {
				str[tlength] = ' '; /* auffuellen */
			}
			str[tlength] = '\0';
			insert_node_in_Sgraph_list (pgraph, NULL, 0, 20+9* maxlength, y,
				0,4,0,4,1,ncol,
				CORX- ELEMENT_LENGTH, 
				CORY+ ELEMENT_LENGTH + ntimes * (y- 1/*gemeinsame Kante!*/),
				str);
				/* this node shows readable the input term */
		}
	}

	return 0;
} /* termgraph_draw_petrinet */

int graph_synchron_top_down( pgraph, p )
Sgraph pgraph;
pnode *p;
{
	if (p == NULL) {
		return 0;
	}
	if (p->name[0] == '$') {
		make_sema_construction( pgraph, p );
	}
	graph_synchron_top_down( pgraph, p->left );
	graph_synchron_top_down( pgraph, p->right );
	return 1;
} /* graph_synchron_top_down */

