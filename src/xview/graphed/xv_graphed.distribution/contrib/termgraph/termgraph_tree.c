/* (C) Universitaet Passau 1986-1991 */
#include "termgraph_decl.h"
/*************************************************************************
**									**
**	MODULNAME:	termgraph_tree.c				**
**									**
**************************************************************************
**									**
**	ERSTELLUNG:	Rudolf Seisenberger, 	1990			**
**									**
**	AUFGABEN:	Die Datei enthaelt hauptsaechlich Prozeduren, 	**
**			die Berechnungen und Modifikationen auf dem 	**
**			Syntaxbaum durchfuehren und Funktionen, die	**
**			damit in enger Verbindung stehen.		**
**									**
*************************************************************************/


/*************************************************************************
**									**
**			    EXTERNE FUNKTIONEN				**
**									**
*************************************************************************/
extern	int 		insert_edge_in_Sgraph_list();
extern	Snode 		insert_node_in_Sgraph_list();
extern	Edgeline 	termgraph_make_edgeline_angle();


/*************************************************************************
**									**
**				FUNKTIONEN				**
**									**
*************************************************************************/
pnode	*(createpnode());
pnode 	*(termgraph_parsetree());
pnode 	*(select_name_node());
pnode	*(recursion_backtrack_path());
pnode 	*prepare_a_transition_as_sema_correspondent ();
pnode 	*prepare_a_transition_after_or ();
int 	search_for_successor();
int 	find_max();
int	priority();
int	op_lenght();
int 	sema_name_char();
int	name_char();
int 	sequential_order();
int	recursiv_name();
int 	sequential_places_insert_activity();
int	max_x_in_tree();
int	min_x_in_tree();
pnode 	*(strict_annealing());
pnode	*(node_annealing());
int 	sequential_nodes_get_their_left_successors_coordinates();
int 	transition_height_set ();
int	set_flag_to_target();
int 	sequential_parallelisms ();
int	termgraph_squares();
int	op_contract();
int	add_to_x();
int	add_to_Y();
pnode	*find_critical();
int	the_two_criticals();
int 	make_sema_construction ();


/*************************************************************************
**									**
**				GLOBALE VARIABLE			**
**									**
**************************************************************************
**									**
**	Alle im Folgenden als extern angegebenen Variablen sind global	**
**	und in den termgraph_main.* definiert.				**
**									**
*************************************************************************/
extern 	int 	termgraph_basic_length;
extern 	int 	ELEMENT_LENGTH;
extern 	char	termglob_agent_name[256];
extern 	char	termglob_agent_stdin[256];

extern 	int 	CORX;
extern 	int 	CORY;
extern 	int 	termgraph_nodes_sum;
extern 	int 	termgraph_placex_length;
extern 	int 	termgraph_placey_length;
extern 	int 	termgraph_transx_length;
extern 	int 	termgraph_transy_length;

/* miscelous layout parameters: */
extern 	int 	termgraph_transition_color;
extern 	int 	termgraph_place_color;
extern 	int 	termgraph_edge_color;
extern 	int 	termgraph_transition_font;
extern 	int 	termgraph_place_font;
extern 	int 	termgraph_edge_font;
extern 	int 	termgraph_transition_nlv;
extern 	int 	termgraph_place_nlv;
extern 	int 	termgraph_edge_elv;
extern 	int 	termgraph_arrow_length;
extern 	float 	termgraph_arrow_angle;
extern 	int 	termgraph_only_defaults;
extern	int	termgraph_edgeline_type;

pnode *(termgraph_parsetree(pt, down, up))
/*** erzeugt aus dem Agenten einen Syntaxbaum mit dem Typ pnode ***/

char	*pt;
int	down, up;
{
	int	maxi, i, leng, braco;
	pnode	*top;
		
	maxi = find_max(pt, down, up);
	
	if (maxi == down) {
		if (*(pt + maxi) == '('){
			i = maxi;
			braco = 1;
			while(braco>0)
				switch(pt[++i])
				{
				case '(':	braco++; break;
				case ')':	braco--; break;
				default:	break;
				}
			top = termgraph_parsetree(pt, maxi+1, i-1);
			return (top);
		}
		else {
#ifdef DEBUG
			printf("ENTER select_name_node at %d.\n", maxi);
#endif
			top = select_name_node(pt, maxi);
			return (top);
		}
	}
	else {
		top = createpnode();
		top->activ = 1;
		top->istarget = top->isfixed = 0;
		top->left = top->right = top->pred = top->succ = NULL;
		top->Snod = NULL;
		strncpy(top->name, pt+maxi, (leng = op_lenght(pt+maxi)));
		top->name[leng] = '\0';
		strsave (top->name);
		
#ifdef DEBUG
		printf("ENTER left termgraph_parsetree() at %d.\n", maxi);
#endif
		top->left = termgraph_parsetree(pt, down, maxi-2);
		
#ifdef DEBUG
		printf("ENTER right termgraph_parsetree() at %d.\n", maxi);
#endif
		top->right = termgraph_parsetree(pt, maxi+leng, up);
		top->left->lbranch = 1;
		top->right->lbranch = 0;
		top->left->pred = top->right->pred = top;
	
		return (top);
	}
} /* termgraph_parsetree */

int find_max(pt, down, up)
/*** gibt die Position zwischen down und up als int-Wert zurueck, ***/
/*** an der sich der Operator mit der hoechsten Prioritaetsstufe befindet. ***/

char	*pt;
int	down, up;
{
	int	maxi, i, braco;
	
	maxi = i = down;
	while(i <= up){
		switch (pt[i])
		{
		case '\0':	return(maxi); /* nur bei Kontext-Inkorrektheit erreicht */
					break;
		
		case '(':	braco = 1;
				while(braco>0)
					switch(pt[++i])
					{
					case '(':	braco++; break;
					case ')':	braco--; break;
					default:	break;
					};
				break;
		
		case '/':	i++;
					if (priority(pt[maxi], pt[i]) > 0) {
						maxi = i;
					}
					break;
					
		default:	i++;
					break;
		}
	}
	return (maxi);
} /* find_max */

pnode *(select_name_node(pt, j))
/*** ermittelt den naechsten Transitionsnamen aus  dem Zeichenpuffer ab j ***/

char	pt[2 * MAXBUF];
int		j;
{
	int	i;
	pnode	*nnode;
	
	i = 0;
	while (name_char(pt[j+i]) > 0) i++;
	
	nnode = createpnode();
	nnode->activ = 1;
	nnode->istarget = nnode->isfixed = 0;
	nnode->left = nnode->right = nnode->pred = nnode->succ = NULL;
	nnode->Snod = NULL;
	if (i != 0){
		if (i<=MAXNAME)
			strncpy (nnode->name, pt+j, i);
		else {
			strncpy (nnode->name, pt+j, MAXNAME-1);
			nnode->name[MAXNAME-1] = '\\';
		}
	}
	*(nnode->name + i) = '\0';
	
	return (nnode);
} /* select_name_node */

pnode *prepare_a_transition_after_or (node)
/*** fuegt in den Syntaxbaum den Knoten fuer eine Hilfstransition ein ***/

pnode *node;
{
	pnode *pseq1;
	
	if ((node == NULL) || (node->pred == NULL)) {
		return NULL;
	}
	if ( ( node->pred->name[0] == '<' ) || (node->pred->name[0] == '%') || 
	     ( strcmp( ALTERNODE(node)->name, SEQUENCE_HELP_TRANSITION_NAME ) == 0 )
		|| (( node->pred->name[0] == ';') && (node->lbranch == 1)
		&&  (( ALTERNODE(node)->name[0] == ':' ) 
		||   ( ALTERNODE(node)->name[0] == '|' )
		||   ( ALTERNODE(node)->name[0] == '$' )
		||   (( ALTERNODE(node)->name[0] == ';' ) && ( ALTERNODE(node)->succ != NULL )
			&&  (( ALTERNODE(node)->succ->name[0] == ':' ) /* succ hier nur default */
			||   ( ALTERNODE(node)->succ->name[0] == '|' )
			||   ( ALTERNODE(node)->succ->name[0] == '$' )))
									 )) ) {
		if (node->pred->name[0] != '<') { 
			return node; /* Transition schon eingefuegt oder anderweitig erstellt */
		}
	}
	
	if (node->lbranch == 0) {
		pseq1 = createpnode();
		pseq1->right = createpnode();
		pseq1->left = node;
		node->pred->right = pseq1; /* vgl. else */
		pseq1->pred = node->pred;
		node->pred = pseq1;
		pseq1->right->pred = pseq1;
		node->lbranch = 1;
		pseq1->lbranch = 0; /* vgl. else */
		pseq1->right->lbranch = 0;
		strcpy( pseq1->right->name, strsave(SEQUENCE_HELP_TRANSITION_NAME) );
		strcpy( pseq1->name, strsave(";") );
		pseq1->activ = pseq1->right->activ = 1;
		pseq1->isfixed = pseq1->right->isfixed = 
			pseq1->istarget = pseq1->right->istarget = 0;
		pseq1->right->right = pseq1->right->left = pseq1->right->succ = NULL;
		pseq1->right->Snod = NULL;
	} else { /* node->lbranch==1 */
		pseq1 = createpnode();
		pseq1->right = createpnode();
		pseq1->left = node;
		node->pred->left = pseq1; /* vgl. then */
		pseq1->pred = node->pred;
		node->pred = pseq1;
		pseq1->right->pred = pseq1;
		/* node->lbranch = 1; stimmt bereits! */
		pseq1->lbranch = 0;
		pseq1->lbranch = 1; /* vgl. then  */
		pseq1->right->lbranch = 0;
		strcpy( pseq1->right->name, strsave(SEQUENCE_HELP_TRANSITION_NAME) );
		strcpy( pseq1->name, strsave(";") );
		pseq1->activ = pseq1->right->activ = 1;
		pseq1->isfixed = pseq1->right->isfixed = 
			pseq1->istarget = pseq1->right->istarget = 0;
		pseq1->right->right = pseq1->right->left = pseq1->right->succ = NULL;
		pseq1->right->Snod = NULL;
	}
	return pseq1;
} /* prepare_a_transition_after_or */

pnode *prepare_a_transition_as_sema_correspondent (node)
/*** fuegt Hilfsknoten fuer eine Semaphorkonstruktion in den ***/
/*** Syntaxbaum ein.					     ***/

pnode *node;
{
	pnode *pseq1, *pseq2;
	
	if ((node == NULL) || (node->pred == NULL)) {
		return NULL;
	}
	
	if (node->name[0] == '%') {
		if (node->lbranch == 0) {
			pseq1 = createpnode();
			pseq1->left = createpnode();
			pseq1->right = pseq2 = createpnode();
			pseq2->right = createpnode();
			pseq2->left = node;
			node->pred->right = pseq1; /* vgl. else */
			pseq2->pred = pseq1;
			node->pred = pseq2;
			pseq2->right->pred = pseq2;
			node->lbranch = 1;
			pseq2->lbranch = 0;
			pseq1->lbranch = 0; /* vgl. else */
			pseq2->right->lbranch = 0;
			pseq1->left->lbranch = 1;
			pseq1->left->pred = pseq1;
			strcpy( pseq2->right->name, strsave(SEQUENCE_HELP_TRANSITION_NAME) );
			strcpy( pseq2->name, strsave(";") );
			strcpy( pseq1->left->name, strsave(SEQUENCE_HELP_TRANSITION_NAME) );
			strcpy( pseq1->name, strsave(";") );
			pseq2->activ = pseq2->right->activ = 1;
			pseq2->isfixed = pseq2->right->isfixed = 
				pseq2->istarget = pseq2->right->istarget = 0;
			pseq1->activ = pseq1->left->activ = 1;
			pseq1->isfixed = pseq1->left->isfixed = 
				pseq1->istarget = pseq1->left->istarget = 0;
			pseq1->left->right = pseq1->left->left = pseq1->left->succ = NULL;
			pseq1->left->Snod = NULL;
			pseq2->right->right = pseq2->right->left = pseq2->right->succ = NULL;
			pseq2->right->Snod = NULL;
		} else { /* node->lbranch==1 */
			pseq1 = createpnode();
			pseq1->left = createpnode();
			pseq1->right = pseq2 = createpnode();
			pseq2->right = createpnode();
			pseq2->left = node;
			node->pred->left = pseq1; /* vgl. then */
			pseq2->pred = pseq1;
			node->pred = pseq2;
			pseq2->right->pred = pseq2;
			/* node->lbranch = 1; stimmt bereits! */
			pseq2->lbranch = 0;
			pseq1->lbranch = 1; /* vgl. then  */
			pseq2->right->lbranch = 0;
			pseq1->left->lbranch = 1;
			pseq1->left->pred = pseq1;
			strcpy( pseq2->right->name, strsave(SEQUENCE_HELP_TRANSITION_NAME) );
			strcpy( pseq2->name, strsave(";") );
			strcpy( pseq1->left->name, strsave(SEQUENCE_HELP_TRANSITION_NAME) );
			strcpy( pseq1->name, strsave(";") );
			pseq2->activ = pseq2->right->activ = 1;
			pseq2->isfixed = pseq2->right->isfixed = 
				pseq2->istarget = pseq2->right->istarget = 0;
			pseq1->activ = pseq1->left->activ = 1;
			pseq1->isfixed = pseq1->left->isfixed = 
				pseq1->istarget = pseq1->left->istarget = 0;
			pseq1->left->right = pseq1->left->left = pseq1->left->succ = NULL;
			pseq1->left->Snod = NULL;
			pseq2->right->right = pseq2->right->left = pseq2->right->succ = NULL;
			pseq2->right->Snod = NULL;
		}
		/*node = pseq1; geht nicht, Zuweisung an die transiente Variable node */
		return pseq1;
	}
	return node;
} /* prepare_a_transition_as_sema_correspondent */

int search_for_successor (p, succ_element)
/*** ermittelt den Knotenzeiger fuer alle record-Variablen ->succ ***/

pnode	*p, *succ_element;
{
	static pnode 	*phelp, *node1, *node2;
	static char 	name1[MAXNAME],name2[MAXNAME];
	int rflag = 0;
	
	if (p==NULL) return -1; /* fault, mustn't happen with a correct tree */
	if (p->name[0] == '.') return rflag;
	
	if (p->name[0] == '%') {	/* Aufruf vor der Fallunterscheidung */
		p = prepare_a_transition_after_or (p); 
			/* die Theorie erwartet eine nachfolgende Transition */
	}
	
	p->succ = succ_element; /* is able to be changed at once */
	
	if (p->left==NULL){		/* that implicates also: p->right==NULL */
		if ((p->pred->name[0] == ':') && (p == NAMENODE(p->pred))) {
			p->succ = p->pred;
		} else {
			p->succ = succ_element;
		}
		return rflag;
	}
	
	/* Fallunterscheidung: */
	if ( (p->name[0] == '$') && (the_two_criticals(p,name1,name2) >= 0) ){
		strict_annealing (p, p->left,'$',name1,0); /* 0=='to the right side' */
		strict_annealing (p, p->right,'$',name2,1); /* 1=='to the left side' */
		node1 = find_critical (p, name1);
		node2 = find_critical (p, name2); /* fuege hier die markierte Trans. ein, die sem bei or erlaubt. */
		prepare_a_transition_as_sema_correspondent (node1);
		prepare_a_transition_as_sema_correspondent (node2);
	}
		
	if ((p->name[0] == '|') ||
	    (p->name[0] == '$')) {
		p->succ = succ_element;
		rflag = search_for_successor (p->left, p);
		rflag = MIN( rflag, search_for_successor (p->right, p) );
		return rflag;
	}
	
	if (p->name[0] == '%') {
		p->succ = succ_element;
		rflag = search_for_successor (p->left, p);
		rflag = MIN( rflag, search_for_successor (p->right, p));
		return rflag;
	}
	if (p->name[0] == ';') {
		phelp = p->left;
		while (phelp->name[0] == ';')
			phelp = phelp->left;
		p->succ = phelp;
		rflag = search_for_successor (p->left, p);
		rflag = MIN( rflag, search_for_successor (p->right, succ_element) );
		return rflag;
	}
	if ((p->name[0] == ':') && (!(name_char(p->left->name[0]) || name_char(p->right->name[0])))) {
		message ("TermGraph%% *** There is no identificator for \":\" ***\n");
		return(-1);
	}
	if (p->name[0] == ':'){
		strict_annealing(p, ALTERNODE( NAMENODE( p ) ), ':', VAL(NAMENODE(p),name),
			VAL(NAMENODE(p),lbranch) );
		if (p->left->left == NULL) {
			if (p->right->name[0] == ';') {
				rflag = MIN(rflag, search_for_successor (p->right, succ_element));
				p->succ = p->right->succ;
				p->left->succ = p;
				p->left->activ = -1;
				recursion_backtrack_path(p->right, NAMENODE(p));
				return rflag;
			} else {
				p->succ = p->right;
				p->left->activ = -1;
			}
			recursion_backtrack_path(p->right, NAMENODE(p));
		} else {
			if (p->left->name[0] == ';') {
				rflag = MIN( rflag, search_for_successor (p->left, succ_element));
				p->succ = p->left->succ;
				p->right->succ = p;
				p->right->activ = -1;
				recursion_backtrack_path(p->left, NAMENODE(p));
				return rflag;
			} else {
				p->succ = p->left;
				p->right->activ = -1;
			}
			recursion_backtrack_path(p->left, NAMENODE(p));
		}
		
	}
	rflag = MIN( rflag, search_for_successor (p->left, succ_element));
	rflag = MIN( rflag, search_for_successor (p->right, succ_element)) ;
	
	return rflag;
}	/* search_for_successor */
	
int sequential_order(p)
/*** interne Umsortierung von Knoten mit Sequentiellem Operations-Symbol ***/

pnode *p;
{
	pnode *phelp, *pgoon;
	int return_flag;
	
	return_flag = 0;
	if (p == NULL) return -1; /* failure */
	if (p->left==NULL) return 0; /* implicates p->right==NULL, except first node "<init>" */
	
	if ((p->name[0] == ';') && (p->left->name[0] == ';')) {
		phelp = p->left->right;
		if (p->lbranch == 1) {
			p->pred->left = p->left;
			pgoon = p->pred->left;
			
			p->lbranch = 0;
		} else {
			p->pred->right = p->left;
			p->pred->right->lbranch = 0;
			pgoon = p->pred->right;
		}
		p->left->right = p;
		p->left->pred = p->pred;
		p->pred = p->left;
		p->left = phelp;
		phelp->pred = p;
		p->left->lbranch = 1;
		
		return_flag = sequential_order (pgoon);
		
	} else if ((p->name[0] == ':') && (p->pred->name[0] == ':') && 
		( strcmp(p->left->name, p->pred->left->name) == 0 )) { /* identificators */
		return_flag = -1;	/* e.g. agent "r:: r:: r" is not allowed */
	} else {
		return_flag = sequential_order(p->left);
		return_flag = sequential_order(p->right);
	}
	return return_flag;
} /* sequential_order */

int recursiv_name(p) /* connects p->succ with its recusion mark */
pnode *p;
{
	if ((p==NULL) || (p->left==NULL) || (p->right==NULL)){
		printf("ERROR(x): can't find mark for recursion!\n");
		exit (1);
	}
	if (p->left->left==NULL){
		if (p->right->left==NULL)
			message("TermGraph%% *** WARNING: Various marks for recursion found! Accepted %s. ***\n", p->left->name);
		p->succ = p->left;
	}
	else
		p->succ = p->right;
	return 0;
} /* recursiv_name */		

pnode *recursion_backtrack_path(p, father_id)
/*** findet im Syntaxbaum den Zielknoten fuer eine Schleifenkante ***/

pnode *p, *father_id;
{
	pnode *recursion_point;
	int i;
	
	if (p==NULL) return NULL;
	
	if ( strcmp (p->name, father_id->name) == 0 ) {
		p->succ = father_id->pred;
		if (p->name[0] == '.') {
			message ("TermGraph%% *** Badly used identifier '%s'. ***\n", p->name);
			sprintf ( p->name, "%s", father_id->name );
		} else {
			sprintf ( p->name, ".%s", father_id->name );
		}
		p->isfixed = 1; /*TRUE*/			
		recursion_point = p;
		
		/*strict_annealing(father_id, ALTERNODE( NAMENODE( father_id ) ), ':', NAMENODE( father_id )->name ),
			VAL(NAMENODE(father_id),lbranch) ); */
	} else {
		recursion_point = NULL;
	}
	if (recursion_point == NULL) {
		recursion_point = recursion_backtrack_path(p->left, father_id);
	}
	if (recursion_point == NULL) {
		recursion_point = recursion_backtrack_path(p->right, father_id);
	}
	return recursion_point;
} /* recursion_backtrack_path */

int op_lenght(lstart)	/* Eingabestring muss Klammerkorrekt sein ! */
/*** zaehlt die Zeichen, die zu einem Operationssymbol gehoeren ***/

char	*lstart;
{
	int i, brace1, brace2;

	if ((*(lstart+1) != '{') && (*(lstart+1) != '[')) 
		return(1);
	i = 1;
	if (lstart[1] == '{') {
		brace1 = 1;
		while (brace1 != 0) {
			switch (lstart[++i])
			{
			case '{':	brace1++; break;
			case '}':	brace1--; break;
			case '[':	brace2++; break;
			case ']':	brace2--; break;
			default:	break;
			}
		}
	} else if (lstart[1] == '[') {
		brace2 = 1;
		while (brace2 != 0) {
			switch (lstart[++i])
			{
			case '{':	brace1++; break;
			case '}':	brace1--; break;
			case '[':	brace2++; break;
			case ']':	brace2--; break;
			default:	break;
			}
		}
	}
	return (i+1);
} /* op_lenght */

int priority(a,b)
/*** bewertet, welches Operationssymbol (a oder b) schwaecher bindet ***/

char	a, b;
/* in decl.h definiert: static char opchar[] = { '$', '|', '%', ';', ':'} */
{
	int x, y;
	
	x=y=0;
	/*while((opchar+x != NULL) && (opchar[x] != a)) x++;
	while((opchar+y != NULL) && (opchar[y] != b)) y++;*/
	
	while((x < 5) && (opchar[x] != a)) x++;
	while((y < 5) && (opchar[y] != b)) y++;
	return(x-y);
} /* priority */

int show_tree(k, father)
int		k;
pnode	*father;
{
#ifdef SHOWTREE
	int		i;

	i=k;
	if (father==NULL) {
		printf("  (nil)");
		return(0);
	}

	printf("  %s (%d|%d),%d",father->name,father->x,father->y, father->activ);
	printf(",suc: %s",father->succ==NULL?"(nil)":father->succ->name);
		printf("\n[%d]left:  ", i);
		show_tree(i+1, father->left);
		printf("                [%d]right: ", i);
		show_tree(i+1, father->right);
		printf("\n                 ");
#endif
	return(1);
} /* show_tree */

int sequential_nodes_get_their_left_successors_coordinates(p)
pnode	*p;
{
	if (p == NULL) return 0;
	
	if (p->name[0] == ';') {
		if (p->right != NULL) {
			p->x = p->right->x;
			p->y = p->right->y;
		}
	}
	
	sequential_nodes_get_their_left_successors_coordinates(p->right);
	sequential_nodes_get_their_left_successors_coordinates(p->left);
	
	return 1;
} /* sequential_nodes_get_their_left_successors_coordinates */

int termgraph_squares(p, number)
/*** hier findet die hauptsaechliche Berechnung des Platzbedarfes ***/
/*** fuer alle Teile des Petrinetzes statt			  ***/

pnode	*p;
int  	*number;
{
	int high, broad, e, lx,ly, rx,ry, rflag=0;
	pnode	*hlp /*, *node1, *node2 */ ;
	/* char	name1[MAXNAME], name2[MAXNAME]; */
	
	if (p == NULL) return 0;
	
	p->cardinal = *number;	 /* current node number for GraphEd */
	(*number)++;
	termgraph_squares(p->left, number);
	termgraph_squares(p->right, number);
	
	e = ELEMENT_LENGTH; /* by pixel */
	
	lx = (p->left==NULL) ? 0 : p->left->width;
	ly = (p->left==NULL) ? 0 : p->left->height;
	rx = (p->right==NULL) ? 0 : p->right->width;
	ry = (p->right==NULL) ? 0 : p->right->height;
	
	switch(p->name[0])
	{
	case ':':	high = MAX(ly, ry);
					
			if ((p->pred->name[0] == '|') || (strncmp(p->pred->name, "/|", 2) == 0)) {
				high += e+ e;
			} else {
				high += e+ e;
			}
				
			p->height = p->y = high;
			p->width = p->x = (2*lx + 2*rx)/2;
			add_to_x (p->right, 2*lx);
			add_to_y (p, e/2); 
			break;
				
	case ';':	high = ry;
	
			if ( sequential_parallelisms(p) ) {
				p->activ = -1;
				high -= e;
			} else if (p->right->name[0] == '%') {
				add_to_y ( p->right, -(e/2) );
				high -= e/2; 
			} else if ((p->right->name[0] == ';') && (p->right->succ->name[0] == '%')) {
				high -= e;
			} else {
				high += e;
			}
					
			add_to_y ( p->left, (p->left->name[0] != '%')? high: high-e );
			p->height = high + ly;
			p->y = high;
			p->width = MAX(lx,rx);
			p->x = p->width;
			add_to_x (lx<rx? p->left: p->right, p->x - MIN(lx,rx));
			break;
				
	case '%':	high = MAX(ly, ry);
						
			if (op_contract('%',p) == 1) {
				p->activ += 1; /* that`s exact */
				high +=   MAX(e, (p->activ*e)/6);
				rflag = search_for_successor (p->left, p);
				rflag = MIN (rflag, search_for_successor (p->right, p));
				
				transition_height_set (p, high - MAX(ly, ry));
				high += high - MAX(ly, ry); /* for end of `or` */
			}				
			p->height = p->y = high;
				
			add_to_x (p->right, 2*lx);
			p->width = p->x = (2*lx + 2*rx)/2;
			break;

	case '$':	high = MAX(ly, ry);
			if (( op_contract('@',p)==1 ) && ( op_contract('@',p)==1 )) { 						p->activ += 1; /* that`s exact */
				high += e + MAX(e, (p->activ*e)/6);
				rflag = search_for_successor (p->left, p);
				rflag = MIN (rflag, search_for_successor (p->right, p));
				transition_height_set (p, high - MAX(ly, ry));
				high += high - MAX(ly, ry); /* see at `|` */
			}
			add_to_x (p->right, e+e + 2*lx);
			p->width = p->x = (e+e + 2*lx + 2*rx) / 2;
			p->height = p->y = high;
			break;

	case '|':	high = MAX(ly, ry);
			if ((op_contract('|',p)==1) && (op_contract('@',p)==1)) { /* in {0,1} */
				p->activ += 1; /* that`s exact */
				high += e + MAX(e, (p->activ*e)/6);
				rflag = search_for_successor (p->left, p);
				rflag = MIN (rflag, search_for_successor (p->right, p));
				transition_height_set (p, high - MAX(ly, ry));
				high += high - MAX(ly, ry); /* see upon */
				  /* high..: because p hasn`t any area while transition_height_set */ 
			}
			add_to_x (p->right, 2*lx);
			p->width = p->x = (2*lx + 2*rx) / 2;
			p->height = p->y = high;
			break;
				
	case '<':	
			p->height = p->right->height + e;
			if ( ((VALa(p->right, '/', name[0]) == ';') && 
				(VALa(p->right->succ, '/', name[0]) == '%')) ||
				(VALa(p->right, '/', name[0]) == '%') ) {
				p->height -= e+e;   /* '%' und '<' fallen zusammen */
			};
			p->y = 0;
			p->width = p->x = rx;
			break;
				
	case'.':	p->width = p->x = (2*lx + 2*ly)/2 +e;
			p->height = p->y = MAX(lx, ly) +e/2;
			break;
	
	default:	if (strcmp(p->name, AGENT_SKIP_NAME) == 0) {
				high = broad = 0;
			} else {
				switch ( VALa(p->succ, ' ', name[0]) ){
				case'$':
				case'|':	high = e ;
						if ((p->succ->succ->name[0] == '%') 
							&& (p->succ->succ->activ <= 0)
							&& (p->succ->succ->succ->name[0] == '%')) {
							high += e;
						} else if ((p->succ->succ->name[0] == '%') 
							&& (p->succ->succ->succ->name[0] == '<')) {
							high -= e; 
						}
						broad = e;
						break;
					
				case'%':	high = e;
						if (p->succ->succ->name[0] == '%') {
							/*high += e;*/
						} else if (p->succ->succ->name[0] == '<') {
							high += e +e/2;
						} else if (p->succ->succ->name[0] == ';') {
							/* high -= e/2; */
						} 
						broad = e;
						break;
					
				case';':	high = e;
						broad = e;
						break;
					
				default:	high = broad = e;
						break;
				}
			}
			p->width = broad;
			p->x = broad;
			p->height = high;
			p->y = high;
	}
	return rflag;
} /* termgraph_squares */

int op_contract(op,p)
/*** markiert gleiche Operationen, die zeichnerisch zusammengefasst werden ***/

char	op;
pnode	*p;
{
	if (p==NULL) return 0; /* failure */
	if (p->pred==NULL) return 0;

	if (p->pred->name[0] == op) {
		p->pred->activ += p->activ;
		p->activ = 0;
		p->name[0] = '/';
		p->name[1] = op;
		
		return 0;
	}
	return 1;
} /* or_contract */

int set_flag_to_target(p)
/*** untersucht und vermerkt, ob ein Knoten aus dem Syntaxbaum im ***/
/*** Petrinetz ueberhaupt ein Knotenaequivalent erhalten soll     ***/

pnode *p;
{ 
	pnode	*hlp, *nod;
	
	if (p == NULL) {
		return 0;
	}
	if (p->right != NULL) {  /* not a terminal node */
		set_flag_to_target(p->left);
		set_flag_to_target(p->right);
	}
	if (p->succ != NULL) {
		switch (p->name[0])
		{
		case '<':
		case ';':
		case '|':
		case '$':
		case '%':
		case ':':
			break;
		default :
			hlp = p->succ;
			nod = p;
			while ((hlp != NULL) && (hlp->name[0] != ':')
				&& ((nod != NULL) && (strcmp(nod->name, AGENT_SKIP_NAME) != 0))
				&& ((hlp->name[0] == '|') ||
				    (hlp->name[0] == '$') ||
				    (hlp->name[0] == '<') ||
				    (hlp->name[0] == '%'))     ) {
				hlp->istarget = 1; /* TRUE */
				nod = hlp;
				hlp = hlp->succ;
			}
			break;
		} /*switch*/
	}
	return 1;
} /* set_flag_to_target */
	
int transition_height_set (p, delta)
pnode *p;
int delta;
{
	return add_to_y (p, delta);
} /* transition_height_set */

int sequential_parallelisms (p)
pnode *p;
{	
	if ((p == NULL) || (p->name[0] != ';')) {
		return 0;
	} /* but now: */
	if ( ((p->left->name[0] == '$') || (p->left->name[0] == '|'))
		&& ((p->right->name[0] == '$') || (p->right->name[0] == '|')) ) {
		return 1;
	} /* or if.. */
	if ( ((p->left->name[0] == '$') || (p->left->name[0] == '|'))
		&& (p->right->name[0] == ';')
		&& ((p->right->succ->name[0] == '$') || (p->right->succ->name[0] == '|')) ) {
		return 1;
	}
	return 0;
} /* sequential_parallelisms */

pnode *(node_annealing(p, op, x, direction))
/*** fuehrt semantisch aequivalente Knotenvertauschungen durch ***/

pnode	*p;
char 	op, *x;
int 	direction;
{
	pnode *hlp, *hlp1, *hlp2;
	
	if (p==NULL) return NULL;
	if ((strcmp(p->name,x) == 0) || ((strcmp(p->name+1,x)==0) && (p->name[0]=='.'))) {
		return p;
	}
	
	hlp = (hlp = node_annealing(p->left,op,x,direction))==NULL ?
		node_annealing(p->right,op,x,direction): hlp;
		
	if ((hlp == NULL) || (hlp->pred == NULL)) {
		 return NULL;
	}
	
	hlp2 = hlp;
	if ((hlp->lbranch != direction) && (hlp->pred->isfixed == 0/*FALSE*/)) {

		while ((hlp->pred != NULL) && (hlp->pred->name[0] == ';') 
			&& (hlp->pred->name[0] != op)) {
			hlp = hlp->pred;
		}
		
		/* Nochmaliges Abfragen, da hlp neu sein kann */
		if ((hlp->pred != NULL) && (hlp->pred->isfixed == 0) &&
			(hlp->pred->name[0] != op)) {
			/* Annaeherung durch Vertauschen mit dem "Geschwisterknoten" */
			hlp1 = hlp->pred->left;
			hlp->pred->left = hlp->pred->right;
			hlp->pred->left->lbranch = 1;	/* "linken" bzw. "rechten" Zweig benennen */
			hlp->pred->right = hlp1;
			hlp->pred->right->lbranch = 0;
		} else {
			return NULL;
		}
	}
	if (hlp2->pred != NULL) {
		hlp2->pred->isfixed = TRUE;
	}
#ifdef SHOW
	printf("%s MOVING TO DIRECTION %d AT NODE NAME %s\n",x,direction, hlp->name);
#endif
	return p;
} /* node_annealing */

pnode *(strict_annealing(origo, p, op, x, direction))
/*** startet node_annealing ***/

pnode	*origo, *p;
char 	op, *x;
int 	direction;
{
	pnode *hlp, *xnode;

	if ( (xnode = node_annealing (p, op, x, direction)) != NULL) {
		/* Fixieren des Pfades vom Annaeherungsknoten hlp zum Ausgangsknoten origo */
		/*for ( xnode = hlp = find_critical( p, x ); 
			(hlp != NULL) && (hlp->pred != NULL) && (hlp != origo); hlp = hlp->pred ) {
			
			hlp->isfixed = TRUE;
		} stopped loop */
	}
	return xnode;
} /* strict_annealing */

int add_to_x(p, delta)
/*** expandiert den Flaechenbedarf im Baum unter p horizontal um delta ***/

pnode	*p;
int		delta;
{
	if (p==NULL) return 0;
	p->x += delta;
	p->width += delta;
	add_to_x(p->left, delta);
	add_to_x(p->right, delta);
	return 1;
} /* add_to_x */

int add_to_y(p, delta)
/*** expandiert den Flaechenbedarf im Baum unter p vertikal um delta ***/

pnode	*p;
int		delta;
{
	if (p==NULL) return 0;
	p->y += delta;
	p->height += delta;
	add_to_y(p->left, delta);
	add_to_y(p->right, delta);
	return 1;
} /* add_to_y */

int max_x_in_tree( p, stop_here )
/*** misst die "breiteste" Stelle im Petrinetz zwischen p und stop_here ***/
/*** und gibt den rechten Rand davon an 				***/

pnode *p;
char *stop_here;
{
	int maxi=0; /* never used 0 */
	
	if (p == NULL) return 0;
	
	if (strncmp(p->name, stop_here, strlen(stop_here)) == 0) {
		if (p->name[0] == ';') {
			return max_x_in_tree(p->left, stop_here);
		} else {
			return p->x;
		}
	}
	
	if ((p->name[0] == ';') || (p->name[0] == ':')) {
		maxi = MAX( max_x_in_tree(p->left, stop_here), max_x_in_tree(p->right, stop_here) );
		maxi = MAX( maxi, p->x );
	} else {
		maxi = MAX( p->x, max_x_in_tree(p->right, stop_here) );
	}
	return maxi;
} /* max_x_in_tree */

int min_x_in_tree( p, stop_here )
/*** misst die "breiteste" Stelle im Petrinetz zwischen p und stop_here ***/
/*** und gibt den linken Rand als int-Wert zurueck			***/

pnode *p;
char *stop_here;
{
	int mini=10000; /* never used 10000 */
	
	if (p == NULL) return mini; /* maximum window width (=4000) and more */
	
	if (strncmp(p->name, stop_here, strlen(stop_here)) == 0) {
		if (p->name[0] == ';') {
			return min_x_in_tree(p->left, stop_here);
		} else {
			return p->x;
		}
	}
	
	if ((p->name[0] == ';') || (p->name[0] == ':')) {
		mini = MIN( min_x_in_tree(p->left, stop_here), min_x_in_tree(p->right, stop_here) );
		mini = MIN( mini, p->x );
	} else {
		mini = MIN( p->x, min_x_in_tree(p->left, stop_here) );
	}
	return mini;
} /* min_x_in_tree */

int sema_name_char(c)
char	c;
{
	return (( (name_char(c)) || c=='{' || c=='}' || c==','
	|| c=='|' || c=='%' || c=='$' || c==':' || c=='.' || c==';' || c=='<') ? 1 : 0);
} /* sema_name_char */


int name_char(c)
/*** prueft, ob das Zeichen c in dem Alphabet fuer Aktionennamen enthalten ist ***/

char	c;
{
	return (((c>='a' && c<='z') || (c>='A' && c<='Z') || (c>='0' && c<='9') || c=='@'
	|| c=='_' || c=='-' || c=='\"' || c=='\'' || c=='[' || c==']' || c=='#') ? 1 : 0);
} /* name_char */

pnode *createpnode()
{
return((pnode *) malloc (sizeof(struct pnodestruct)));
} /* createnode */


pnode *(find_critical(p,x))
/*** sucht einen Knoten mit dem label x ***/

pnode 	*p;
char 	x[MAXNAME];
{
	pnode *phelp;
	
	if (p==NULL) return NULL;
	
	if (strcmp(x, p->name) == 0) return p;  /* vergleiche Namen */
	
	phelp = (((phelp = find_critical(p->left,x))==NULL)?
		phelp = find_critical(p->right,x): phelp);
		
	return phelp;
} /* find_critical */

int the_two_criticals(synchron_node,name1,name2)
/*** analysiert anhand der Semaphoroperation, welche zwei Knoten ***/
/*** aus kritischen Bereichen stammen 				 ***/

pnode	*synchron_node;
char	name1[MAXNAME], name2[MAXNAME];
{
	int i,j,k, end, brace;
	char n1[MAXNAME],n2[MAXNAME];
	
	if (name_char(synchron_node->name[0])) return -1;
	if (strcmp (synchron_node->name, "$") == 0) return -1;
	if ((synchron_node->name+1==NULL) || (synchron_node->name[1]!='{')) return -1;
	
	/* find the first comma: */
	brace = 1;
	for ( end = 2; (end <= strlen(synchron_node->name)) && (brace > 0); end++ ) {
		switch (synchron_node->name[end])
		{
		case '{': brace++; 
			  break;
		
		case '}': brace--;
			  break;
			  
		case ',': if ( brace == 1 ) {
				brace = 0;
			  }
			  break;
			  
		default:  break;
		}
		n1[end - 2] = synchron_node->name[end];
	}
	end--;
	n1[end - 2] = '\0';	/* overwrite the comma */
	i = end + 1;
	
	if ( (end >= strlen(synchron_node->name)) ){ 
		/* there is only one name */
		strncpy( n2, n1, strlen(n1) );
		return 0;
	}
	
	for ( end = strlen(synchron_node->name); (end > 0) && (synchron_node->name[end] != '}'); end-- );
		/* end shows the last bracket */
	for( j = i; (i < end) ;i++) {
		n2[i-j] = synchron_node->name[i];
	}
	n2[i-j] = '\0';
	strcpy( name1, n1 );
	strcpy( name2, n2 );
	return 1;
}   /* the_two_criticals */
		
int make_sema_construction (pgraph, synchron_node)
/*** berechnet und zeichnet eine Semaphor-Konstruktion ***/

Sgraph	pgraph;
pnode	*synchron_node;
{
	pnode		*node1, *node2, *helpnode;
	char 		name1[MAXNAME], name2[MAXNAME], helpname[MAXNAME],
		 		np[MAXBUF], *node_name;
	int  		i, j,sx, sy, sxlup, sxldo, sxrup, sxrdo, xspace, yspace;
	int		node1_y, node1_height, node2_y, node2_height;
	int  		nsx,nsy,nti,nfi,nlp,nei,nlv,ncol,ecol,npy,npx,eti,efi,elv,al;
	float		aa;
	Snode		num_Snod;
	Edgeline	el, first_el;
	
	if (synchron_node == NULL) return -1;
	
	if ( the_two_criticals (synchron_node, name1, name2) != -1 ) {
		node1 = find_critical (synchron_node->left, name1);
		node2 = find_critical (synchron_node->right, name2);
	} else {
		node1 = synchron_node->left;
		node2 = synchron_node->right;
	} 
	if ((node1 != NULL) && (node2 != NULL) && (node1->x > node2->x)) {
		helpnode = node2;
		node2 = node1;
		node1 = helpnode;
		sprintf (helpname, "%s", name2);
		sprintf (name2, "%s", name1);
		sprintf (name1, "%s", helpname);
	}	
	/* Im Folgenden wird davon ausgegangen, dass node1 links und node2 rechts */
	/* von synchron_node liegt; daher werden ggfs. diese Knoten vertauscht.   */
		
	sxlup = max_x_in_tree (synchron_node->left, name1);	/* Suche bis Knotenname name1 */
	sxldo = max_x_in_tree (synchron_node->left, "/"); 	/* "/": Suche bis zum Baumende */
	sxrup = min_x_in_tree (synchron_node->right, name2);
	sxrdo = min_x_in_tree (synchron_node->right, "/");
	
	if (node1 == NULL) {
		node1_height = synchron_node->height;
		node1_y = synchron_node->y;
	} else if (node1->name[0] == '%') { /* '%' bekam umklammernde Hilfstransitionen */
		node1_height = (node1->pred!=NULL)&&(node1->pred->pred!=NULL) ? 
			node1->pred->pred->left->y : node1->height;
		node1_y = node1->pred!=NULL ?
			node1->pred->right->height : node1->y;
	} else {
		node1_height = node1->height;
		node1_y = node1->y;
	}
	if (node2 == NULL) {
		node2_height = synchron_node->height;
		node2_y = synchron_node->y;
	} else if (node2->name[0] == '%') { /* '%' bekam umklammernde Hilfstransitionen */
		node2_height = (node2->pred != NULL) && (node2->pred->pred != NULL) ? 
			node2->pred->pred->left->y : node2->height;
		node2_y = node2->pred != NULL ?
			node2->pred->right->height : node2->y;
	} else {
		node2_height = node2->height;
		node2_y = node2->y;
	}

	sx = ABS( sxldo - sxrdo )/2 + sxldo; 
	sy = MIN( MAX( node1_height, node2_height ) + 
		     ABS( VALa(node1,synchron_node->x,x) - VALa(node2,synchron_node->x,x) ),
		synchron_node->height - ELEMENT_LENGTH);
	
	xspace = MAX( PLACEX, TRANSX );
	yspace = MAX( PLACEY, TRANSY );

	if ((node1 != NULL) || (node2 != NULL)) {
		num_Snod = insert_node_in_Sgraph_list (pgraph, NULL, 0,
			P_SEMA_NSX,P_SEMA_NSY,P_NTI,0,P_NLP,P_NEI,T_NLV/*visibel*/,P_NCOL,
			CORX+ sx,
			CORY- sy,
			SEMA_NAME);
		
		el = termgraph_make_edgeline_angle ('s', synchron_node->x, synchron_node->height, 0,0, sx, sy);			/* 'S' statt 's' faellt steiler ab. */
		synchron_node->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV, T_NCOL, 
			CORX+ synchron_node->x, 
			CORY- synchron_node->height, 
			THETA_HELP_NAME);
		insert_edge_in_Sgraph_list ( synchron_node->Snod, num_Snod, 
			0,0, P_ECOL,2/* dotted */,P_EFI,P_ELV,P_AL,P_AA,el,np," ");
	} else {
		message ("TermGraph%% Cannot make semaphor construct between\n");
		message ("           label %s and label %s.\n", name1, name2);
	}
		
	if ((node1 != NULL) && /*(node1->name[0] != '%') &&*/ (node1->name[0] != ';')) {
	
	    	/* Linie vom Sema-Knoten weg: */
		first_el = new_edgeline (CORX+sx, CORY-sy);
		el = add_to_edgeline (first_el, 
					CORX+ MAX( sxlup + xspace, sx - xspace),
					CORY- (sy - P_SEMA_NSY/2) );
		if (ABS(sy - node1_y) > 2*ELEMENT_LENGTH) {			
			el = add_to_edgeline (el,
					CORX+ MIN( sxlup + xspace, sx - xspace), 
					CORY- (MIN(node1_height + yspace, sy - P_SEMA_NSY/2 - yspace)) );
		}
		el = add_to_edgeline (el,
					CORX+ node1->x, 
					CORY- node1_height);
		node1->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
			T_NCOL,
			CORX+ node1->x,
			CORY- node1_height, 
			THETA_HELP_NAME);
		insert_edge_in_Sgraph_list (num_Snod, node1->Snod,
			0, 0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,0.5, first_el,np, " ");
			
		/* Linie zurueck: */
		node1->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
			T_NCOL,
			CORX+ node1->x,
			CORY- node1_y, 
			THETA_HELP_NAME);
		first_el = new_edgeline (CORX+ node1->x, CORY- node1_y);
		if (termgraph_edgeline_type != 0) {
			el = add_to_edgeline (first_el,
					CORX+ node1->x + ABS((sx - P_SEMA_NSY) - node1->x) /5,
					CORY- (node1_y - 4*ELEMENT_LENGTH/10) );
			el = add_to_edgeline (el,
					CORX+ node1->x + ABS((sx - P_SEMA_NSY) - node1->x) /2,
					CORY- (node1_y - ELEMENT_LENGTH/2) );
			el = add_to_edgeline (el,
					CORX+ node1->x + 4*ABS((sx - P_SEMA_NSY) - node1->x)/5,
					CORY- (node1_y - 4*ELEMENT_LENGTH/10) );
		} else {
			el = add_to_edgeline (first_el,
					CORX+ node1->x + ABS((sx - P_SEMA_NSY) - node1->x) /2,
					CORY- (node1_y - ELEMENT_LENGTH/2) );
		}
		el = add_to_edgeline (el,
					CORX+ MAX( sx - xspace /2, sxldo + xspace + xspace/4), 
					CORY- node1_y );
		if (ABS(sy - node1_y) > 2*ELEMENT_LENGTH) {
			el = add_to_edgeline (el,
					CORX+ MAX( sx - xspace /2, sxldo + xspace + xspace/4), 
					CORY- MAX( sy - P_SEMA_NSY, node1_y ));
		}
		el = add_to_edgeline (el,
					CORX+ sx, 
					CORY- sy);
					
		insert_edge_in_Sgraph_list (node1->Snod, num_Snod,
			0, 0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,0.5, first_el,np, " ");
	} else {
		message ("TermGraph%% Critical label %s not reached.\n", name1);
	}
	
 	if ((node2 != NULL) && /*(node2->name[0] != '%') &&*/ (node2->name[0] != ';')) {
 	
 	   	/* Linie vom Sema-Knoten weg: */
		first_el = new_edgeline (CORX+ sx, CORY- sy);
		el = add_to_edgeline (first_el, 
					CORX+ MIN( sxrup - xspace, sx + xspace),
					CORY- (sy - P_SEMA_NSY/2) );
		if (ABS(sy - node2_y) > 2*ELEMENT_LENGTH) {
			el = add_to_edgeline (el,
					CORX+ MAX( sxrup - xspace, sx + xspace), 
					CORY- (MIN(node2_height + yspace, sy - P_SEMA_NSY/2 - yspace)) );
		}
		el = add_to_edgeline (el,	
					CORX+ node2->x, 
					CORY- node2_height);
		
		node2->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
			T_NCOL,
			CORX+ node2->x,
			CORY- node2_height, 
			THETA_HELP_NAME);
		insert_edge_in_Sgraph_list (num_Snod, node2->Snod,
			0, 0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,0.5, first_el,np, " ");
	
		/* Linie zurueck: */
		node2->Snod = insert_node_in_Sgraph_list(pgraph, NULL,
			0,THETA_HELP_NSX,THETA_HELP_NSY,T_NTI,T_NFI,T_NLP,T_NEI,P_NLV/*!*/,
			T_NCOL,
			CORX+ node2->x,
			CORY- node2_y, 
			THETA_HELP_NAME);
		first_el = new_edgeline (CORX+ node2->x, CORY- node2_y);
		if (termgraph_edgeline_type != 0) {
			el = add_to_edgeline (first_el,
					CORX+ node2->x - ABS((sx + P_SEMA_NSY) - node2->x)/5,
					CORY- (node2_y - 4*ELEMENT_LENGTH/10) );
			el = add_to_edgeline (el,
					CORX+ node2->x - ABS((sx + P_SEMA_NSY) - node2->x)/2,
					CORY- (node2_y - ELEMENT_LENGTH/2) );
			el = add_to_edgeline (el,
					CORX+ node2->x - 4*ABS((sx + P_SEMA_NSY) - node2->x)/5,
					CORY- (node2_y - 4*ELEMENT_LENGTH/10) );
		} else {
			el = add_to_edgeline (first_el,
					CORX+ node2->x - ABS((sx + P_SEMA_NSY) - node2->x)/2,
					CORY- (node2_y - ELEMENT_LENGTH/2) );
		}
		el = add_to_edgeline (el,
					CORX+ MIN(sx + xspace /2, sxrdo - xspace - xspace/4), 
					CORY- node2_y );
		if (ABS(sy - node2_y) > 2*ELEMENT_LENGTH) {
			el = add_to_edgeline (el,
					CORX+ MIN(sx + xspace /2, sxrdo - xspace - xspace/4), 
					CORY- MAX( sy - P_SEMA_NSY, node2_y ));
		}
		el = add_to_edgeline (el,
					CORX+sx, 
					CORY-sy);
		insert_edge_in_Sgraph_list (node2->Snod, num_Snod,
			0,0, P_ECOL,P_ETI,P_EFI,P_ELV,P_AL,0.5, first_el,np, " ");	
	} else {
		message ("TermGraph%% Critical label %s not reached.\n", name2);
	}
	
	return 0;
} /* make_sema_construction */
