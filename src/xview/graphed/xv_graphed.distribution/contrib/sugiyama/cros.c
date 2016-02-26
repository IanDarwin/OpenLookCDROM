/* (C) Universitaet Passau 1986-1991 */
/*********************************************************************************/
/*                                                                               */
/*                            C  R  O  S  .  C                                   */
/*                                                                               */
/*                         Version vom 13.11.1989                                */
/*                                                                               */
/*********************************************************************************/

#include "def.h"

#define IT1 sugiyama_settings.it1
#define IT2 sugiyama_settings.it2

/* typedef int Matrix[SIZE][SIZE]; */

static	int	matrix_size, size;

#define ROW_SIZE size
#define COLUMN_SIZE size
#define MATRIX_SIZE matrix_size
/* typedef int Matrix[COLUMN_SIZE][ROW_SIZE]; /* MH 3/10/91 */
typedef int	*Matrix;

static check(i,limit) { if(i>=0 && i<limit) ; else printf("i : %d, limit : %d\n", i, limit); return 1;  }
#define mget(m,i,j) (*((m)+ROW_SIZE*(i)+(j)))
#define marrget(marr,i) ((marr)+MATRIX_SIZE*(i))

/***************************************************/
/* allgemeine Hilfsfunktionen                      */
/***************************************************/

Local void make_matrix(g,l,m)
Sgraph g;
int l;
Matrix m;

/* erzeugt die Adjazenzmatrix zum Level l des Graphen g */

{
	Snode n;
	Sedge e;
	int i,j;
	
	/* Knotennummern speichern */
	for_all_nodes (g,n)
		if (level(n) == l)
		{
			i = n->x;
			mget (m,i,0) = n->nr;
		}
		else if (level(n) == l + 1)
		{
			j = n->x;
			mget (m,0,j) = n->nr;
		}
	end_for_all_nodes (g,n);

	/* Adjazenzmatrix erstellen */
	for_all_nodes (g,n)
		if (level(n) == l)
		 	for_sourcelist(n,e)
				i = n->x;
				j = e->tnode->x;
				mget (m,i,j) = 1;
			end_for_sourcelist(n,e);
	end_for_all_nodes(g,n);
}


Local Snode find_node_with_number(g,nr)
Sgraph g;
int nr;
{
	return(make_node_with_number(g,make_attr(ATTR_FLAGS,0),nr));
}


Local void store_new_positions(m,l,g)
Matrix m;
int l;
Sgraph g;

/* speichert die neu berechneten Positionen der Knoten der Levels l und l+1 im Graph */

{	
	Snode n;
	int i;
	for (i=1; i<=nodes_of_level[l]; i++)
	{
		n = find_node_with_number(g,mget(m,i,0));
		n->x = i;
	}
	for (i=1; i<=nodes_of_level[l+1]; i++)
	{
		n = find_node_with_number(g,mget(m,0,i));
		n->x = i;
	}
}


Local bool sorted(vector,max)
double *vector;
int max;

/* prueft, ob die Elemente vector[1],...,vector[max] in aufsteigender Reihenfolge sortiert sind */
/* Elemente mit dem Wert 0.0 bleiben unberuecksichtigt */

{
	int i;
	
	for (i=1; i<max; i++)
		if ((vector[i] > vector [i+1]) && (vector[i+1]!= 0.0))			
			return(FALSE);
	return(TRUE);
}


Local void copy_m(m1,m2)
Matrix m1,m2;

/* kopiert m1 nach m2 */

{
	int i,j;
	(void)memcpy (m2,m1, MATRIX_SIZE*sizeof(int));			/* !!!!! */
}


Local bool equal_m(m1,m2,r,c)
Matrix m1,m2;

/* prueft, ob m1 und m2 bis zur Zeile r und zur Spalte c uebereinstimmen */

{
	register int i,j;

	for (i=0; i<= r; i++)
	    for (j=0; j<=c; j++)
		if (mget(m1,i,j) != mget(m2,i,j))
		    return(FALSE);
	return(TRUE);
}


Local double row_barycenter(m,i,max)
Matrix m;
int i,max;

/* Barycenter der i-ten Zeile der Matrix m bis zum Index max */

{
	int j;
	int r1 = 0;
	int r2 = 0;
	
	for (j=1; j<=max; j++)
	{
		if (mget (m,i,j) == 1)
		{
			r1 += j;
			r2 ++;
		}
	}
	if (r2 == 0)
		return(0.0);
	else
		return((double)r1/(double)r2);
}


Local double column_barycenter(m,j,max)
Matrix m;
int j, max;

/* Barycenter der j-ten Spalte der Matrix m bis zum Index max */

{
	int i;
	int r1 = 0;
	int r2 = 0;
	
	for (i=1; i<=max; i++)
	{
		if (mget (m,i,j) == 1)
		{
			r1 += i;
			r2 ++;
		}
	}
	if (r2 == 0)
		return(0.0);
	else
		return((double)r1/(double)r2);
}


Local int number_of_crossings(m,r,c)					/* !!!!!! */
Matrix m;
register int r,c;
{
	register int alpha, beta, j, k;
	register int result = 0;
	register int *m_k_alpha,
	             *m_j_beta,
	             *m_k, *m_j;
	             
/* Original Code by E.Preiss
	for (j=1; j<=r-1; j++)
		for (k=j+1; k<=r; k++)
			for (alpha=1; alpha<=c-1; alpha++)
				for (beta=alpha+1; beta<=c; beta++)
					result += (m[j][beta] * m[k][alpha]);
*/
/*
	for (j=1; j<=r-1; j++)
	    for (k=j+1; k<=r; k++)
		for (alpha=1; alpha<=c-1; alpha++)
		    if (m[k,alpha]) {
			for (beta=alpha+1; beta<=c; beta++)
			    if (m[j][beta]) {
				result ++;
			    }
		    }
*/
	for (j=1; j<=r-1; j++) {
	    m_j = (int*)m + j*ROW_SIZE;
	    
	    for (k=j+1; k<=r; k++) {
		m_k = (int*)m + k*ROW_SIZE;
		
		for (alpha=1; alpha<=c-1; alpha++) {
		    m_k_alpha = m_k + alpha;
		    
		    if (*(m_k_alpha)) {

			for (beta=alpha+1; beta<=c; beta++) {
			    m_j_beta = m_j + beta;
			    
			    if (*(m_j_beta)) {
				result ++;
			    }
			}
		    }
		}
	    }
	}

	return(result);
}


Local void exch_double(x1,x2)
double *x1, *x2;

/* vertauscht den Inhalt von x1 und x2 */

{
	double h;
	h = *x1;
	*x1 = *x2;
	*x2 = h;
}


Local void exch_rows(m,r1,r2)
Matrix m;
int r1, r2;

/* vertauscht die Zeilen r1 und r2 der Matrix m */

{
	register int j;
	register int h;
	register int *m1 = ((int*)m+ROW_SIZE*r1);	
	register int *m2 = ((int*)m+ROW_SIZE*r2);	
	
	for (j=0; j<ROW_SIZE; j++)
	{
/*
		h = m[r1][j];
		m[r1][j] = m[r2][j]; 
		m[r2][j] = h;
*/
		h = *m1;
		*m1 = *m2;
		*m2 = h;
		m1++; m2++;
	}
}


Local void exch_columns(m,c1,c2)
Matrix m;
int c1, c2;

/* vertauscht die Spalten c1 und c2 der Matrix m */

{
	register int i;
	register int h;
	register int *m1 = ((int*)m+c1);	
	register int *m2 = ((int*)m+c2);	
	
	for (i=0; i<COLUMN_SIZE; i++)
	{
/*
		h = m[i][c1];
		m[i][c1] = m[i][c2];
		m[i][c2] = h;
*/
		h = *m1; *m1 = *m2; *m2 = h;
		m1 += ROW_SIZE;
		m2 += ROW_SIZE;
	}
}


Local void reverse_r(m,r1,r2)
Matrix m;
int r1, r2;

/* kehrt die Reihenfolge der Zeilen r1 bis r2 in m um */

{
	int i,j;
	for (i=r1, j=r2; i<j; i++, j--)
		exch_rows(m,i,j);
}


Local void reverse_c(m,c1,c2,max_r)
Matrix m;
int c1, c2, max_r;

/* kehrt die Reihenfolge der Spalten c1 bis c2 in m um */

{
	int i,j;
	for (i=c1, j=c2; i<j; i++, j--)
		exch_columns(m,i,j);
}


Local void r_r(m1, m2, max_r, max_c)
Matrix m1, m2;
int max_r, max_c;
{
	double b[SIZE];
	int i,j;
	
	/*  Zeilenbarycenter berechnen */
	for (i=1; i<=max_r; i++)
		b[i] = row_barycenter(m1,i,max_c);
	
	/* Reihenfolge der Zeilen mit gleichem Barycenter umkehren */
	for (i=1; i<max_r; i++)
	{
		j=i;
		while ((j<max_r) && (b[j+1] == b[j]))
			j++;
		if (j>i)
		{
			reverse_r(m1,i,j);
			if (m2 != nil)
				reverse_c(m2,i,j);
			i = j;
		}
	}
}


Local void r_c(m1, m2, max_r, max_c)
Matrix m1, m2;
int max_r, max_c;
{
	double b[SIZE];
	int i,j;
	
	/*  Spaltenbarycenter berechnen */
	for (i=1; i<=max_c; i++)
		b[i] = column_barycenter(m1,i,max_c);

	/* Reihenfolge der Spalten mit gleichem Barycenter umkehren */
	for (i=1; i<max_c; i++)
	{
		j=i;
		while ((j<max_c) && (b[j+1] == b[j]))
			j++;
		if (j>i)
		{
			reverse_c(m1,i,j);
			if (m2 != nil)
				reverse_r(m2,i,j);
			i = j;
		}
	}
}


Local void b_r(m1,m2,max_r,max_c)
Matrix m1, m2;
int max_r, max_c;
{
	double b[SIZE];
	int i, j, k;

	/* Zeilenbarycenter berechnen */
	
	for (i=1; i<=max_r; i++)
		b[i] = row_barycenter(m1,i,max_c);
	
	/* Zeilen nach Barycenter ordnen */
	/* Nullzeilen bleiben stehen     */
	
	for ( j = max_r; j > 1; j--)
		if (b[j] != 0.0)						
			for (i = 1; i < j; i++)
				if (b[i] != 0.0)
				{
					k = i+1;
					while (b[k] == 0.0)
						k++;
					if (b[i] > b[k])
					{
						exch_double(&b[i], &b[k]);
						exch_rows(m1,i,k);
						if (m2 != nil)
							exch_columns(m2,i,k);
					}
				}
}						


Local void b_c(m1,m2,max_r,max_c)
Matrix m1, m2;
int max_r, max_c;
{
	double b[SIZE];
	int i, j, k;

	/* Spaltenbarycenter berechnen */

	for (i=1; i<=max_c; i++)
		b[i] = column_barycenter(m1,i,max_r);

	/* Spalten nach Barycenter ordnen */
	/* Nullspalten bleiben stehen     */
		
	for (j = max_c ; j > 1; j--)
		if (b[j] != 0.0) 
			for (i = 1; i < j ; i++)
				if (b[i] != 0.0)
				{
					k = i+1; 
					while (b[k] == 0.0) 
						k++;
					if (b[i] > b[k])
						{
						exch_double(&b[i], &b[k]);
						exch_columns(m1,i,k);
						if (m2 != nil)
							exch_rows(m2,i,k);									}
				}
}

/*********************************************/
/* Algorithmus fuer einen Graph mit 2 Levels */
/*********************************************/

Local void bc_2(g)
Sgraph g;
{
	Matrix m  = (Matrix)malloc(MATRIX_SIZE*sizeof(int)),
	       m0 = (Matrix)malloc(MATRIX_SIZE*sizeof(int)),
	       ms = (Matrix)malloc(MATRIX_SIZE*sizeof(int));
	       
	int k, ks;
	double b[SIZE];

	make_matrix(g,0,m);
	/* S t e p 1 */
	copy_m(m,ms);
	ks = number_of_crossings(m,nodes_of_level[0],nodes_of_level[1]);
	while TRUE
	{
		do {
			copy_m(m,m0);
			/* S t e p  2 */
			b_r(m,nil,nodes_of_level[0],nodes_of_level[1]);
			/* S t e p  3 */
			k = number_of_crossings(m,nodes_of_level[0],nodes_of_level[1]);
			if (k < ks)
			{	
				copy_m(m,ms);
				ks = k;
			}
			/* S t e p  4 */
			b_c(m,nil,nodes_of_level[0],nodes_of_level[1]);
			/* S t e p  5 */
			k = number_of_crossings(m,nodes_of_level[0],nodes_of_level[1]);
			if (k < ks)
			{	
				copy_m(m,ms);
				ks = k;
			}
			/* S t e p  6 (= Schleifenende) */
		} while (! equal_m(m0,m,nodes_of_level[0],nodes_of_level[1]));
		/* S t e p  7 */
		r_r(m,nil,nodes_of_level[0],nodes_of_level[1]);
		/* S t e p  8 */
		{	int i;
			for (i=1; i<=nodes_of_level[1]; i++)
				b[i] = column_barycenter(m,i,nodes_of_level[0]);
		}
		if (! sorted(b,nodes_of_level[1]))
		{
			copy_m(m,m0);
			continue;
		}
		/* S t e p  9 */
		r_c(m,nil,nodes_of_level[0],nodes_of_level[1]);
		/* S t e p  10 */
		{
			int i;
			for (i=1; i<=nodes_of_level[0]; i++)
				b[i] = row_barycenter(m,i,nodes_of_level[1]);
		}
		if (sorted(b,nodes_of_level[0]))
			break;
		/* S t e p  11 (= Schleifenende) */
	}
	store_new_positions(ms,0,g);
	
	cfree (m);
	cfree (m0);
	cfree (ms);
}

/******************************************************************************/
/* Die folgenden Funktionen sind Hilfsfunktionen fuer den n-Level Algorithmus */
/******************************************************************************/

Local void phase1_down(a)
Matrix *a;
{
	int i;

	for (i=0; i<maxlevel-1; i++)
		b_c(marrget(a,i),marrget(a,i+1),
		    nodes_of_level[i],nodes_of_level[i+1]);
	b_c(marrget(a,maxlevel-1),nil,
	    nodes_of_level[maxlevel-1],nodes_of_level[maxlevel]);
}

Local void phase1_up(a)
Matrix *a;
{
	int i;

	for (i = maxlevel-1; i>0; i--)
		b_r(marrget(a,i),marrget(a,i-1),
		    nodes_of_level[i],nodes_of_level[i+1]);
	b_r(marrget(a,0),nil,
	    nodes_of_level[0],nodes_of_level[1]);
}

Local void phase2_down(a)
Matrix *a;
{
	int l; /* Level */
	int i; 
	double b[SIZE];

	/* Von oben nach unten werden in allen Levels die Spalten mit gleichem 
	Barycenter in der Reihenfolge umgekehrt, solange die Spaltenbarycenter
	geordnet sind. Sind diese in einem Level nicht geordnet, so bricht 
	der Algorithmus ab. */

	for (l = 0; l<maxlevel-1; l++)
	{
		/* Spaltenbarycenter berechnen */
		for (i=1; i<=nodes_of_level[l+1]; i++)
			b[i] = column_barycenter(marrget(a,l),i,nodes_of_level[l]);
		if (sorted(b,nodes_of_level[l+1]))
			r_c(marrget(a,l),marrget(a,l+1),
			    nodes_of_level[l],nodes_of_level[l+1]);
		else return;
	}
	for (i=1; i<=nodes_of_level[maxlevel]; i++)
		b[i] = column_barycenter(marrget(a,maxlevel-1),i,nodes_of_level[maxlevel-1]);
	if (sorted(b,nodes_of_level[maxlevel]))
		r_c(marrget(a,maxlevel-1),nil,nodes_of_level[maxlevel-1],nodes_of_level[maxlevel]);
}	

Local void phase2_up(a)
Matrix *a;
{
	int l; /* Level */
	int i; 
	double b[SIZE];
	
	/* Von unten nach oben werden in allen Levels die Zeilen mit gleichem 
	Barycenter in der Reihenfolge umgekehrt, solange die Zeilenbarycenter 
	geordnet sind. Sind diese in einem Level nicht geordnet, so bricht 
	der Algorithmus ab. */

	for (l = maxlevel-1; l > 0; l--)
	{
		/* Zeilenbarycenter berechnen */
		for (i=1; i<=nodes_of_level[l]; i++)
			b[i] = row_barycenter(marrget(a,l),i,nodes_of_level[l+1]);
		if (sorted(b,nodes_of_level[l]))
			r_r(marrget(a,l),marrget(a,l-1),
			    nodes_of_level[l],nodes_of_level[l+1]);
		else return;
	}
	for (i=1; i<=nodes_of_level[0]; i++)
		b[i] = row_barycenter(marrget(a,0),i,nodes_of_level[1]);
	if (sorted(b,nodes_of_level[0]))
		r_r(marrget(a,0),nil,nodes_of_level[0],nodes_of_level[1]);
}	

Local bool equal_a(a1,a2)					/* !!! */
Matrix *a1, *a2;

/* ueberprueft, ob die Arrays von Matrizen a1 und a2 uebereinstimmen */

{	int l; 
	for (l = 0; l < maxlevel; l++)
		if (! equal_m(marrget(a1,l),marrget(a2,l), nodes_of_level[l],nodes_of_level[l+1]))
			return(FALSE);
	return(TRUE);
}

Local void copy_a(a1,a2)					/* !!!! */
Matrix *a1, *a2;

/* kopiert a1 nach a2 */

{
	int i;
	for (i = 0; i < maxlevel; i++)
		copy_m(marrget(a1,i),marrget(a2,i));
}

Local int number_of_crossings_a(a)				/* !!!! */
Matrix *a;
{
	int k = 0;
	int i;
	
	for (i = 0; i < maxlevel; i++)
		k += number_of_crossings(marrget(a,i),nodes_of_level[i],nodes_of_level[i+1]);
	return(k);
}

/********************************************************/
/* Algorithmus fuer einen Graph mit mindestens 3 Levels */
/********************************************************/

Local void bc_n(g)
Sgraph g;
{
	Matrix *a, *a1, *a2, *as;				/* !!!!  */
	int i, j;
	int ks, k; /* Anzahl der Kreuzungen */
	int n1, n2; /* Anzahl der Iterationen */		/* !!!! */
	
	int max_nodes_of_level = 0; /* MH 3/10/91 */
		
	/* Zuerst wird die Phase 1 ("down" - "up") solange durchlaufen, bis man
	am Ende dieselben Matrizen wie am Anfang hat oder eine bestimmte Anzahl 
	("iterations") von Iterationen erreicht ist. Mit der dadurch bestimmten 
	Loesung "as" wird die Phase 2 gestartet. In der Phase 2 wird zuerst die 
	Prozedur "down" ausgefuehrt, darauf folgt Phase 1 ("down" - "up"), dann
	wird die Prozedur "up" ausgefuehrt, wiederum gefolgt von Phase 1 ("up" -
	"down"). Auch die Phase 2 wird solange durchlaufen, bis man am Ende die-
	selben Matrizen wie am Anfang hat oder eine bestimmte Anzahl von 
	Iterationen erreicht ist. */

	/* a  = (Matrix *)calloc(maxlevel,MATRIX_SIZE); */
	/* a1  = (Matrix *)calloc(maxlevel,MATRIX_SIZE); */
	/* a2  = (Matrix *)calloc(maxlevel,MATRIX_SIZE); */
	/* a1 und a2 dienen zum Vergleich, um das Abbruchkriterium zu bestimmen */
	/* as = (Matrix *)calloc(maxlevel,MATRIX_SIZE); */
	
	/* Extension MH 3/10/91 to calculate Matrix size more effectively */
	max_nodes_of_level = 0;
	for (i=0; i < SIZE; i++) {
		if (nodes_of_level[i] > max_nodes_of_level) {
			max_nodes_of_level = nodes_of_level[i];
		}
	}
	
	size = max_nodes_of_level+1;
	matrix_size = size*size;
	
	a  = (Matrix *)calloc(maxlevel,MATRIX_SIZE*sizeof(int));
	a1  = (Matrix *)calloc(maxlevel,MATRIX_SIZE*sizeof(int));
	a2  = (Matrix *)calloc(maxlevel,MATRIX_SIZE*sizeof(int));
	/* a1 und a2 dienen zum Vergleich, um das Abbruchkriterium zu bestimmen */
	as = (Matrix *)calloc(maxlevel,MATRIX_SIZE*sizeof(int));

	for (i = 0; i<maxlevel; i++)
		make_matrix(g,i,marrget(a,i));
	
	copy_a(a,as);
	ks = number_of_crossings_a(as);
	
	/* P h a s e  1 */
	n1 = 0;
	do
	{
		copy_a(a,a1);
		phase1_down(a);
		if ((k = number_of_crossings_a(a)) < ks)
		{
			ks = k;
			copy_a(a,as);
		}
		phase1_up(a);
		if ((k = number_of_crossings_a(a)) < ks)
		{
			ks = k;
			copy_a(a,as);
		}		
	}
	while (++n1 < IT1 && ! equal_a(a,a1));
	
	/* weiterrechnen mit bisheriger Loesung */
	if (! equal_a(a,as))
			copy_a(as,a);

	/* P h a s e  2 */
	n2 = 0;
	do 
	{	
		copy_a(a,a2);
		phase2_down(a);
		n1 = 0;
		do
		{
			copy_a(a,a1);
			phase1_down(a);
			if ((k = number_of_crossings_a(a)) < ks)
			{
				ks = k;
				copy_a(a,as);
			}
			phase1_up(a);
			if ((k = number_of_crossings_a(a)) < ks)
			{
				ks = k;
				copy_a(a,as);
			}		
		}
		while (++n1 < IT1 && ! equal_a(a,a1));

		phase2_up(a);
		n1 = 0;
		do
		{
			copy_a(a,a1);
		
			phase1_up(a);
			if ((k = number_of_crossings_a(a)) < ks)
			{
				ks = k;
				copy_a(a,as);
			}
			phase1_down(a);
			if ((k = number_of_crossings_a(a)) < ks)
			{
				ks = k;
				copy_a(a,as);
			}		
		}
		while (++n1 < IT1 && ! equal_a(a,a1));
	
	}	
	while (++n2 < IT2 && ! equal_a(a,a2));
	
	for (i=0; i<maxlevel; i+=2)
		store_new_positions(marrget(as,i),i,g);
	if (i == maxlevel)
		store_new_positions(marrget(as,maxlevel-1),maxlevel-1,g);
	cfree(a);
	cfree(a1);
	cfree(a2);
	cfree(as);
}




Global	void	reduce_crossings(g)
Sgraph g;
{	
	if (maxlevel == 1) 
		bc_2(g);
	else
		bc_n(g);

}


/*********** E n d e  C R O S . C ****************************************************/
