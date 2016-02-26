/* (C) Universitaet Passau 1986-1991 */
#include <string.h>
#define TRUE (1==1)
#define FALSE !TRUE

typedef struct komp_list {
	char			*komponent;
	struct komp_list	*next;
} *Komp_list;

static int	fls_fits_regular_expression( str, exp )
char	*str, *exp;
{
	int	el = 0, sl = 0, last_star = -1, pp = -1;
	
	do {
		while( exp[el] == '*' ) {
			el++;
			pp = sl;
			last_star = el;
		}
		if( exp[el] == '\0' ) {
			if( (last_star == el) || (str[sl] == '\0') ) {
				return TRUE;
			} else {
				return FALSE;
			}
		}
		if( (str[sl] == exp[el]) || (exp[el] == '?') ) {
			el++;
			sl++;
		} else {
			if( last_star < 0 ) {
				return FALSE;
			} else {
				el = last_star;
				sl = ++pp;
			}
		}
		if( str[sl] == '\0' ) {
			if( (exp[el] == '\0') || ((exp[el]=='*') && (exp[el+1]=='\0')) ){
				return TRUE;
			} else {
				return FALSE;
			}
		}
	} while( TRUE );
}

static Komp_list	new_komplist()
{
	Komp_list hilf;
	
	hilf = (Komp_list)malloc(sizeof(struct komp_list));
	if( hilf != (Komp_list)NULL ){
		hilf->komponent = (char *)NULL;
		hilf->next = (Komp_list)NULL;
	}
	return hilf;
}

void	dispose_komplist( k )
Komp_list	k;
{
	Komp_list h;
	
	while ( k != (Komp_list)NULL ) {
		h = k->next;
		if( k->komponent != (char *)NULL ) {
			free( k->komponent );
		}
		free( k );
		k = h;
	}
}

Komp_list	filename_komponents( name )
char *name;
{
	struct komp_list	hilf;
	Komp_list		h;
	register int		len, pos1, pos2;
	register char		test;
	
	hilf.komponent = (char *)NULL;
	hilf.next = (Komp_list)NULL;
	h = &hilf;
	len = strlen(name) + 1;
	pos1 = 0;
	
	do {
		h->next = new_komplist();
		if( h->next != (Komp_list)NULL ) {
			h->next->komponent = (char *)malloc( len );
			if( h->next->komponent == (char *)NULL ){
				dispose_komplist( h->next );
				h->next = (Komp_list)NULL;
				return (Komp_list)hilf.next;
			}
			pos2 = 0;
		} else {
			return (Komp_list)hilf.next;
		}
		h = h->next;
		test = name[pos1++];
		while( test != '\0' && test != ' ' ){
			h->komponent[pos2++] = test;
			test = name[pos1++];
		}
		h->komponent[pos2] = '\0';
	} while( test != '\0' );
/*	h->komponent[pos2++] = '|';
	h->komponent[pos2] = '\0';
*/	return (Komp_list) hilf.next;
}

Komp_list	rexp_komponents( name )
char *name;
{
	struct komp_list	hilf;
	Komp_list		h,hh;
	register int		len, pos1, pos2;
	register char		test;
	register int		first;
	
	h = &hilf;
	len = strlen(name) + 1;
	pos1 = 0;
	first = TRUE;
	hilf.komponent = (char *)NULL;
	hilf.next = (Komp_list)NULL;
	
	do {
		h->next = new_komplist();
		if( h->next != (Komp_list)NULL ) {
			h->next->komponent = (char *)malloc( len );
			if( h->next->komponent == (char *)NULL ){
				dispose_komplist( h->next );
				h->next = (Komp_list)NULL;
				return (Komp_list)hilf.next;
			}
			if( first ) {
				pos2 = 0;
				first = FALSE;
			} else {
				h->next->komponent[0] = '*';
				pos2 = 1;
			}
		} else {
			return (Komp_list)hilf.next;
		}
		h = h->next;
		test = name[pos1++];
		while( test != '\0' && test != '*' ){
			h->komponent[pos2++] = test;
			test = name[pos1++];
		}
		if( test == '*') {
			h->komponent[pos2++] = '*';
		}
		h->komponent[pos2] = '\0';
	} while( test != '\0' );
	h = hilf.next;
	if( !strcmp((hilf.next)->komponent,"*") ){
		h = (hilf.next)->next;
		(hilf.next)->next = (Komp_list)NULL;
		dispose_komplist(hilf.next);
		hilf.next = h;
	}
	while( (h != (Komp_list)NULL ) && ((h->next) != (Komp_list)NULL )) {
		if( 	(!strcmp( ((h->next)->komponent), "*" ))   ||
			(!strcmp( ((h->next)->komponent), "**" ))    ) {
			hh = h->next;
			h->next = hh->next;
			hh->next = (Komp_list)NULL;
			dispose_komplist(hh);
		} else {
			h = h->next;
		}
	}
			
	return (Komp_list) hilf.next;
}

static void print_komplist( k , s)
Komp_list k;
char *s;
{
	printf( "components of '%s'\n", s );
	while( k != (Komp_list)NULL ) {
		printf( "---%s---\n", k->komponent );
		k = k->next;
	}
	printf( "fin-------------\n" );
}

static void	special_add_string( s1, s2 )
char	*s1,*s2;
{
	int h=0,hh=0;
	char	c;
	
	while( s1[h] != '\0' ) {
		h++;
	}
	while( (c = s2[hh++]) != '\0' ) {
		if( c == '?' ) {
			c = '_';
		}
		if( c != '*' ) {
			s1[h++] = c;
		}
	}
	s1[h] = '\0';
}

char	*get_name_of_lists( nkl, ekl )
Komp_list	nkl, ekl;
{
	Komp_list	lauf1,lauf2;
	int		print_nkl;
	char		*result, s1[1024];
	
/*	print_komplist( nkl, "File" );
	print_komplist( ekl, "Extension" );
*/
	
	lauf1 = nkl;
	lauf2 = ekl;
	result = (char *)malloc(1025);
	if( result == (char *)NULL ){
		return (char *)NULL;
	}
	result[0] = '\0';
	
	if( ekl->komponent[0] == '*' ) {
		print_nkl = TRUE;
	} else {
		print_nkl = FALSE;
	}
	
	while( lauf1 != (Komp_list)NULL && lauf2 != (Komp_list)NULL ) {
		if( fls_fits_regular_expression( lauf1->komponent, lauf2->komponent ) ) {
			special_add_string( result, lauf1->komponent );
			if( lauf2->next != (Komp_list)NULL ) {
				sprintf( s1, "%s%s", lauf2->komponent, (lauf2->next)->komponent );
				if( !print_nkl && fls_fits_regular_expression( lauf1->komponent, s1) ) {
					lauf2 = lauf2->next->next;
					print_nkl = TRUE;
				} else {
					lauf2 = lauf2->next;
				/*	print_nkl = FALSE;*/
				}
			} else {
				if(	lauf1->next == (Komp_list)NULL || 
					(lauf2->komponent)[strlen(lauf2->komponent)-1] == '*' ) {
					
					lauf2 = lauf2->next;
				}
				print_nkl = TRUE;
			}
			lauf1 = lauf1->next;
		} else {
			if( print_nkl ) {
				special_add_string( result, lauf1->komponent );
				lauf1 = lauf1->next;
				print_nkl = FALSE;
			} else {
				if( 	(lauf2->komponent)[strlen(lauf2->komponent)-1] == '*' ) {
					special_add_string( result, lauf2->komponent );
					lauf2 = lauf2->next;
					print_nkl = TRUE;
				} else {
					print_nkl = TRUE;
				}
			}
		}
		
		if( lauf1 == (Komp_list)NULL ) {
			while( lauf2 != (Komp_list)NULL ) {
				special_add_string( result, lauf2->komponent );
				lauf2 = lauf2->next;
			}
		}
		if( lauf2 == (Komp_list)NULL ) {
			while( lauf1 != (Komp_list)NULL ) {
				special_add_string( result, lauf1->komponent );
				lauf1 = lauf1->next;
			}
		}
	}
	return result;
}
/*		
main( argc, argv )
int	argc;
char	**argv;
{
	Komp_list k,nl;
	char *r;
	
	k = rexp_komponents("test=x?z=.c=" );
	print_komplist( k, "test*x?z*.c*" );
	while( --argc > 0 ){
		nl = filename_komponents( argv[argc] );
		print_komplist( nl , argv[argc] );
		r = get_name_of_lists( nl, k );
		printf( "\nsuggestion : %s\n\n", r );
		free( r );
		dispose_komplist( nl );
	}
}
*/
