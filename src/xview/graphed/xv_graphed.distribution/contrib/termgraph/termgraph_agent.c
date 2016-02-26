/* (C) Universitaet Passau 1986-1991 */
#include "termgraph_decl.h"
/*************************************************************************
**									**
**	MODULNAME:	termgraph_agent.c				**
**									**
**************************************************************************
**									**
**	ERSTELLUNG:	Rudolf Seisenberger, 	1990			**
**									**
**	AUFGABEN:	Die Datei enthaelt die Funktionen, die einen	**
**			Eingabeagenten analysieren und fuer die 	**
**			Erstellung eines Syntaxbaumes aufbereiten.	**
**									**
*************************************************************************/

/*************************************************************************
**									**
**				FUNKTIONEN				**
**									**
**************************************************************************
**									**
**	EXTERNE: 	Keine						**
**	INTERNE:							**
**	int 		brace_syntax_correct(), brace_correct();	**
**	char 		*fill_buffer(), *flatten(),			**
**			*call_name(), *substitutions()			**
**									**
*************************************************************************/

/*************************************************************************
**									**
**			    GLOBALE VARIABLE				**
**									**
**************************************************************************
**									**
**	In diesem Modul sind alle globalen Variablen, die nur durch	**
**	TermGraph benutzt werden, definiert. Fuer andere Module	sind 	**
**	sie also extern erreichbar.					**
**									**
*************************************************************************/


char	*(fill_buffer());
char 	*flatten();
int	brace_syntax_correct();
int	brace_correct();
char 	*(call_name());
char 	*substitutions();

extern Sgraph pgraph;

extern char termgraph_default_agent_name[MAXNAME];
extern int termgraph_basic_length;
extern int ELEMENT_LENGTH;

char *(call_name(argc, argv))
int		argc;
char	*argv[];

/*** argv und argc sind fuer eine Eingabe des Agenten bei Programmaufruf vorgesehen, ***/
/*** in Abhaengigkeit von GraphEd darf derzeit aber nicht beliebig ueber den 	     ***/
/*** Argumentenvektor verfuegt werden. 						     ***/

{
	if (argc != 2)
		return(termgraph_default_agent_name /*DEFAULT_AGENT_FILE*/);
	else
		return(argv[1]);
} /* call_name */


char *fill_buffer(pgraph, input_name, output_string)
Sgraph		pgraph;
char		*input_name;
char		*output_string[MAXBUF];

{
	int		i;
	char	*term_string, ch[MAXBUF];
	FILE 	*input;
	
	if ((input = fopen(input_name,"r")) == NULL){
		message ("TermGraph%% *** Unknown file \"%s\"! ***\n", input_name);
		return "\0";	/* opening error */
	}

	i=0;
	while ((i < MAXBUF-1) && ((*(ch+i) = getc(input)) != EOF) 
	  && (*(ch+i) != '\0')) 
	  	i++;
	fclose(input);	/* File wird nie mehr angetastet */
	*(ch+i) = '\0';
	return(i >= MAXBUF ? "\0": ch); 
} /* fill_buffer */

char *flatten(ch_in)
char	*ch_in;
{		
	int 		i, f, brace, commata;
	char		flats[MAXBUF], ch[MAXBUF], term[MAXBUF], *flat_out;
	int		input_error;
	
	
	input_error = 0;
	
	if (ch_in[0] == '\0') {
		sprintf (ch, ".");
	} else if ( ch_in[strlen(ch_in) - 1] == '.' ) {
		sprintf (ch, "%s", ch_in);
	} else {
		sprintf (ch, "%s.", ch_in);
	}
	if (strlen(ch_in) >= MAXBUF) {
		ch_in[MAXBUF] = '.'; /* Termende erzwungen */
	}		
	for (i=0,f=0; *(ch +i) != '\0'; i++) {
		if ( *(ch +i)=='\t' || *(ch +i)=='\n' || *(ch +i)=='\r' )
			*(ch +i) = ' '; 
	  	if (!( (ch[i]=='\n')||(ch[i]=='\t')||((ch[i]==' ') && (ch[i+1]==' ')) || 
	  	  (ch[i]=='/') ))
	  		term[f++] = ch[i];
	}
	term[f /*-1*/ ] = '\0'; /* falls -1 : abschliessender '.' wird nicht im Ausgabeknoten gezeigt */
	ch[i] = '\0'; 
	
	if ( ch[0] == '!' )  {
		ch[0] = ' ';	/* substitutions() kann somit unterdr"uckt werden */
	} else {
		sprintf (ch, "%s", substitutions ( ch ));
	}
	
	i = 0;
	while ( (ch[i] != '\0') && ((ch[i] == '\t') || (ch[i] == '\n') || (ch[i] == ' ')) ) {
	  	i++;
	}
	  
	for (f=0 /* i.a.: i */; ch[i] != '\0' && ch[i] != '.'; i++)	{
		  
	  switch (ch[i])
	  {
	  /***	tolerante Analyse:
	    case '\t':
	    case '\n':
	    case ' ':	break;
	  ***/
	    case '\t':
	    case '\n':
	    case ' ':	if ( (ch[i+1] != '\0') && ((ch[i+1] == ' ') || (ch[i+1] == '\t') || (ch[i+1] == '\n')
	    			|| (ch[i+1] == ';' ) || (ch[i+1] == ':') || (ch[i+1] == '|') || (ch[i+1] == '$')
	    			|| ((ch[i+1] == 'o') && (ch[i+2]  != '\0') && (ch[i+2] == 'r')) /* nur 'or' ist oder */
	    			) ) {
	    			break;
	    		} else {
	    			input_error = 1;
	    		 	break;
	    		}
	  
	    
	    case '/':   if (ch[i+1] != '*') {
	    			message ("TermGraph%% *** Ignored reserved character '/'! ***\n");
	    			break; /* '/' ist ein reserviertes Zeichen */
	    		}
	    		i++;
	    		while( ch[i+1] != '\0' && ch[i+2] != '\0' ){
	    			if( ch[i+1] == '*' && ch[i+2] == '/' ){
	    				i+=2;
	    				while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') ||
	    				(ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  					i++;
					}
	    				break;
	    			}
	    			i++;
	    		}
	    		break; /* comment */
	    
	    case ';':	strncpy(flats+f, "/;", 2);
	    		f+=2;
	    		while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') || (ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  			i++;
			}
	    		break;
	    
	    /*** 	nicht zugelassene ODER-Operation aus der Testphase:
	    case '%':	strncpy(flats+f, "/%", 2);
	    		f+=2;
	   	    	break;
	    ***/

	    case 'O':
	    case 'o':	if ((i==0 || *(ch+i-1)==' ' || *(ch+i-1)==')')
	    		&& (*(ch+i+1)=='r' || *(ch+i+1)=='R')
	    		&& (i+2>=MAXBUF || *(ch+i+2)==' ' || *(ch+i+2)=='(') || *(ch+i+2)=='[') {
	    			strncpy(flats+f, "/%", 2);
	    			f+=2;
	    			i++;	/* ueberspringe 'r' von "or" */
	    			while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') || 
	    			(ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  				i++;
				}
	    			break;
	    		}
	    		strncpy (flats + f++, ch+i, 1);
	    		while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') || (ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  			i++;
			}
	    		break;
		    		
	    case '|':	/*** 	tolerante Analyse:
	    		if (*(ch+i+1) == '|') {
	    			break;
	    		} else if (*(ch+i+1) != '$') {	 das ist also nicht '|$' 
	    			message ("TermGraph%% *** Completed '|' to '||'. ***\n"); 
	    			break;
	    		}
	    		***/
	    		
	    		if ((ch[MAX(0, i-1)] != '|') && (ch[i+1] != '|') && (ch[i+1] != '$')) {
	    			message ("TermGraph%% *** Uncomplete operation '|'. ***\n");
	    			input_error = 1;
	    		}
	    		
	    		    		
	    		if (( ((ch[i+1]=='$' || ch[i+1]=='$') && ((ch[i+2]!=NULL)&&(ch[i+2]=='{')) )
	    			/*** zweimal '$' durch z. B. 's' oder 'S' ersetzbar ***/
	    			|| ch[i+1] =='$')     
	    			&& ((ch[i+2]!=NULL) && (ch[i+2]==' ' || ch[i+2]=='{'))) {
	    			/* ch[i] = ' ';
	    			ch[i+1] = '$'; redundant! */
	    			
	    		} else if (ch[MAX(0, i-1)] != '|') {
	    			strncpy(flats+f, "/|", 2);
	    			f+=2;
	    		}
	    		
	    		while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') || (ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  			i++;
			}
	    		break;
	    		
	    case '$':	if ((i==0) || ((i>0) && (*(ch+i-1) != '|'))) {
	    			message ("TermGraph%% *** Completed '$' to '|$'. ***\n");
	    		}
	    		
	    		commata = 0;
	    		strncpy(flats+f, "/$", 2);
	    		f+=2;
	    		while (ch[++i] == ' ');
	    		if (ch[i] == '{') {
	    			strncpy (flats + f++, ch+i, 1);
	    			brace = 1;
	    			while (brace != 0) {
	    				i++;
	    				if (ch[i] != ' ') {
	    					switch (ch[i])
	    					{
	    					case '{':	brace++; break;
	    					case '}':	brace--; break;
	    					
	    					case 'O':
	 					case 'o':	if ((*(ch+i-1)==' ' || *(ch+i-1)==',' || *(ch+i-1)=='{')
	    							&& (*(ch+i+1)=='r' || *(ch+i+1)=='R')
	    							&& (*(ch+i+2)==' ' || 
	    							*(ch+i+2)=='}') || *(ch+i+2)=='[') {
	    								ch[i] = ' ';
	    								ch[i+1] = '%';
	    								i++; /*blank weg!!!*/
	    							}
	    							break;
	    							
	    					case '|':	if (*(ch+i+1)=='|') {
	    								ch[i] = ' ';
	    								i++; /*Leerzeichen weg!*/
	    							}
	    							if (( ((*(ch+i+1)=='s' || *(ch+i+1)=='S') && 
	    							((ch[i+2]!=NULL)&&(ch[i+2]=='{')) )
	    							|| *(ch+i+1)=='$') 
	    							&& ((ch[i+2]!=NULL)&&(ch[i+2]==' ' || ch[i+2]=='{'))) {
	    								ch[i] = ' ';
	    								ch[i+1] = '$';
	    								i++; /*blank weg!!!*/
	    							}
	    							break;
	    							
	    					case ',':	if (brace == 1) {
	    							/* z.B.: (a|${a,b}b)|${|${a, b},c}c */
	    							/* ignoriere hier zweites Komma     */
	    							
	    								commata++;
	    							}
	    					
	    					default:	break;
	    					}
	    					strncpy (flats + f++, ch+i, 1);	
	    				}
	    			}
	    		}
	    		
	    		if (commata != 1){
	    			input_error = 1;
	    		}
	    		while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') || (ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  			i++;
			}
	  	    	break;
	    		
	    case ':':	if (*(ch+i+1)==':') {
	    			break;
	    		} else if ( (i==0) || ((i>0) && (ch[i-1] != ':')) ) {
	    			message ("TermGraph%% *** Completed ':' to '::'. ***\n");
	    		}
	    		strncpy(flats+f, "/:", 2);
	    		f+=2;
	    		while ( (ch[i+1] != '\0') && ((ch[i+1] == '\t') || (ch[i+1] == '\n') || (ch[i+1] == ' ')) ) {
	  			i++;
			}
	    		break;
	    		
	    default : 	strncpy (flats + f++, ch+i, 1);
	    		break;
	    
	  }   		
	}
	
	if (input_error == 0) {
		strncpy (flats+f, "\0", 1);
		sprintf (ch_in, "%s", term); /* Eingabe geht geglaettet zurueck */
	} else {
		message ("TermGraph%% *** Input refused. Syntax error! ***\n");
		bell();
		strncpy (flats, "\0", 1);
		sprintf (ch_in, "\0");	
	}
	
	return flats;
} /* flatten */

char *substitutions (ch_in)
char *ch_in;
/******************************************************************************/
/* Ersetzt die hinter dem Agenten-Hauptterm evtl. angegebenen Termersetzungen */
/* wie sie vor allem bei Rekursionen auftreten. Teilterme sind durch '.' von- */
/* einander getrennt. Ein Beispiel:					      */
/*                                                                            */
/*         a; b or C; 11_a || (G1 ;G1' |${G1, H2} h or (H1 || H2)) .          */
/*             C  :: c0; b; C .                                               */
/*             H1 :: k; l; h .                                                */
/*									      */
/* Auch Operationen koennen ersetzt werden: Dann aber nicht als Rekursion.    */
/* Die Operation muss den Anfang der Ersetzung bilden. Beispiel:	      */
/*                                                                            */
/*         a ${ a, S1 } b S1 c.						      */
/*             S1 :: ${ b, c }.                                               */
/*									      */
/* wird zu a ${ a, ${ b, c } } b ${ b, c } c.				      */
/******************************************************************************/

{
	char ch[MAXBUF], subst[MAXBUF], ch_tmp[MAXBUF], cmpname[MAXBUF], 
	     copystring[MAXBUF], subst_rest[MAXBUF];
	int  anf, drin, u, i, j, k, l, m, copystring_len, cmpname_len;
	int  with_name;
	
	for ( u=0; (u<MAXBUF) && (ch_in[u] != TERM_END_CHR) && (ch_in[u]!='\0'); u++ ) {
		if ( strncmp (ch_in+u, "/*", 2) == 0 ) {
			for ( ; (u < MAXBUF) && (strncmp (ch_in + u + 2, "*/", 2) != 0); ) {
				 ch_in[u++] = ' ';
			}
			if (strcmp (ch_in + u + 2, "*/", 2) != 0) {
				ch_in[u++] = ' ';
				ch_in[u++] = ' ';  /* zweimal loescht nachtraeglich Kommentaranfangklammer aus */
				ch_in[u++] = ' ';
				ch_in[u++] = ' ';  /* zweimal loescht Kommentarendeklammer aus */
			}
		} 
	}
	
	if ( ch_in[u] == TERM_END_CHR ) {
		strcpy ( subst, ch_in + u  + 1);
		anf = 0;
		strncpy ( ch, ch_in, u ); 
		ch[u] = '\0';
		/* einmalig, um die Zeichen vor der ersten Ersetzung zu kopieren */
	} else {
		anf = MAXBUF;	/* keine Ersetzungsterme vorhanden */
	}
	
	drin = k = 0;
	while ( (anf < MAXBUF) &&  (subst[anf] != '\0') && (anf < strlen(subst) ) && (subst[anf] != TERM_END_CHR) ) {
		
		while ( (anf < MAXBUF) && (subst[anf] != '\0') && (!name_char(subst[anf])) ) {
			anf++; /* Leerraum... */
		}
		sprintf (subst, "%s", subst + anf);
		if (anf < MAXBUF) {
			anf = 0;
		}
		if (subst[0] == '\0') {
			break; /* Es gibt ohnehin nichts mehr zu ersetzen */
		}
		
		for ( j=0; (j < MAXBUF) && (subst[anf] != '\0') && (name_char(subst[anf])); anf++, j++ )
			cmpname[j] = subst[anf];
		cmpname[j] = '\0';  /* kopiere den Identifikatornamen auf cmpname */
			
		while ( (anf < MAXBUF) && (subst[anf] != ':') && (subst[anf] != TERM_END_CHR) ) {
			anf++;
		}
		if ( subst[anf+1] == ':' ) anf++;	/* falls "::" anstatt ":" auftritt */
		
		with_name = FALSE;
		m = anf;
		while (m+j <= strlen( subst )) {
			if (( strncmp(subst+m, cmpname, j) == 0 ) && ( !name_char(subst[m-1]) )
				&& ( !name_char(subst[m+j]) )) {
				with_name = TRUE;
				break;
			}
			m++;
		}
		if ( subst[anf] == ':' ) {
			anf++;
			for ( l=0; (anf+l < MAXBUF) && (subst[anf+l] != TERM_END_CHR); l++ );
			
			if (subst[anf+l] == TERM_END_CHR) {
				l--;	     /* l nimmt dagegen TERM_END_CHR nicht mit */
				strncpy ( subst_rest, subst+anf, l+1 ); /* subst_rest ist die Ersetzung */
				subst_rest[l+1] == '\0';
				anf += l+1 +1;  /* anf setzt hinter TERM_END_CHR wieder auf */
			} else {
				anf = MAXBUF;
				strncpy ( subst_rest, subst+anf, l+1 ); /* copyfy on subst_rest from anf to l */
			}
			subst_rest[l+1] = '\0';
			
			for (k=0; subst_rest[k] == ' '; k++); /* erstes Nicht-Leerzeichen */
			
			if (with_name == TRUE) {
				sprintf (copystring, "(%s:(%s))", cmpname, subst_rest);
			} else if ((name_char(subst_rest[k])) || (subst_rest[k] == '(')) {
				sprintf (copystring, "(%s)", subst_rest);
			} else {
				sprintf (copystring, "%s", subst_rest); /* ist eine Operation */
			}
			copystring_len = strlen( copystring );
			cmpname_len = strlen( cmpname );
			k = l = 0;
			while ( l <= u ) {
				if (cmpname[0] == '\0') {
					message ("TermGraph%% Ignored empty substitution name!\n");
					break; /* Durch einen leeren Namen wuerde die ganze Eingabe ueberschrieben */
				}
				if ( (strncmp(ch + l, cmpname, j)==0) && !name_char(ch[l + j]) ) {
					sprintf (ch_tmp + k, "%s", copystring);
					k += copystring_len;
					l += cmpname_len;
				} else {
					ch_tmp[k] = ch[l];
					k++;
					l++;
				}	
			} /* while */
			sprintf (ch, "%s", ch_tmp);
			u = strlen( ch );
		}
	}	
	return ch;
} /* substitutions */

int brace_syntax_correct (leftc, rightc, str, fault)
char	leftc, rightc, *str;
int	*fault;
{	
	int	i, f, max_length;
	
	if (str == NULL) return 0;
	
	i= f= 0;
	max_length = strlen( str );
	while (i <= max_length) {
	
		if (str[i] == rightc) {
			*fault = f;
			return i;	/* hier ist die Klammerung korrekt */
		}
		
		switch (str[i]) 
		{
		  case '{': 	i += 1 + brace_syntax_correct ( '{', '}', str+i+1, &f);
		    		break;

		  case '(':	i += 1 + brace_syntax_correct ( '(', ')', str+i+1, &f);
		  		break;
		  		
		  case '[': 	i += 1 + brace_syntax_correct ( '[', ']', str+i+1, &f);
		  		break;
		  				  		
		  case '}': 			  		
		  case ')':	
		  case ']': 	
		  case '\0':	if ( leftc == '?' ) {
		  			message ( "TermGraph%% *** Too many %c`s in input. ***\n",
		  				str[i] );
		  		} else {
		  			message ( "TermGraph%% *** Too many %c`s in input. ***\n",
		  				leftc );
		  		}
		  		*fault = 1; /* is a fault */
		  		return ((str[i] == '\0')? i-1: i);
		  		
		  		
		  default :	break;
		}
		i++;
	}
	
	message ( "TermGraph%% *** Input overflow with length %d! ***\n", i /* MAXBUF */ );
	*fault = 1;
	return i;
	
} /* brace_syntax_correct */

int brace_correct(bc)
char	*bc;
{	
	int fault=0;
	
	if (bc[0] == '\0') {
		return -1;
	}
	
	brace_syntax_correct ('?', '\0', bc, &fault);
	return ( fault );
	/* 0 = korrekt, 1 = Klammerfehler */
	
} /* brace_correct */
	
