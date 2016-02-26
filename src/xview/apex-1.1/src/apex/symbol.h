/*
 * $Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/symbol.h,v 1.1 93/01/06 03:27:55 gounares Exp Locker: gounares $
 */
/*
 * symbol.h
 * 
 * Written by Alex Gounares for the apeX environment.
 * 
 * declarations for the data structures used to hold information for the program
 * analysis modules
 */
/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the GNU General Public License Version 2
 * 
 * see the apeX manual for more details
 */

#ifndef __symbol_h
#define __symbol_h

typedef struct symbol {		       /* symbol table entries: */
	char           *szName;	       /* name */
	char           *szFilename;
	int             lineno;
	int				V;
	int				scope;			/*not currently used*/
	int				seen;			/*is the symbol currently being viewed?*/
	int				fStatic;		/*is the function global or static?*/
	char          **calls;
}              *Symbol;



Symbol         *allsymbols(/*int*/),
                installat(/* Symbol, int */),
                lookup(/*char *, char * */);
void            enterscope(),
                clear_table();
char           *strsave(/*char **/);

#define install(s) installat(s, 0)

#endif
