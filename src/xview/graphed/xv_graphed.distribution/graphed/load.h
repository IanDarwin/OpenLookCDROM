/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef LOAD_HEADER
#define	LOAD_HEADER

extern	int	load          ();
extern	void	set_lex_input ();

extern	int	lex_input_file_linenumber;
extern	int	overwrite_state;

extern	void	set_filename     ();
extern	char	*get_filename    ();

extern	int	load_buffer;

/*yylex und yyparse geloescht*/
#endif
