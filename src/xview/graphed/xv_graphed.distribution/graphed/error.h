/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#ifndef ERROR_HEADER
#define ERROR_HEADER

extern	void	write_message ();
extern	void	message       ();
extern	void	warning       ();
extern	void	error         ();
extern  void	sys_error     (); 
extern	void	fatal_error   ();
extern	void	die           ();	/* main.c	*/


/* UNIX - Fehlermeldungen	*/

#include <errno.h>

extern	int	sys_nerr;
extern	char	*sys_errlist[];

#endif
