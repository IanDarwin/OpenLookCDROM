/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <sys/types.h>
#include <pwd.h>

/*
 *  get_user_info() returns the user's full name and any other personal info
 * 	contained in the 5th field of the record in /etc/passwd which begins
 *	with their login name.  If L_Name is the loginname of the user then 
 * 	the information returned is the same as the info gotten by the shell
 *	command   
 *	/usr/bin/awk '{FS=":"} {if ($1 ~ /^L_Name/) {print $5}}' /etc/passwd
 *  From the GETLOGIN(3) man page:
 *	"The correct  procedure for determining the login name is to call
 *	cuserid, or to call getlogin() and, if  it  fails,  to  call
 *	getpwuid(getuid())."
 *  Because of the controversy over cuserid() (see CUSERID(3V)) and whether
 *	it will continue to be part of the standard library, we have 
 *	chosen to not use cuserid().
 */
char *
get_user_info()
{
char		*getlogin(),*name;
int		getuid();
struct passwd 	*p,*getpwuid(), *getpwnam();

if( name = getlogin() )
   p = getpwnam(name);
else 
   p = getpwuid(getuid());
return(p->pw_gecos);
}
