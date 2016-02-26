/********************************************************************************/
/* fsp.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtoollib_FSP_H_
#define _FSPtoollib_FSP_H_ 1

/********************************************************************************/

extern void set_environ();
extern void set_fsp_dir(const char*);
extern void set_fsp_port(const char*);
extern void set_fsp_host(const char*);
extern void set_fsp_bufsize(int);
extern void set_fsp_delay(int);
extern void set_fsp_local_dir(const char*);
extern void set_fsp_local_port(int);
extern void set_fsp_timeout(int);
extern void set_fsp_sorttype(SortFormat);

extern char *get_fsp_dir();
extern char *get_fsp_local_dir();
extern char *get_fsp_port();
extern char *get_fsp_host();

extern int   read_client_data(int,int*,char*);

extern SortFormat get_fsp_sorttype();

/********************************************************************************/
/* Defined Return types from read_client_data */

#define	CLIENT_ERROR	0		/* -- fget bombs out			*/
#define CLIENT_COMPLETE	1		/* -- fget has finished getting file	*/
#define CLIENT_STATUS	2		/* -- fget is returning transfer detail	*/
#define CLIENT_NONE	3		/* -- nothing intelligible returned	*/
#define CLIENT_WARNING	4		/* -- a warning problem like RRRIII	*/

/********************************************************************************/
#endif

