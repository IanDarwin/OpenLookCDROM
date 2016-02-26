/********************************************************************************/
/* fsp.c --									*/
/*										*/
/* Routines common to XView and Motif FSPtool clients. For alteration of FSP	*/
/* specific features, current remote directory etc. Also includes stream read	*/
/* routine which interprets feedback from trace output of fgetcmd		*/
/*										*/
/* (C)1993 A.J.Doherty								*/
/********************************************************************************/

#include "common.h"
#include "cache.h"
#include "fsp.h"
#include "unix.h"

#include "../config.h"

#include <string.h>

/********************************************************************************/

static char	/* -- all these used by putenv so need to be in fixed memory	*/
	fsp_host[80],		/* -- host name variable FSP_HOST=		*/
	fsp_port[64],		/* -- host port variable FSP_PORT=		*/
	fsp_buf_size[32],	/* -- fsp buffer size FSP_BUF_SIZE =		*/
	fsp_delay[32],		/* -- fsp delay size FSP_DELAY=			*/
	fsp_local_port[32],	/* -- fsp local port FSP_LOCALPORT=		*/
	fsp_timeout[32],	/* -- fsp timeout value FSP_TIMEOUT=		*/
	fsp_local_dir[512],	/* -- fsp local download dir FSP_LOCAL_DIR=	*/
	fsp_dir[512];		/* -- remote fsp dir FSP_DIR=			*/

static SortFormat
	fsp_sort_type;		/* -- sort style for remote listings.		*/

char	*fsp_ls_cmd  = FSP_LS_CMD,	/* -- ptr to pathname of FSP flscmd	*/
	*fsp_ver_cmd = FSP_VER_CMD,	/* -- ptr to pathname of FSP fver	*/
	*fsp_get_cmd = FSP_GET_CMD,	/* -- ptr to pathname of FSP fgetcmd	*/
	*fsp_put_cmd = FSP_PUT_CMD;	/* -- ptr to pathname of FSP fputcmd	*/

/********************************************************************************/

void set_environ()

/* this fn sets up all the FSP client specific environment variables		*/

{ char *ptr;

setenv_var("FSP_TRACE=ON");

if (!(ptr = getenv_var("FSP_TRACE")))
    exit(1);

if ((ptr = getenv_var("FSP_LOCAL_DIR")))
    set_fsp_local_dir(ptr);
else
    if ((ptr = getenv_var("HOME")))
	set_fsp_local_dir(ptr);
    else
	set_fsp_local_dir(".");

if ((ptr = getenv_var("FSP_HOST")))
    set_fsp_host(ptr);

if ((ptr = getenv_var("FSP_PORT")))
    set_fsp_port(ptr);

set_fsp_dir("/");
set_fsp_sorttype(Alpha);
set_fsp_bufsize(1024);
set_fsp_delay(2000);
}

/********************************************************************************/

void set_fsp_dir ( const char *directory_name )

/* this function sets the fsp_dir value and updates the environment variable	*/
/* containing the current fsp directory name					*/

{
strcpy(fsp_dir,"FSP_DIR=");
strcat(fsp_dir,directory_name);
setenv_var(fsp_dir);
}

/********************************************************************************/

void set_fsp_port ( const char *port_name )

/* this function sets the fsp_port value and updates the environment variable	*/
/* containing the current fsp port name						*/

{
strcpy(fsp_port,"FSP_PORT=");
strcat(fsp_port,port_name);
setenv_var(fsp_port);
}

/********************************************************************************/

void set_fsp_host ( const char *host_name )

/* this function sets the fsp_host value and updates the environment variable	*/
/* containing the current fsp host name						*/

{
strcpy(fsp_host,"FSP_HOST=");
strcat(fsp_host,host_name);
setenv_var(fsp_host);
}

/********************************************************************************/

void set_fsp_bufsize ( int bufsize )

/* this function sets the fsp_buf_size value and updates the environment	*/
/* variable containing the current fsp buf size value				*/

{
sprintf(fsp_buf_size,"FSP_BUF_SIZE=%1d",bufsize);
setenv_var(fsp_buf_size);
}

/********************************************************************************/

void set_fsp_delay ( int delay )

/* this function sets the fsp_delay value and updates the environment variable	*/
/* containing the current fsp delay value					*/

{
sprintf(fsp_delay,"FSP_DELAY=%1d",delay);
setenv_var(fsp_delay);
}

/********************************************************************************/

void set_fsp_local_dir ( const char *local_dir )

/* this function sets the local directory to which files should be uploaded	*/

{
sprintf(fsp_local_dir,"FSP_LOCAL_DIR=%s",local_dir);
setenv_var(fsp_local_dir);
}

/********************************************************************************/

void set_fsp_local_port ( int local_port )

/* this function sets the name/address of the local port which FSP clients	*/
/* should use.									*/

{
sprintf(fsp_local_port,"FSP_LOCALPORT=%1d",local_port);
setenv_var(fsp_local_port);
}

/********************************************************************************/

void set_fsp_timeout ( int timeout )

/* this function sets the value of the timeout value for FSP clients 		*/

{
sprintf(fsp_timeout,"FSP_TIMEOUT=%1d",timeout);
setenv_var(fsp_timeout);
}

/********************************************************************************/

void set_fsp_sorttype ( SortFormat sorttype )

/* set the sort type for FSP remote directory listings.				*/

{
fsp_sort_type = sorttype;
}

/********************************************************************************/

SortFormat get_fsp_sorttype()

{
return(fsp_sort_type);
}

/********************************************************************************/

char *get_fsp_dir()

/* return pointer to string containing current remote FSP dir, this string	*/
/* points to the directory name after the FSP_DIR= part of the string		*/

{ char *ptr;

ptr = strchr(fsp_dir,'=');

return(++ptr);
}

/********************************************************************************/

char *get_fsp_local_dir()

/* return pointer to string containing current local FSP dir, this string	*/
/* points to the directory name after the FSP_LOCAL_DIR= part of the string	*/

{ char *ptr;

ptr = strchr(fsp_local_dir,'=');

return(++ptr);
}

/********************************************************************************/

char *get_fsp_host()

{ char *ptr;

ptr = getenv_var("FSP_HOST");

if (!ptr)
    return(NULL);

return(ptr);
}

/********************************************************************************/

char *get_fsp_port()

{ char *ptr;

ptr = getenv_var("FSP_PORT");

if (!ptr)
    return(NULL);

return(ptr);
}

/********************************************************************************/

int read_client_data ( int fd, int *data, char *message )

/* this fn reads the stream data returned by the fget and fput commands. Error	*/
/* conditions are flagged by this routine, the int pointed to by data will be	*/
/* updated with the amount (in bytes) of data transferred			*/

{ static char buffer[256];
  long   pipebytes;
  int    i, bytes, cnt = 0;
  char	*ptr, *ptr2, chkc = 0;



#if !defined(SYSV) && !defined(SVR4)
(void) ioctl(fd, FIONREAD, (char*)&pipebytes);
#else
(void) ioctl(fd, I_NREAD, (char*)&pipebytes);
#endif

if (pipebytes < 1L)
    return(CLIENT_NONE);

buffer[0] = 0;

while (((i = read(fd, buffer, 1)) > 0) && !isalnum(buffer[0]));

if ( i > 0 )
    while (((i = read(fd, &buffer[++cnt], 1)) > 0) && (cnt<255) &&
	    (buffer[cnt]!='\n') && (buffer[cnt]!='\r'));

if (!cnt)		/* -- no data */
    return(CLIENT_NONE);

buffer[cnt] = 0;

if ((sscanf(buffer,"%d%c",&bytes,&chkc)==2) && !strstr(buffer,"no permission")) {
    if (chkc == 'k')
        *data = bytes*1024;
    else
	*data = bytes;

    if ((ptr = strchr(buffer,'['))) {
	ptr2 = strchr(buffer,']');
	*ptr2 = 0;
	strcpy(message,++ptr);
	return(CLIENT_COMPLETE);
	}

    return(CLIENT_STATUS);
    }

ptr = buffer;

while (*ptr) {
    if (isalnum(*ptr))
	if ((*ptr != 'R') && (*ptr != 'I') && (*ptr != 'E')) {
	    strcpy(message,buffer);
	    *data = 0;
	    return(CLIENT_ERROR);
	    }
    ptr++;
    }

*data = 0;
return(CLIENT_WARNING);
}

/********************************************************************************/

