/********************************************************************************/
/* xview/system.c --								*/
/*										*/
/* Operating System related functionality which must be dealt with or is used	*/
/* by the xview toolkit is in this file. More general OS functions are in the	*/
/* lib/unix.c file.								*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#include "../config.h"
#include "../lib/common.h"
#include "../lib/cache.h"
#include "../lib/fsp.h"
#include "../lib/unix.h"

#include "fsptool.h"
#include "frame.h"
#include "file.h"
#include "system.h"

#include <signal.h>

#ifndef HAS_VFORK
#define vfork() fork()
#endif

/********************************************************************************/

extern int	files_found, files_selected;

extern Base_frame  baseframe;
extern Local_frame localframe;

/********************************************************************************/

int		pipe_io[2][2];

Notify_client	client1 = (Notify_client) 72;

/********************************************************************************/

Notify_value sigcatcher_decompress (	Notify_client	client,
					int		pid,
#if defined(SVR4) || defined(SYSV)
					int		*status,
#else
					union wait	*status,
#endif
					struct rusage	*rusage )

{ char	    *ptr1, *ptr2, *ptr3;
  CacheData *ptr;


if (WIFEXITED(*status)) {
#if defined(SVR4) || defined(SYSV)
    if (*status|255) {
#else
    if (status->w_retcode) {
#endif
	left_footer(baseframe.frame,"Couldn't uncompress file ...");
	return(NOTIFY_DONE);
	}

    ptr = (CacheData*) xv_get(baseframe.frame, XV_KEY_DATA, pid);
    xv_set(baseframe.frame, XV_KEY_DATA_REMOVE, pid, NULL);

    ptr1 = c_malloc(strlen(get_fsp_local_dir())+strlen(ptr->name)+2);
    strcpy(ptr1,get_fsp_local_dir());
    strcat(ptr1,"/");
    strcat(ptr1,ptr->name);
    ptr2 = strrchr(ptr1,'.');

    if (ptr2)
	*ptr2 = 0;

    ptr3 = c_strdup(ptr1);
    free(ptr1);

    left_footer(baseframe.frame,"Starting %s ...",ptr1);

    xv_set(baseframe.frame,
		XV_KEY_DATA,
			do_command(string_to_args(
				get_file_action(ptr->filetype),ptr3)),
			ptr3,
		NULL);

    return(NOTIFY_DONE);
    }

return(NOTIFY_IGNORED);
}

/********************************************************************************/

Notify_value sigcatcher ( Notify_client client,
			  int		pid,
#if defined(SVR4) || defined(SYSV)
			  int		*status,
#else
			  union wait	*status,
#endif
			  struct rusage *rusage )

{
if (WIFEXITED(*status))
    {
    notify_set_input_func(client, NOTIFY_FUNC_NULL,
	(client1 == client) ? pipe_io[1][0] : 0);

    close(pipe_io[0][1]);
    right_footer(baseframe.frame,files_found,files_selected);
    set_frame_busy(FALSE);
    return(NOTIFY_DONE);
    }

return(NOTIFY_IGNORED);
}

/********************************************************************************/

Notify_value batch_sigcatcher ( Notify_client client,
			  int		pid,
#if defined(SVR4) || defined(SYSV)
			  int		*status,
#else
			  union wait	*status,
#endif
			  struct rusage *rusage )

{
if (WIFEXITED(*status)) {
    return(NOTIFY_DONE);
    }

return(NOTIFY_IGNORED);
}

/********************************************************************************/

Notify_value sigcatcher_noio ( Notify_client client,
			  int		pid,
#if defined(SVR4) || defined(SYSV)
			  int		*status,
#else
			  union wait	*status,
#endif
			  struct rusage *rusage )

/* this fn is used as the signal catcher for commands used to execute files	*/
/* from the remote and local sites. If the file was from the remote site then	*/
/* this will delete it if it is still in existence.				*/

{
if (WIFEXITED(*status)) {
    char *ptr = (char *) xv_get(baseframe.frame, XV_KEY_DATA, pid);

    xv_set(baseframe.frame, XV_KEY_DATA_REMOVE, pid, NULL);

    if (ptr) {			/* -- if NULL then local file so don't delete	*/
        (void) unlink(ptr);
	free(ptr);
	}

    return(NOTIFY_DONE);
    }

return(NOTIFY_IGNORED);
}

/********************************************************************************/

Notify_value sigcatcher_null ( Notify_client	client,
			  	int		pid,
#if defined(SVR4) || defined(SYSV)
				int		*status,
#else
				union wait	*status,
#endif
				struct rusage *rusage )

/* this fn is a signal catcher for SIGPIPE, this way occur if the fput process	*/
/* terminates before we write to it to tell it which file to send. If so we	*/
/* just ignore the Signal.							*/

{
left_footer(localframe.frame,"Couldn't send file ...");
return(NOTIFY_DONE);
}

/********************************************************************************/

int do_spawn ( char *argv[], void *stream_handler )

/* this function forks a new process, the new child process is used to carry	*/
/* out the commands specified in argv, and the argv arguments list. It calls	*/
/* the system specific unix_spawn to handle actual forking and only performs	*/
/* XView toolkit related actions.						*/

{ int pid = (int) unix_spawn(argv);

(void) notify_set_input_func(client1, stream_handler, pipe_io[1][0]);
(void) notify_set_wait3_func(client1, sigcatcher, pid);
return(pid);
}

/********************************************************************************/

int do_batch_spawn ( char *argv[], void *stream_handler )

{ int pid = (int) unix_spawn(argv);

(void) notify_set_input_func(client1, stream_handler, pipe_io[1][0]);
(void) notify_set_wait3_func(client1, batch_sigcatcher, pid);
return(pid);
}

/********************************************************************************/

int do_spawn_put ( char *argv[], void *stream_handler )

/* this function forks a new process, the new child process is used to carry	*/
/* out the commands specified in argv, and the argv arguments list. It calls	*/
/* the system specific unix_spawn to handle actual forking and only performs	*/
/* XView toolkit related actions. Also sets up signal catcher for SIGPIPE	*/
/* trapping.									*/

{ int pid = (int) unix_spawn(argv);

(void) notify_set_input_func(client1, stream_handler, pipe_io[1][0]);
(void) notify_set_wait3_func(client1, sigcatcher, pid);
(void) notify_set_signal_func(client1, sigcatcher_null, SIGPIPE, NOTIFY_SYNC);
return(pid);
}

/********************************************************************************/

int do_command ( char *argv[] )

/* this function forks a new process, the new child process is used to carry	*/
/* out the commands specified in argv, and the argv arguments list. No new I/O	*/
/* pipes set up for these commands however. Calls unix_command to do command	*/
/* simply handles toolkit specific needs.					*/

{ int pid = (int) unix_command(argv);

(void) notify_set_wait3_func(client1, sigcatcher_noio, pid);
return(pid);
}

/********************************************************************************/

int do_decompress ( char *argv[] )

/* this function forks a new process, the new child process is used to carry	*/
/* out the commands specified in argv, and the argv arguments list. No new I/O	*/
/* pipes set up for these commands however. Unlike the previous routine this	*/
/* will take a named file, attempt to decompress it and perform the appropriate	*/
/* action on the decompressed file.						*/

{ int pid = (int) unix_command(argv);

(void) notify_set_wait3_func(client1, sigcatcher_decompress, pid);
return(pid);
}

/********************************************************************************/
