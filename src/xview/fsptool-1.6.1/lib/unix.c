/********************************************************************************/
/* lib/unix.c --								*/
/*										*/
/* Operating System related functionality. Ostensibly this means unix (hence	*/
/* the file name :-> ). Items of system to system dependence should be confined	*/
/* to this file if possible. However this file still retains toolkit freedom	*/
/* so items which are system and toolkit specific will be found in the named	*/
/* toolkit directory as system.h						*/
/*										*/
/* (c)1993 A.J.Doherty								*/
/********************************************************************************/

#include "../config.h"
#include "common.h"
#include "cache.h"
#include "fsp.h"
#include "unix.h"

#include <string.h>
#include <signal.h>

#include <sys/types.h>
#include <sys/time.h>
#include <sys/vfs.h>
#include <sys/resource.h>

#if defined(SVR4) || defined(SYSV)
#include <sys/statfs.h>
#include <sys/wait.h>
#endif

/********************************************************************************/
/* All toolkit apps linked should provide a pipe_io global var for piping	*/

extern int pipe_io[2][2];

/********************************************************************************/
/* this var indicates the name of the error-handler which fsptoollib routines	*/
/* will call. It takes a var-args list parameter with position 1 as severity	*/
/* level (int), item 2 is printf style format string, items onwards are for	*/
/* printf use.									*/

static void (*error_handler)(int,...);

/********************************************************************************/

static pid_t stream_pid;

/********************************************************************************/

long csectime(void)

/* this fn returns the current time to a resolution of 1 centi-second. Time	*/
/* value returned is number of centi-seconds since 00:00 January 1, 1970	*/

{ struct timeval tp;

gettimeofday(&tp,NULL);
return((tp.tv_sec*100)+(tp.tv_usec/10000));
}

/********************************************************************************/

FILE *perropen ( char *argv[] )

/* this fn is practically the same as the standard popen fn but it also sends	*/
/* stderr output from the child process down the pipe - needed for FSP commands	*/

{ int		loop;
  pid_t		pid;
  struct rlimit rlp;


if (pipe(pipe_io[0]) < 0) {
    perror("FSPtool:pipe");
    exit(-1);
    }

if ((pid = fork()) == 0)
    {
    dup2(pipe_io[0][1], 1);
    dup2(pipe_io[0][1], 2);

    if (getrlimit(RLIMIT_NOFILE,&rlp) != 0) {
	perror("FSPtool:getrlimit");
	exit(-1);
	}

    for ( loop = (int) rlp.rlim_cur; loop > 2; loop--)
	(void) close(loop);

    for (loop = 0; loop < NSIG; loop++)
	(void) signal(loop, SIG_DFL);

    execvp(*argv,argv);

    if (errno == ENOENT)
	fprintf(stderr,"%s command not found\n",argv[0]);
    else
	perror(FSP_LS_CMD);

    perror("FSPtool:execvp");
    _exit(-1);
    }

if (pid == -1) {
    perror("FSPtool:fork");
    exit(-1);
    }

stream_pid = pid;
close(pipe_io[0][1]);
return(fdopen(pipe_io[0][0],"r"));
}

/********************************************************************************/

void perrclose ( FILE *stream )

/* this fn is the companion to perropen it closes up the stream pointer and	*/
/* shuts down the parent end of the pipe. We assume the process at the other	*/
/* end of the stream has finished whatever it was doing so we must now wait	*/
/* wait on it to reclaim sys resource. The wait should not block for any length	*/
/* of time.									*/

{
fclose(stream);
close(pipe_io[0][0]);
waitpid(stream_pid,NULL,0);
}

/********************************************************************************/

void free_space ( int *total, int *percent )

/* this fn returns the space free on the current FSP_LOCAL_DIR mounted file	*/
/* system, returns absolute kbytes free and percentage				*/

{ int totalkb, freekb;

#if defined(SVR4) || defined(SYSV)
  struct statvfs buf;
if (statvfs(get_fsp_local_dir(),&buf)) {
#else
  struct statfs buf;
if (statfs(get_fsp_local_dir(),&buf)) {
#endif
    perror("statfs");
    (*error_handler)(ERROR,"statfs: couldn't get disk free");
    *total = 0;
    *percent = 0;
    return;
    }

totalkb = (int)(buf.f_bsize*buf.f_blocks/1024);

#if defined(SVR4) || defined(SYSV)
  freekb = (int)(buf.f_bsize*buf.f_bfree/1024);
#else
  freekb = (int)(buf.f_bsize*buf.f_bavail/1024);
#endif

*total   = freekb;
*percent = (int) (((float)freekb)/((float)totalkb)*100.0);
}

/********************************************************************************/

void *c_malloc ( size_t size )

/* this fn is simply a checking malloc - if the returned value is NULL then an	*/
/* appropriate error is printed and the program exits.				*/

{ register void *ptr;

if ((ptr = malloc(size)) == NULL)
    (*error_handler)(SERIOUS_ERROR,"malloc: unable to allocate memory");

return(ptr);
}

/********************************************************************************/

void *c_calloc ( size_t size, int number )

/* this fn is simply a checking calloc - if the returned value is NULL then an	*/
/* appropriate error is printed and the program exits.				*/

{ register void *ptr;

if ((ptr = calloc(size,number)) == NULL)
    (*error_handler)(SERIOUS_ERROR,"calloc: unable to allocate memory");

return(ptr);
}

/********************************************************************************/

char *c_strdup ( const char *source )

/* as above this fn is a checking version of strdup which aborts if memory	*/
/* could not be allocated for the new string copy				*/

{ register char *ptr;

if ((ptr = strdup(source)) == NULL)
    (*error_handler)(SERIOUS_ERROR,"strdup: unable to allocate memory");

return(ptr);
}

/********************************************************************************/

void terminate_process ( int process_id )

/* this fn simply terminates a process which we may have spawned off. Handles	*/
/* any system dependencies or requirements as needed.				*/

{
kill(process_id,SIGQUIT);
}

/********************************************************************************/

void setenv_var ( const char *env_var )

/* write out a value to the local process environment table. Should handle any	*/
/* systemic differences required.						*/

{ 
if (putenv((char*) env_var))
    (*error_handler)(ERROR,"putenv(%s) failed",env_var);
}

/********************************************************************************/

char *getenv_var ( const char *env_var )

/* read a value from the local process environment table. Should handle any	*/
/* systemic differences required.						*/

{ 
return(getenv(env_var));
}

/********************************************************************************/

pid_t unix_spawn ( char *argv[] )

/* this function forks a new process, the new child process is used to carry	*/
/* out the commands specified in argv, and the argv arguments list. Full stdin	*/
/* stdout and stderr piping is provided.					*/

{ int		i;
  pid_t		pid;
  struct rlimit rlp;


pipe(pipe_io[0]);		/* -- set up input pipe */
pipe(pipe_io[1]);		/* -- set up output pipe */

switch (pid = fork())
    {
    case -1 :				/* -- fork fails */
	close(pipe_io[0][0]);
	close(pipe_io[0][1]);
	close(pipe_io[1][0]);
	close(pipe_io[1][1]);
	perror("FSPtool:fork");
	exit(-1);

    case 0 :				/* -- child */
	dup2(pipe_io[0][0], 0);
	dup2(pipe_io[1][1], 1);
	dup2(pipe_io[1][1], 2);

	if (getrlimit(RLIMIT_NOFILE,&rlp) != 0) {
	    perror("FSPtool:getrlimit");
	    exit(-1);
	    }

	for ( i = (int) rlp.rlim_cur; i > 2; i--)
	    (void) close(i);

	for (i = 0; i < NSIG; i++)
	    (void) signal(i, SIG_DFL);

	execvp(*argv,argv);

	if (errno == ENOENT)
	    fprintf(stderr,"FSPtool: %s not found\n",argv[0]);
	else
	    perror("FSPtool:execvp");

	_exit(-1);

    default:				/* -- parent */
	close(pipe_io[0][0]);
	close(pipe_io[1][1]);
    }

return(pid);
}

/********************************************************************************/

pid_t unix_command ( char *argv[] )

/* this function forks a new process, the new child process is used to carry	*/
/* out the commands specified in argv, and the argv arguments list. No new I/O	*/
/* pipes set up for these commands however.					*/

{ int		i;
  pid_t		pid;
  struct rlimit rlp;


switch (pid = fork())
    {
    case -1 :				/* -- fork fails */
	perror("FSPtool:fork");
	exit(-1);

    case 0 :				/* -- child */
	if (getrlimit(RLIMIT_NOFILE,&rlp) != 0) {
	    perror("FSPtool:getrlimit");
	    exit(-1);
	    }

	for ( i = (int) rlp.rlim_cur; i > 2; i--)
	    (void) close(i);

	for (i = 0; i < NSIG; i++)
		(void) signal(i, SIG_DFL);

	execvp(*argv,argv);

	if (errno == ENOENT)
	    fprintf(stderr,"FSPtool: %s not found\n",argv[0]);
	else
	    perror("FSPtool:execvp");

	_exit(-1);

    default:				/* -- parent */
	break;
    }

return(pid);
}

/********************************************************************************/

void initialise_fsptool_lib(void)

/* this fn should be called by apps linked to fsptoollib prior to use of any	*/
/* of the routines within it. Initialises error handlers.			*/

{
error_handler = default_error_handler;
}

/********************************************************************************/

void register_error_handler ( void (*error_routine)(int,...) )

/* this fn register an error_handler which library routines will use.		*/
/* error_handler takes a variable argument list. First item should always be	*/
/* an int indicating severity of error - according to defines in common.h	*/

{
error_handler = error_routine;
}

/********************************************************************************/

void default_error_handler ( int error_level, ... )

/* the default error handler. For all conditions it outputs given string to	*/
/* stderr. On warning and for errors it then returns. For fatal errors it	*/
/* does an exit(1) call and terminates the application.				*/

{ char    *fmt;
  va_list  args;

va_start(args, error_level);
fmt = va_arg(args,char *);
(void) fprintf(stderr, "FSPtool: ");
(void) vfprintf(stderr, fmt, args);
(void) fprintf(stderr, "\n");
va_end(args);

if (error_level == SERIOUS_ERROR)
    exit(1);
}

/********************************************************************************/

char **string_to_args ( const char *str, const char *filename )

/* this fn takes a whitespaced string and splits it into indivdual arguments	*/
/* for passing to execvp, if %f is found in string then replaces this with	*/
/* filename arg, otherwise filename will be appended to the end.		*/

{ static char *blk_ptrs[256];	/* -- limit of 256 args, should be enough :-)	*/
  static char  nstr[1024];
	  int  cnt = 0, flnm_placed = FALSE;

strcpy(nstr,str);

if ((blk_ptrs[cnt] = strtok(nstr," ")) == NULL) {
    blk_ptrs[cnt++] = (char*)filename;
    blk_ptrs[cnt]   = (char*)NULL;
    return(&(blk_ptrs[0]));
    }

if (strcmp(blk_ptrs[cnt],"%f") == 0) {
    blk_ptrs[cnt] = (char*)filename;
    flnm_placed = TRUE;
    }

do
    {
    blk_ptrs[++cnt] = strtok(NULL," ");

    if (blk_ptrs[cnt] && (strcmp(blk_ptrs[cnt],"%f") == 0)) {
	blk_ptrs[cnt] = (char*)filename;
	flnm_placed = TRUE;
	}
    }
while (blk_ptrs[cnt]);

if (!flnm_placed) {
    blk_ptrs[cnt] = (char*)filename;
    blk_ptrs[++cnt] = (char*)NULL;
    }

return(&(blk_ptrs[0]));
}

/********************************************************************************/
