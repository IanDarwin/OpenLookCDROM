/********************************************************************************/
/* xview/system.h --								*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtool_SYSTEM_H_
#define _FSPtool_SYSTEM_H_ 1

/********************************************************************************/

#if defined(SVR4) || defined(SYSV)
Notify_value sigcatcher_decompress(Notify_client,int,int*,struct rusage*);
Notify_value sigcatcher_noio(Notify_client,int,int*,struct rusage*);
Notify_value sigcatcher(Notify_client,int,int*,struct rusage*);
Notify_value sigcatcher_null(Notify_client,int,int*,struct rusage*);
Notify_value batch_sigcatcher(Notify_client,int,int*,struct rusage*);
#else
Notify_value sigcatcher_decompress(Notify_client,int,union wait*,struct rusage*);
Notify_value sigcatcher_noio(Notify_client,int,union wait*,struct rusage*);
Notify_value sigcatcher(Notify_client,int,union wait*,struct rusage*);
Notify_value sigcatcher_null(Notify_client,int,union wait*,struct rusage*);
Notify_value batch_sigcatcher(Notify_client,int,union wait*,struct rusage*);
#endif

extern int do_command(char*[]);
extern int do_decompress(char*[]);
extern int do_spawn(char*[],void*);
extern int do_batch_spawn(char*[],void*);
extern int do_spawn_put(char*[],void*);

/********************************************************************************/
#endif
