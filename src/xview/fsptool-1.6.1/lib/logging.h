/********************************************************************************/
/* logging.h --									*/
/*										*/
/* Author : A.J.Doherty								*/
/********************************************************************************/

#ifndef _FSPtoollib_LOGGING_H_
#define _FSPtoollib_LOGGING_H_ 1

/********************************************************************************/

extern void log_startup(void);
extern void log_exit(void);
extern void log_transfer(const char*,long int);
extern void log_directory(const char*);

/********************************************************************************/
#endif

