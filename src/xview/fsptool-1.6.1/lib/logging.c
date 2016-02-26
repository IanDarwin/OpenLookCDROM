/********************************************************************************/
/* logging.c --									*/
/*										*/
/* Logging code for FSPtool. Shouldn't normally be required, mainly included	*/
/* to resolve problems with "problem" file transfers and users.			*/
/*										*/
/* (C)1993 A.J.Doherty								*/
/********************************************************************************/

#include "common.h"
#include "logging.h"
#include "cache.h"
#include "fsp.h"

/********************************************************************************/

#include <stdio.h>
#include <time.h>
#include <string.h>

#include <sys/types.h>
#include <sys/time.h>

/********************************************************************************/

static char	  username[256];
static struct tm *timeinfo;
static time_t	  sectime;
static char 	 *logfilenm = LOGFILE;

/********************************************************************************/

void log_startup()

/* write logging information to log file, username time and date at which tool	*/
/* was started.									*/

{ FILE *logfile;

if ((logfile = fopen(logfilenm,"a")) == NULL)
    return;

if (cuserid(username)) {
    sectime  = time((time_t*)NULL);
    timeinfo = localtime(&sectime);
    fprintf(logfile,"%02d/%02d/%02d %02d:%02d:%02d %s : FSPTOOL STARTED\n",
			timeinfo->tm_mday, timeinfo->tm_mon, timeinfo->tm_year,
			timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec,
			username);
    }

fclose(logfile);
}

/********************************************************************************/

void log_exit()

/* write end of FSPtool logging data to the logging file.			*/

{ FILE *logfile;

if ((logfile = fopen(logfilenm,"a")) == NULL)
    return;

if (cuserid(username)) {
    sectime  = time((time_t*)NULL);
    timeinfo = localtime(&sectime);
    fprintf(logfile,"%02d/%02d/%02d %02d:%02d:%02d %s : FSPTOOL EXITED\n",
			timeinfo->tm_mday, timeinfo->tm_mon, timeinfo->tm_year,
			timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec,
			username);
    }

fclose(logfile);
}

/********************************************************************************/

void log_transfer ( const char *filename, long int fsize )

/* log a file transfer, write filename, user and date/time			*/

{ static char  filenm[1024];
  FILE	      *logfile;


if ((logfile = fopen(logfilenm,"a")) == NULL)
    return;

if (cuserid(username) && (strcmp(username,"ssudoher") != 0)) {
    sectime  = time((time_t*)NULL);
    timeinfo = localtime(&sectime);

    strcpy(filenm,get_fsp_dir());

    if (strcmp(filenm,"/") != 0)
	strcat(filenm,"/");

    strcat(filenm,filename);

    fprintf(logfile,"%02d/%02d/%02d %02d:%02d:%02d %s : FILE %s:%s %s (%1ld)\n",
			timeinfo->tm_mday, timeinfo->tm_mon, timeinfo->tm_year,
			timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec,
			username,
			get_fsp_host(), get_fsp_port(),
			filenm, fsize);
    }

fclose(logfile);
}

/********************************************************************************/

void log_directory ( const char *dirname )

/* log a directory listing, write directory name, user and date/time		*/

{ FILE *logfile;

if ((logfile = fopen(logfilenm,"a")) == NULL)
    return;

if (cuserid(username) && (strcmp(username,"ssudoher") != 0)) {
    sectime  = time((time_t*)NULL);
    timeinfo = localtime(&sectime);
    fprintf(logfile,"%02d/%02d/%02d %02d:%02d:%02d %s : DIR  %s:%s %s\n",
			timeinfo->tm_mday, timeinfo->tm_mon, timeinfo->tm_year,
			timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec,
			username, get_fsp_host(), get_fsp_port(), dirname);
    }

fclose(logfile);
}

/********************************************************************************/
