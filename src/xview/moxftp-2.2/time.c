/*
 * Copyright (c) 1992 The Regents of the University of Texas System.
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: time.c,v 1.1 1994/03/14 18:55:53 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/time.c,v $\n";
#endif

/* $Log: time.c,v $
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include <time.h>

#if !defined(timemaker)
#if defined(sun)&&!defined(SVR4)
#define timemaker timegm
#else
#define timemaker mktime
#endif
#else
time_t timemaker();
#endif



static struct {
     char *month, number;
} months[] = {
	{ "january"  ,  0},
	{ "febuary"  ,  1},
	{ "march"    ,  2},
	{ "april"    ,  3},
	{ "may"      ,  4},
	{ "june"     ,  5},
	{ "july"     ,  6},
	{ "august"   ,  7},
	{ "september",  8},
	{ "october"  ,  9},
	{ "november" , 10},
	{ "december" , 11}
};
	
time_t 
get_unix_time(day, time_year, month)
char *day;
char *time_year;
char *month;
{
     struct tm tm, nowtm;
     time_t now;

     ZERO((char *)&tm, sizeof(tm));

     time(&now);
     nowtm = *localtime(&now);
#if defined(GMTOFF)
     tm.tm_gmtoff = nowtm.tm_gmtoff;
#endif
     
     /* 
      * Set month
      */
     get_month(&tm, &nowtm, month);
     /*
      * Set day.
      */
     get_day(&tm, &nowtm, day);
     /*
      * Set year or time;
      */
     get_year_time(&tm, &nowtm, time_year);
     now = timemaker(&tm);
     return now;
}

get_month(tm, nowtm, month)
struct tm *tm, *nowtm;
char *month;
{
     char temp[1000];
     int i,n;

     if (!month)  return;

     strcpy(temp, month);
     Lower(temp);
     n = strlen(temp);
     for (i=0; i<12; i++) {
	if (strncmp(temp, months[i].month, n) == 0) {
		tm->tm_mon   = months[i].number;
		if (tm->tm_mon <= nowtm->tm_mon) {
		    tm->tm_year  = nowtm->tm_year;
		} else {
		    tm->tm_year  = nowtm->tm_year - 1;
		}
		return;
	}
     }
}
	
get_day(tm, nowtm, day)
struct tm *tm, *nowtm;
char *day;
{
    int daynum;

    if (!day) return;
    daynum = atoi(day);
    if (daynum < 0 || daynum > 32) return;
    tm->tm_mday = daynum;
}

get_year_time(tm, nowtm, year_time)
struct tm *tm, *nowtm;
char *year_time;
{
     char *cp;
     char temp[8];
     int  year;

     strcpy(temp, year_time);
     if ((cp = INDEX(temp,':')) && (int)strlen(year_time) == 5) {
	/*
	 * Time of day.
	 */
	*cp = '\0';
	cp++;
	tm->tm_hour = atoi(temp);
	tm->tm_min  = atoi(cp);
     } else {
	/* 
	 * Must be year 
	 */
	year = atoi(year_time);
	year = year - 1900;
	if (year < 0) return;
	if (year > nowtm->tm_year) return;
	tm->tm_year = year;
    }
}

time_t 
get_remote_time(r_date, r_time)
char *r_date;
char *r_time;
{
    struct tm tm, nowtm;
    time_t now;

    ZERO((char *)&tm, sizeof(tm));

    time(&now);
    nowtm = *localtime(&now);
#if defined(SUN)
     tm.tm_gmtoff = nowtm.tm_gmtoff;
#endif
    /*
     * Parse date;
     */
    if(get_vms_date(&tm, &nowtm, r_date) == 0)  {
	get_vms_time(&tm, &nowtm, r_time);
    } else if (get_vm_date(&tm, &nowtm, r_date) == 0) {
	get_vm_time(&tm, &nowtm, r_time);
    }else if (get_tops20_date(&tm, &nowtm, r_date) == 0)  {
	get_tops20_time(&tm, &nowtm, r_time);
    } else {
	return 0;
    }

    now = timemaker(&tm);
    if (now >= 0) return now;
    return  0;
	
}

get_vms_date(tm, nowtm, r_date)
struct tm *tm, *nowtm;
char *r_date;
{
    char temp[12];
    int day;
    int month;
    int year;
    int i, n;
    char *cp;
    char *cp1;

    if (!r_date) return 1;
    if ((int)strlen(r_date) < 10) return 1;
    if (!((int)	strlen(r_date) == 11 &&
	  r_date[2] == '-' && r_date[6] == '-') &&
        !((int)strlen(r_date) == 10 &&
  	  r_date[1] == '-' && r_date[5] == '-') ) {
	 return 1;
    }
    strcpy(temp, r_date);
    Lower(temp);
    cp = INDEX(temp, '-');
    *cp = 0;
    day = atoi(temp);
    if (day < 0 || day > 32) return 1;
    cp++;
    cp1 = cp;
    cp = INDEX(cp, '-');
    *cp = 0;
    n = strlen(cp1);
    for (i=0; i<12; i++) {
	if (strncmp(cp1, months[i].month, n) == 0) {
	    month   = months[i].number;
	    break;
	}
    }
    if (i == 12) return 1;
    cp++;
    year = atoi(cp) - 1900;
    if (year < 0 || year > nowtm->tm_year) return 1;
    tm->tm_mday = day;
    tm->tm_year = year;
    tm->tm_mon  = month;
    return 0;
}
   

get_vms_time(tm, nowtm, r_time)
struct tm *tm, *nowtm;
char *r_time;
{
    char temp[7];

    if (!r_time || (int)strlen(r_time) != 5 || r_time[2] != ':') return; 
    strcpy(temp, r_time);
    temp[2] = '\0';
    tm->tm_hour = atoi(temp);
    tm->tm_min  = atoi(&temp[3]);
}


get_vm_date(tm, nowtm, r_date)
struct tm *tm, *nowtm;
char *r_date;
{
    char temp[12];
    int day;
    int month;
    int year;
    int i, n;
    char *cp;
    char *cp1;

    if (!r_date) return 1;
    if ((int)strlen(r_date) < 7) return 1;
    if (!((int)strlen(r_date) == 8 &&
	  r_date[2] == '/' && r_date[5] == '/') &&
        !((int)strlen(r_date) == 7 && 
  	  r_date[1] == '/' && r_date[4] == '/') ) {
	 return 1;
    }
    strcpy(temp, r_date);
    Lower(temp);
    cp = INDEX(temp, '/');
    *cp = 0;
    month = atoi(temp) - 1;
    if (month < 0 || month > 11) return 1;
    cp++;
    day = atoi(cp);
    if (day < 0 || day > 32) return 1;
    cp = INDEX(cp, '/');
    cp++;
    year = atoi(cp);
    if (year < 0 || year > nowtm->tm_year) return 1;
    tm->tm_mday = day;
    tm->tm_year = year;
    tm->tm_mon  = month;
    return 0;
}
   

get_vm_time(tm, nowtm, r_time)
struct tm *tm, *nowtm;
char *r_time;
{
    char temp[7];

    if (!r_time || (int)strlen(r_time) != 8 || r_time[2] != ':' ||
	 r_time[5] != ':') return;
    strcpy(temp, r_time);
    temp[2] = '\0';
    temp[5] = '\0';
    tm->tm_hour = atoi(temp);
    tm->tm_min  = atoi(&temp[3]);
    tm->tm_sec  = atoi(&temp[6]);
}



get_tops20_date(tm, nowtm, r_date)
struct tm *tm, *nowtm;
char *r_date;
{
    char temp[12];
    int day;
    int month;
    int year;
    int i, n;
    char *cp;
    char *cp1;

    if (!r_date) return 1;
    if ((int)strlen(r_date) < 8) return 1;
    if (!((int)strlen(r_date) == 9 &&
	  r_date[2] == '-' && r_date[6] == '-') &&
        !((int)strlen(r_date) == 8 &&
  	  r_date[1] == '-' && r_date[5] == '-') ) {
	 return 1;
    }
    strcpy(temp, r_date);
    Lower(temp);
    cp = INDEX(temp, '-');
    *cp = 0;
    day = atoi(temp);
    if (day < 0 || day > 32) return 1;
    cp++;
    cp1 = cp;
    cp = INDEX(cp, '-');
    *cp = 0;
    n = strlen(cp1);
    for (i=0; i<12; i++) {
	if (strncmp(cp1, months[i].month, n) == 0) {
	    month   = months[i].number;
	    break;
	}
    }
    if (i == 12) return 1;
    cp++;
    year = atoi(cp);
    if (year < 0 || year > nowtm->tm_year) return 1;
    tm->tm_mday = day;
    tm->tm_year = year;
    tm->tm_mon  = month;
    return 0;
}
   

get_tops20_time(tm, nowtm, r_time)
struct tm *tm, *nowtm;
char *r_time;
{
    get_vm_time(tm, nowtm, r_time);
}
