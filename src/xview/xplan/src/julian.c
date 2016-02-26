/* FILE julian.c
 *
 * xplan - project planning tool
 * Copyright (C) 1992 Brian Gaubert, Mark M. Lacey, Richard Malingkas,
 * and Mike Marlow.
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License (distributed with this program in the file
 * COPYING) for more details.
 * 
 * If you did not received a copy of the GNU General Public License
 * along with this program, write to the Free Software Foundation,
 * Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 * 
 * Since this was a project for a one semester software engineering
 * course, the authors will not be offering support for the product
 * after its release.
 *
 * DESCRIPTION OF CONTENTS
 *
 * Functions to manipulate dates.  Unfortunately nobody is certain of
 * the origin of the first two, and we have no references.
 *
 */

#include <stdio.h>
#include "julian.h"

static char buffer[50];

struct date julian_to_date(int jdate)
{
   int m1,y1,m2,d2,y2;
   int x,y,d,m,e;
   struct date temp_date;
   
   x = 4 * jdate - 6884477;
   y = (x / 146097) * 100;
   e = x % 146097;
   d = e / 4;
   
   x = 4 * d + 3;
   y = (x / 1461) + y;
   e = x % 1461;
   d = e / 4 + 1;
   
   x = 5 * d - 3;
   m = x / 153 + 1;
   e = x % 153;
   d = e / 5 + 1;
   
   if( m < 11 )
     temp_date.month = m + 2;
   else
     temp_date.month = m - 10;
   
   
   temp_date.day = d;
   temp_date.year = y + m / 11;
   return (temp_date);
}


int date_to_julian(int month,int day,int year)
{
   int m1,y1,m2,d2,y2;
   long int jdate,a,b,c,d,j1;
   
   if (year < 100)
     year = year + 1900;
   
   if (month > 2)
     {
	m1 = month - 3;
	y1 = year;
     }
   else
     {
	m1 = month + 9;
	y1 = year - 1;
     }
   a = 146097*(y1/100)/4;
   d = y1 % 100;
   b = 1461*d/4;
   c = (153*m1+2)/5+day+1721119;
   j1 = a+b+c;
   return(j1);
}

/* take the string date and convert it to julian */
int str_date_to_julian(char *str)
{
   int m, d, y;

   /* empty string */
   if (strtok(str, " ")==NULL) return 0;

   if (sscanf(str, "%d-%d-%d", &m, &d, &y)!=3) {
      if (sscanf(str, "%d/%d/%d", &m, &d, &y)!=3) {
	 return 0;
      } else {
	 return date_to_julian(m, d, y);
      }
   }
   else {
      return date_to_julian(m, d, y);
   }
}

/* take a julian date and convert it to a string format */
char *julian_to_str_date(int jdate)
{
   struct date newdate;

   if (jdate <= 0) {
      buffer[0]='\0';
   } else {
      newdate = julian_to_date(jdate);
      if (newdate.year > 1900) newdate.year -= 1900;
      
      sprintf(buffer, "%d/%d/%d", newdate.month, newdate.day,
	      newdate.year);
   }
   return buffer;
}

