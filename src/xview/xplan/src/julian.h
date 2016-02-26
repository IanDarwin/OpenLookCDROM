#ifndef _julian_h_
#define _julian_h_

struct date {
  int year;
  int month;
  int day;
};

struct date julian_to_date(int);
int date_to_julian(int,int,int);
int str_date_to_julian(char *);
char *julian_to_str_date(int);

#endif
