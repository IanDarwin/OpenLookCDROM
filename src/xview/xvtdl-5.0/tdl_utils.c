/*
 * $Id: tdl_utils.c,v 1.2 1992/10/06 11:37:54 jipping Exp $
 * *********************************************************************
 *
 * Tdl_Utils.c --> Routines for printing todo lists.
 *    Basically, stripped versions of the same routines for XVTDL.
 *
 * ----------------------------------------------------------------------
 * Copyright (c) 1993 by Mike Jipping and Hope College
 *  
 * Permission is granted to copy and distribute this file in modified or
 * unmodified form, for noncommercial use, provided (a) this copyright notice
 * is preserved, (b) no attempt is made to restrict redistribution of this
 * file, and (c) this file is not distributed as part of any collection whose
 * redistribution is restricted by a compilation copyright.
 * ----------------------------------------------------------------------
 *
 * Revision History:
 *
 * $Log: tdl_utils.c,v $
 * Revision 1.2  1992/10/06  11:37:54  jipping
 * Fixes after beta test: The "&" in passwd gecos is now replaced with
 * login name.
 *
 * Revision 1.1  1992/09/15  12:16:52  jipping
 * Initial revision
 *
 *
 */

#include "globaldefs.h"

extern struct day_entry_list *delhead, *delcurr, *delprev;
extern int position[10];
extern FILE *yyin;
struct category_rec *new_category();

/*
 * **********************************************************************
 * This routine lists the todo list for the category "cr", the user
 * "name" and the date "title" This includes recurring and non-recurring
 * entries.  The list is sorted as specified by criteria.
 */
list_category (cr,name,title)
struct category_rec *cr;
char *name, *title;
{
   int datecode, count;

   /*
    *  Clear the position tally for sorting the display.  Then clear
    *  the display.
    */
   for (count=0; count<10; count++) position[count] = -1;
   clear_display();

   /*
    *  Compute the correct datecode.
    */
   datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;

   /*
    * If we have one, display its entries....
    */
   fill_display_with_category(cr, datecode);
	sort_display_list(delhead, delcurr, 1);

	/* 
	 *  And, finally, print the list.
    */
	printf("*** TO DO LIST ***\n");
	printf("      for %s\n", name);
	printf("      Category: %s\n", cr->name);
	printf("      Date: %s\n\n\n", title);
	display_display_list();

}

/*
 * **********************************************************************
 * This routine lists the todo list all categories, user "name" and the
 * date "title" This includes recurring and non-recurring entries.  The
 * list is sorted as specified.
 */
list_all_cats (name,title)
char *name, *title;
{
	char txt[80];
   int datecode, count;
   struct category_rec *cr, *tcr;

	/*
	 *  Clear the position tally for sorting the display, and the 
	 *  display itself.
	 */
	for (count=0; count<10; count++) position[count] = -1;
	clear_display();
      
	/*
	 *  compute the correct datecode and find the entry list for that
	 *  date.
	 */
	datecode = (curr_year-1990)*10000 + curr_month*100 + curr_day;
	
	cr = category_head;
	while (cr != NULL) {
		fill_display_with_category(cr, datecode);
		if (cr->subcats != NULL) {
			for (tcr=cr->subcats; tcr != NULL; tcr=tcr->next)
				fill_display_with_category(tcr, datecode);
		}
		cr = cr->next;
	}
	sort_display_list(delhead, delcurr, 1);
		
	/* 
	 *  And, finally, contruct the list.
	 */
	printf("*** TO DO LIST ***\n");
	printf("      for %s\n", name);
	printf("      Category: ALL CATEGORIES\n");
	printf("      Date: %s\n\n\n", title);
	display_display_list();

}

/*
 * **********************************************************************
 * Driver for the listing routines.  Does some preprocessing, then calls
 * the correct routine based on the command line settings.
 */
void show_list(category, datecode, do_all)
struct category_rec *category;
int datecode, do_all;
{
	char *temp_file;
	int ps;
	float scale_factor;
	struct category_rec *cr;
	char day_title[50], personname[80];
	char printercmd[120];
	char *amp, *gecos, *comma;
	struct tm *now;
   struct timeval tv;
	struct passwd *pwd;

   /*
    * Get set up.  Set up the time variables, and set up the file 
    * header.
    */
   now = localtime(&tv.tv_sec);
	now->tm_mon = curr_month - 1;
	now->tm_mday = curr_day;
	now->tm_year = curr_year-1900;
	now->tm_wday = zeller(curr_month, curr_day, curr_year);
	strftime(day_title, 50, "%A, %B %e, %Y", now);

	pwd = getpwuid(getuid());
	if (pwd->pw_name[0] > 96) pwd->pw_name[0] -= 32;

	personname[0] = '\0';
	gecos = malloc(80*sizeof(char));
	strcpy(gecos, pwd->pw_gecos);
	comma = strchr(gecos, ',');
	if (comma != NULL) *comma = '\0';
	amp = strchr(gecos, '&');
	if (amp == NULL) {
		strcpy(personname, gecos);
	} else {
		while (amp != NULL) {
			*amp++ = '\0';
			strcat(personname, gecos);
			strcat(personname, pwd->pw_name);
			gecos = amp;
			amp = strchr(gecos, '&');
		}
	}

	if (do_all) {
		cr = category_head;
		while (cr != NULL) {
			list_category(cr,personname,day_title);
			cr = cr->next;
		}
	} else if (category == NULL) {
		list_all_cats(personname,day_title);
	} else {
		list_category(category,personname,day_title);
	}
}

/*
 * **********************************************************************
 * Does a small dialog with the user to add an entry to the todo list.
 *
 */
void add_dialog (category)
struct category_rec *category;
{
	char linein[LINESIZ];
	char text[LINESIZ];
	int priority;
	
	printf("*** Adding an entry ***\n\n");

	printf("Please enter the entry's text: ");
	fflush(stdout);
	gets(linein);
	strcpy(text, linein);
	
	printf("Please enter the entry's priority: ");
	fflush(stdout);
	gets(linein);
	priority = atoi(linein);

	add_to(category, curr_month, curr_day, curr_year, text, priority);
}


/*
 * **********************************************************************
 * Utility routine...copying the contents of the file denoted by "str"
 * into the file denoted by the descriptor "fd".
 */

void copyfile2(str, fd)
char *str;
FILE *fd;
{
	FILE    *spfp;
	int     t;
	
	if((spfp=fopen(str,"r")) == NULL) {
		fprintf(stderr,"Unable to open file %s\n",str);
		return;
	}
	while ((t=getc(spfp)) != EOF ) {
		putc(t, fd);
	}
	fclose(spfp);
}

/*
 * **********************************************************************
 * Driver routine for printing lists.  Ripped from "control.c"
 *
 * Note that the current version depends on two prolog files for printing
 * in PostScript.  These must be set up in the "globaldefs.h" file, and 
 * are denoted by the PROLOG1 and PROLOG2 macros.
 */
void print_list(cr, do_all, ps, incl_checked, scale_factor, printer)
struct category_rec *cr;
int do_all, ps, incl_checked, scale_factor;
char *printer;
{
	char *temp_file, new_file[80];
	FILE *tmp;
	char day_title[50], personname[80];
	char printercmd[120];
	char *amp, *gecos, *comma;
	struct tm *now;
   struct timeval tv;
	struct passwd *pwd;

	/* 
    *  Open the temp file.
    */
   if ( (temp_file = (char *)tempnam(NULL, "tdl")) == NULL) {
		fprintf(stderr, "Unable to create temporary file name\n");
		return;
	}
	if ( (tmp = fopen(temp_file, "w")) == NULL) {
		fprintf(stderr, "Unable to open temp file %s\n", temp_file);
		free(temp_file);
		return;
	}

   /*
    * Get set up.  Set up the time variables, and set up the file 
    * header.
    */
   now = localtime(&tv.tv_sec);
	now->tm_mon = curr_month - 1;
	now->tm_mday = curr_day;
	now->tm_year = curr_year-1900;
	now->tm_wday = zeller(curr_month, curr_day, curr_year);
	strftime(day_title, 50, "%A, %B %e, %Y", now);

	pwd = getpwuid(getuid());
	if (pwd->pw_name[0] > 96) pwd->pw_name[0] -= 32;

	personname[0] = '\0';
	gecos = malloc(80*sizeof(char));
	strcpy(gecos, pwd->pw_gecos);
	comma = strchr(gecos, ',');
	if (comma != NULL) *comma = '\0';
	amp = strchr(gecos, '&');
	if (amp == NULL) {
		strcpy(personname, gecos);
	} else {
		while (amp != NULL) {
			*amp++ = '\0';
			strcat(personname, gecos);
			strcat(personname, pwd->pw_name);
			gecos = ++amp;
			amp = strchr(gecos, '&');
		}
	}

	if (ps) {
		copyfile2(PRINT_PROLOG1, tmp);
		fprintf(tmp, "%f %f 612 792 0 1 3 FMDOCUMENT\n", scale_factor/100.0, scale_factor/100.0);
		fprintf(tmp, "/fillprocs 32 array def\n");
		fprintf(tmp, "fillprocs 0 { 0.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 1 { 0.100000 grayness } put\n");
		fprintf(tmp, "fillprocs 2 { 0.300000 grayness } put\n");
		fprintf(tmp, "fillprocs 3 { 0.500000 grayness } put\n");
		fprintf(tmp, "fillprocs 4 { 0.700000 grayness } put\n");
		fprintf(tmp, "fillprocs 5 { 0.900000 grayness } put\n");
		fprintf(tmp, "fillprocs 6 { 0.970000 grayness } put\n");
		fprintf(tmp, "fillprocs 7 { 1.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 8 {<0f1e3c78f0e1c387> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 9 {<0f87c3e1f0783c1e> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 10 {<cccccccccccccccc> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 11 {<ffff0000ffff0000> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 12 {<8142241818244281> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 13 {<03060c183060c081> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 14 {<8040201008040201> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 15 {} put\n");
		fprintf(tmp, "fillprocs 16 { 1.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 17 { 0.900000 grayness } put\n");
		fprintf(tmp, "fillprocs 18 { 0.700000 grayness } put\n");
		fprintf(tmp, "fillprocs 19 { 0.500000 grayness } put\n");
		fprintf(tmp, "fillprocs 20 { 0.300000 grayness } put\n");
		fprintf(tmp, "fillprocs 21 { 0.100000 grayness } put\n");
		fprintf(tmp, "fillprocs 22 { 0.030000 grayness } put\n");
		fprintf(tmp, "fillprocs 23 { 0.000000 grayness } put\n");
		fprintf(tmp, "fillprocs 24 {<f0e1c3870f1e3c78> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 25 {<f0783c1e0f87c3e1> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 26 {<3333333333333333> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 27 {<0000ffff0000ffff> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 28 {<7ebddbe7e7dbbd7e> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 29 {<fcf9f3e7cf9f3f7e> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 30 {<7fbfdfeff7fbfdfe> 8 1 setpattern } put\n");
		fprintf(tmp, "fillprocs 31 {} put\n");
		fprintf(tmp, "%%%%EndSetup\n");
		fprintf(tmp, "0 18 /NewCenturySchlbk-Bold FMDEFINEFONT\n");
		fprintf(tmp, "1 14 /NewCenturySchlbk-Italic FMDEFINEFONT\n");
		fprintf(tmp, "2 9 /NewCenturySchlbk-Roman FMDEFINEFONT\n");
		fprintf(tmp, "%%%%BeginPaperSize: Letter\n");
		fprintf(tmp, "%%%%EndPaperSize\n");
	}

	delhead = delcurr = delprev = NULL;
	
	if (cr == NULL) {
		print_all(tmp,ps,incl_checked,(float)scale_factor,personname,day_title);
	} else if (do_all) {
		cr = category_head;
		while (cr != NULL) {
			print_category(cr,tmp,ps,incl_checked,(float)scale_factor,personname,day_title);
			cr = cr->next;
		}
	} else {
		print_category(cr,tmp,ps,incl_checked,(float)scale_factor,personname,day_title);
	}

	/*
    *  And we're done.
    */
	fclose(tmp);
#ifdef SVR4		
		sprintf(printercmd, "lp -d %s %s", printer, temp_file);
#else
		sprintf(printercmd, "lpr -P%s %s", printer, temp_file);
#endif
	system(printercmd);

	unlink(temp_file);
	free(temp_file);
}

/*
 * **********************************************************************
 * This routine implements the "-categories" option.  It recursively
 * lists all the categories in the todo database specified.
 */
void list_categories (cr, level)
struct category_rec *cr;
{
	char printfline[256];

	while (cr != NULL) {
		sprintf(printfline, "%%%dc%%s\n", level*4);
		printf(printfline, ' ', cr->name);
		if (cr->subcats != NULL) {
			list_categories(cr->subcats, level+1);
		}
		cr = cr->next;
	}
	
}
