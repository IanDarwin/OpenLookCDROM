/* FILE latepert.c ****************************************
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
 * Functions to generate LaTeX picture environment code for PERT
 * charts.  It is not complete, as it does not draw the dependency
 * lines.
 *  
 */   

#include <stdio.h>
#include <math.h>
#include "db.h"
#include "pagelist.h"
#include "latpert.h"

extern char export_filename[];

int gcd(int, int);

void strfix(char []);
void latex_line_scale(int, int, int *, int *);

void calculate_latex_pert_pages(void);
void calculate_latex_pert_lines(void);
void write_pert_chart(void);
void breaklines(int startx, int starty, int endx, int endy, int cp);

struct page_list *list;

/* FUNCTION generate_latex_pert_pages ************************************

   PURPOSE

   Generates LaTeX representation of a PERT chart.
   
   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 9 Dec 1992
   Tester.... Mark M. Lacey, 9 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void generate_latex_pert_pages()
{
   struct task_node *node;
   int      
     large_x = 0,
     large_y = 0;

   /* fill in formatting information */
   traverse_task_list();
   /* fill in critical path information */
   calc_critical();

   /* get head of list */
   node = get_main_task_list()->head;

   /* find the largest x and y coordinates */
   while (node) {
      if (node->data->x_pert > large_x) large_x = node->data->x_pert;
      if (node->data->y_pert > large_y) large_y = node->data->y_pert;
      node = node->next;
   }

   /* create a page list (see pagelist.c) */
   list = create_page_list((large_x-1)/PERT_BOXES_ACROSS+1, 
			   (large_y)/PERT_BOXES_DOWN+1);


   /* find the positions of the pages */
   calculate_latex_pert_pages();
   /* find the positions of the lines */
   calculate_latex_pert_lines();
   /* write the LaTeX file for the chart */
   write_pert_chart();
}

/* FUNCTION calculate_latex_pert_pages ************************************

   PURPOSE

   Finds the positions of the boxes in the PERT chart.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 9 Dec 1992
   Tester.... Mark M. Lacey, 9 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void calculate_latex_pert_pages()
{
   struct 
     task_node *node;
   struct
     object_node *onode;
   int 
     across,
     down,
     gridx,
     gridy,
     viewx,
     viewy;
   
   node = get_main_task_list()->head;

   /* fill our list of pages with information */
   while (node) {
      across = (node->data->x_pert-1) / PERT_BOXES_ACROSS + 1;
      down = (node->data->y_pert) / PERT_BOXES_DOWN + 1;

      gridx = (node->data->x_pert-1) - ((across-1)*PERT_BOXES_ACROSS);
      viewx = gridx * (LATEX_PERT_BOX_WIDTH+LATEX_PERT_BOX_X_SKIP)-
	LATEX_PERT_BOX_X_SKIP;

      gridy = (node->data->y_pert) - ((down-1)*PERT_BOXES_DOWN);
      viewy = (PAGE_HEIGHT-LATEX_PERT_BOX_HEIGHT)-
	gridy * (LATEX_PERT_BOX_HEIGHT+LATEX_PERT_BOX_Y_SKIP);

      onode = create_object_node(PERT_BOX, 
				 viewx,
				 viewy,
				 0, 0, 0, 0, 0,
				 node->data->critical_path,
				 node->data->name, NULL);
      add_object_to_page(list, across, down, onode);

      node = node->next;
   }
}

/* FUNCTION calculate_latex_pert_pages ************************************

   PURPOSE

   Finds the positions of the lines in the PERT chart.

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 9 Dec 1992
   Tester.... Mark M. Lacey, 9 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void calculate_latex_pert_lines()
{
   struct task_node 
     *node, *deps;
   struct
     object_node *onode;
   int
     across, down,
     end_page_across,
     end_page_down,
     endgridx, endgridy,
     endx, endy,
     startx, starty,
     gridx, viewx,
     gridy, viewy,
     endviewx, endviewy;

   char blah[100];

   node = get_main_task_list()->head;

   /* fill our list of pages with information */
   while (node) {
      startx = (node->data->x_pert)*
	(LATEX_PERT_BOX_WIDTH+LATEX_PERT_BOX_X_SKIP)-
	  LATEX_PERT_BOX_X_SKIP;
      starty = (node->data->y_pert+1)*
	(LATEX_PERT_BOX_HEIGHT+LATEX_PERT_BOX_Y_SKIP)-
	  LATEX_PERT_BOX_Y_SKIP;

      /* line drawing code not working
      deps = node->data->dependents->head;

      while (deps) {
	 endx = (deps->data->x_pert)*
	   (LATEX_PERT_BOX_WIDTH+LATEX_PERT_BOX_X_SKIP)-
	     LATEX_PERT_BOX_X_SKIP;
	 endy = (deps->data->y_pert+1)*
	   (LATEX_PERT_BOX_HEIGHT+LATEX_PERT_BOX_Y_SKIP)-
	     LATEX_PERT_BOX_Y_SKIP;

	 breaklines(startx, starty, endx, endy,
		    node->data->critical_path &&
		    deps->data->critical_path);

	 deps = deps->next;
      }
      */
      node = node->next;
   }
}

/* FUNCTION breaklines ************************************

   PURPOSE

   Break a single line across pages --- DOES NOT WORK

   AUTHOR/AUDITOR/TESTER

   Author.... Mark M. Lacey, 13 Dec 1992
   Tester.... Mark M. Lacey, 13 Dec 1992

   MODIFICATIONS (most recent to least)

*/
void breaklines(int startx, int starty, int endx, int endy, int cp)
{
   struct object_node *onode;
   int 
     dx, dy,
     newdx, newdy,
     start_viewx, start_viewy,
     end_viewx, end_viewy,
     borderx, bordery,
     page_across, page_down;
   double slope;
   char dumbbuf[100];

   slope = (double) dy/(double) dx;

   latex_line_scale(dx, dy, &newdx, &newdy);

   printf("startx=%d, starty=%d, endx=%d, endy=%d\n",
	  startx, starty, endx, endy);

   while (startx < endx) {
      page_across = startx/PAGE_WIDTH+1;
      page_down = starty/PAGE_HEIGHT+1;

      borderx = page_across*PAGE_WIDTH;
      bordery = page_down*PAGE_HEIGHT;

      start_viewx = PAGE_WIDTH-(borderx - startx);
      start_viewy = PAGE_HEIGHT-(bordery - starty);

      if (endy-starty == 0) {
	 end_viewx = PAGE_WIDTH;
      } else {
	 end_viewx = start_viewx +
	   (double)(endx - startx) *
	     (double)(bordery-starty)/(double)(endy-starty);
      }

      if (endx-startx == 0) {
	 end_viewy = PAGE_HEIGHT;
      } else {
	 end_viewy = start_viewy +
	   (double)(endy-starty) *
	     (double)(borderx-startx)/(double)(endx-startx);
      }

      startx = end_viewx + borderx - PAGE_WIDTH;
      starty = end_viewy + bordery - PAGE_HEIGHT;

      onode = 
	create_object_node(PERT_LINE,
			   start_viewx,
			   start_viewy,
			   end_viewx - start_viewx, 
			   newdx, newdy,
			   0, 0, cp, NULL, NULL);
      
      add_object_to_page(list, page_across, page_down, onode);
   }
}

void write_pert_chart()
{
   FILE *fp;
   int across, down;
   char tmpname[100];
   struct object_node *onode;

   fp = fopen(export_filename, "w");
   if (!fp) { 
      /* print out error message */ 
   } else {
      fprintf(fp, "\\documentstyle{article}\n");
      fprintf(fp, "\\begin{document}\n\n");
      for (down = 1; down <= list->pages_down; ++down) {
	 for (across = 1; across <= list->pages_across; ++across) {
	    onode = get_page_from_list(list, across, down);
	    if (onode) {
	       fprintf(fp, "\\begin{picture}(%d,%d)\n\n",
		       PAGE_WIDTH, PAGE_HEIGHT);
	       while (onode) {
		  switch (onode->object_type) {
		   case PERT_BOX:
		     strncpy(tmpname, onode->name, 25);
		     strfix(tmpname);
		     fprintf(fp, "\\put(%d,%d){\\framebox(%d,%d){%s}}\n",
			     onode->x, onode->y, 
			     LATEX_PERT_BOX_WIDTH, LATEX_PERT_BOX_HEIGHT,
			     tmpname);
		     break;
		   case PERT_LINE:
		     if (onode->critical) {
			fprintf(fp, "\\thicklines\n");
		     }
		     fprintf(fp, "\\put(%d, %d){\\line(%d,%d){%d}}\n",
			     onode->x, onode->y, 
			     onode->dx, onode->dy,
			     onode->length);
		     if (onode->critical) {
			fprintf(fp, "\\thinlines\n");
		     }
		     break;
		  }
		  onode = onode->next;
	       }
	       fprintf(fp, "\n\\end{picture}\n");
	       fprintf(fp, "\\clearpage\n\n");
	    }
	 }
      }
      fprintf(fp, "\\end{document}\n");
      fclose(fp);
   }
}

static char bigbuf[512];

/* insert escape characters for LaTeX when special characters are used */
/* in a string */
void strfix(char str[])
{
   int i, j;

   for (i = 0, j = 0; i < strlen(str); ++i) {
      switch (str[i]) {
       case '#':
       case '$':
       case '%':
       case '&':
       case '~':
       case '_':
       case '^':
       case '{':
       case '}':
	 bigbuf[j++] = '\\';
	 bigbuf[j++] = str[i];
	 break;
       case '<':
       case '>':
	 bigbuf[j++] = '$';
	 bigbuf[j++] = str[i];
	 bigbuf[j++] = '$';
	 break;
       case '\\':
	 strcpy(&bigbuf[j], "$\\backslash$");
	 j += strlen("$\\backslash$");
	 break;
       case '|':
	 strcpy(&bigbuf[j], "$\\mid$");
	 j += strlen("$\\mid$");
	 break;
       default:
	 bigbuf[j++] = str[i];
	 break;
      }
   }
   bigbuf[j]='\0';
   strcpy(str, bigbuf);
}

/* scale a difference in x and a difference in y so that it is between */
/* -6 and 6 for LaTeX's \line function */
void latex_line_scale(int x, int y, int *newxptr, int *newyptr)
{
   int d;
   int newx, newy;

   newx = x;
   newy = y;
   while ((newx < -6) || (newx > 6) || (newy < -6) || (newy > 6)) {
      if (newx > -6 && newx < 6) {
	 newy = newy/newx;
	 newx = 1;
      } else {
	 newx /= 6;
	 newy /= 6;
      }
   }

   d = gcd(newx, newy); /* LaTeX makes _weird_ lines if you don't give */
			/* it the gcd of the two numbers */
   *newxptr = newx/d;
   *newyptr = newy/d;
}

/* from pg. 12 of the introduction to "Algorithms in C" by Robert */
/* Sedgewick --- fixed so that if either u or v is 0 it returns 1 */
int gcd(int u, int v) 
{
   int t;
   
   if ((u == 0) || (v == 0)) return 1;
   if (u < 0) u = -u;
   if (v < 0) v = -v;
   do {
      if (u < v) { t = u; u = v; v = t; }
      u = u - v;
   } while (u != 0);

   return v;
}

