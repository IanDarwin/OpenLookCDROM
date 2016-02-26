/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */

#define HASHSIZE 1013  /* this constant should be tweeked to maximize performance */

struct pm_list
  {
    struct pm_list *next_in_hash;       /* next address in linked-list     */
    struct pm_list *prev_in_hash;	/* last address in linked-list     */
    char *label;			/* entry label                     */
    struct pm_list *next_elem;          /* next element in data structure  */
    struct pm_list *prev_elem;          /* prev element in data structure  */
    int  category;			/* category (PM_OBJECT or PM_ELEMENT)    */
    int  type;                          /* element type                    */
    int  list_size;                     /* if a list, max length           */
    int  string_max_len;                /* if a string, max number of char */
    int	 savable;			/* SAVE_NONE, SAVE_CONFIG, SAVE_SETTINGS */
    union
       {
	void      *addr_data;
        int       int_data;
	int       *int_list_data;
	double    double_data;
	double    *double_list_data;
	char      *string_data;
	char      **string_list_data;
	int       (*fnct_ptr)();
       }data;
  };

extern struct pm_list *hashtab[];

