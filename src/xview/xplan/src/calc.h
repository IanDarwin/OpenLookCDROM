#ifndef _calc_h_
#define _calc_h_

/*----------------------------------------------------------------- 
  Set to 0 : use the old PERT algorithm which aligned the new subpath
             under different y-coordinate.
         1 : use the new PERT algorithm which incorporated breadth
	     first seach algorithm to calculate y-coordinate.
--------------------------------------------------------------------*/
#define OLD_PERT 0    

extern enum boolean loop_found;        /* set to true if a loop is found */
extern struct task_node *loop_pointer; /* points to where the loop is found */

#endif
