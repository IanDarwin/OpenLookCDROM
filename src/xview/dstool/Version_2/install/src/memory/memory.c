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
/* 
 * paw  11/9/90
 * modified: 12/14/92 paw
 *
 * dstool
 *
 * flow storage routines
 *
 *
 */

#include <stdio.h>
#include <malloc.h>

#include <constants.h>
#include <memory.h>
#include <manifold.h>
#include "mem_defs.h"


/******************************  PUBLIC ROUTINES  *********************/
 
memory 
memory_create(mtype, size, def_traj_len, vdim, pdim, cdim)
int mtype, size, def_traj_len, vdim, pdim, cdim;
{
  Memory m, create_memory_obj();

  if (!(m=create_memory_obj())) return(NULL);
  m->mem_type = mtype;
  m->chunk_size = size;
  m->default_traj_length = def_traj_len;
  m->too_small_mem = TOO_SMALL_MEM;
  m->varb_dim = vdim;
  m->param_dim = pdim;
  m->color_dim = cdim;
  
  increase_allocated_memory(m,size);
  return((memory ) m);
}


int
  memory_destroy(m)
memory m;
{
  if (m) dismantle_memory_obj((Memory) m);
  return(0);
}


int
  memory_start_new_flow(m,n_trajs,n_extra_dvars,n_extra_ivars,n_points,
		      n_extra_dparams,n_extra_iparams)
memory m;
int n_trajs,n_extra_dvars,n_extra_ivars,n_points,n_extra_dparams,n_extra_iparams;
{
  FLOW_ITEM 	*obj, *build_flow_item();
  int dp,ip,d,i;
  
  if (!m) return(0);
  memory_end_current_flow(m);
  mem_get_sizes((Memory) m,&dp,&ip,&d,&i);
  obj = build_flow_item((Memory) m,n_trajs,d+n_extra_dvars,i+n_extra_ivars,n_points,
			dp+n_extra_dparams,ip+n_extra_iparams);
  add_flow_item((Memory) m,obj);
  set_current_flow((Memory) m,obj);
  
  return(0);
}


int
  memory_vanilla_start_new_flow(m,n_trajs,n_dvars,n_ivars,n_points,
		      n_dparams,n_iparams)
memory m;
int n_trajs,n_dvars,n_ivars,n_points,n_dparams,n_iparams;
{
  FLOW_ITEM 	*obj, *build_flow_item();
  
  if (!m) return(0);
  memory_end_current_flow(m);
  obj = build_flow_item((Memory) m,n_trajs,n_dvars,n_ivars,n_points,n_dparams,n_iparams);
  add_flow_item((Memory) m,obj);
  set_current_flow((Memory) m,obj);
  
  return(0);
}


int
  memory_end_current_flow(m)
memory m;
{
  int	i;
  TRAJ_ITEM	*last_traj_seg();
  
  if (!m) return(0);
  if (!((Memory) m)->cur_flow) return(0);
  for (i=0; i<((Memory) m)->cur_flow->num_trajs; i++) 
    free_extra_traj_mem((Memory) m,last_traj_seg(((Memory) m)->cur_flow->trajs[i]));
  set_current_flow((Memory) m,NULL);
  return(0);
}


int
  memory_exists_current_flow(m)
memory m;
{
  if (!m) return(0);
  if (((Memory) m)->cur_flow) return(1);
  return(0);
}


int
  memory_clear_flows(m)
memory m;
{
  int status=0;
  
  if (!m) return(status);
  
  set_current_flow((Memory) m,NULL);
  dismantle_mem_list((Memory) m,((Memory) m)->free_mem_list);	/* this is necessary so that we don't keep track of free'd mem */
  ((Memory) m)->free_mem_list = NULL;
  dismantle_flow_list((Memory) m,((Memory) m)->flow_list);
  ((Memory) m)->flow_list = NULL;
  if (make_free_mem_total_mem((Memory) m)) status = -1;
  return(status);
}


void
  memory_clear_selected_flows(m,flow_test)
memory m;
int (*flow_test)();
{
  FLOW_ITEM **pflow, *temp;
  
  if (!m || !flow_test) return;
  
  memory_end_current_flow(m);
  pflow = &(((Memory) m)->flow_list);
  while (*pflow)
    {
      if (flow_test((*pflow)->iparams+extra_iparams_index((Memory) m),
		    (*pflow)->num_iparams-extra_iparams_index((Memory) m)))
	{
	  /* remove this flow item */
	  temp = (*pflow)->next_flow;
	  (*pflow)->next_flow = NULL;
	  dismantle_flow_list((Memory) m,*pflow);
	  *pflow = temp;
	}
      else
	{
	  /* keep this flow item */
	  pflow = &((*pflow)->next_flow);
	}
    }
  return;
}


int
  memory_dump(fp,m,dl)
FILE *fp;
memory m;
int dl;
{
  if (!m) return(0);
  dump_memory_obj(fp,(Memory) m,dl);
}



int
  memory_add_point(m, p_varb, p_param, p_color, p_extra_doubles, p_extra_ints, 
		 p_extra_dparams, p_extra_iparams)
memory m;
double *p_varb, *p_param, *p_extra_doubles, *p_extra_dparams;
int *p_color, *p_extra_ints, *p_extra_iparams;
{
  if (!m) return(0);
  if (!((Memory) m)->cur_flow) return(1);
  return(add_point_to_traj((Memory) m,((Memory) m)->cur_flow,
			   ((Memory) m)->cur_traj,
			   p_varb, p_param, p_color, p_extra_doubles, p_extra_ints,
			   p_extra_dparams, p_extra_iparams));
}



int
  memory_vanilla_add_point(m, p_doubles, p_ints, p_dparams, p_iparams)
memory m;
double *p_doubles, *p_dparams;
int *p_ints, *p_iparams;
{
  if (!m) return(0);
  if (!((Memory) m)->cur_flow) return(1);
  return(vanilla_add_point_to_traj((Memory) m,((Memory) m)->cur_flow,
			   ((Memory) m)->cur_traj,
			   p_doubles, p_ints, p_dparams, p_iparams));
}


int
  memory_add_points(m, n, varb_seg, param_seg, color_seg, extra_doubles_seg, extra_ints_seg)
memory m;
int n, **color_seg, **extra_ints_seg;
double **varb_seg, **param_seg, **extra_doubles_seg;
{
  int i, status=0, *color=NULL, *extra_ints=NULL;
  double *varb=NULL, *param=NULL, *extra_doubles=NULL;
  
  if (!m) return(0);
  if (!((Memory) m)->cur_flow) return(1);
  for (i=0; i<n && status==0; i++)
    {
      if (varb_seg) varb = *(varb_seg++);
      if (param_seg) param = *(param_seg++);
      if (color_seg) color = *(color_seg++);
      if (extra_doubles_seg) extra_doubles = *(extra_doubles_seg++);
      if (extra_ints_seg) extra_ints = *(extra_ints_seg++);
      status = add_point_to_traj((Memory) m, ((Memory) m)->cur_flow,
				 ((Memory) m)->cur_traj,
				 varb, param, color, 
				 extra_doubles, extra_ints, NULL, NULL);
    }
  return(status);
}


int
  memory_get_last_point(m,pp_varb, pp_param, pp_color, pp_extra_doubles, pp_extra_ints,
		      pp_extra_dparams, pp_extra_iparams)
memory m;
double **pp_varb, **pp_param, **pp_extra_doubles, **pp_extra_dparams;
int **pp_color, **pp_extra_ints, **pp_extra_iparams;
{
  if (!m || !((Memory) m)->cur_flow) return(1);
  return(get_last_point((Memory) m, ((Memory) m)->cur_flow->trajs[((Memory) m)->cur_traj_num],
			pp_varb, pp_param, pp_color, pp_extra_doubles, pp_extra_ints,
			pp_extra_dparams, pp_extra_iparams));
}


int
  memory_get_first_point(m,pp_varb, pp_param, pp_color, pp_extra_doubles, pp_extra_ints,
		      pp_extra_dparams, pp_extra_iparams)
memory m;
double **pp_varb, **pp_param, **pp_extra_doubles, **pp_extra_dparams;
int **pp_color, **pp_extra_ints, **pp_extra_iparams;
{
  if (!m || !((Memory) m)->cur_flow) return(1);
  return(get_first_point((Memory) m, ((Memory) m)->cur_flow->trajs[((Memory) m)->cur_traj_num],
			 pp_varb, pp_param, pp_color, pp_extra_doubles, pp_extra_ints,
			 pp_extra_dparams, pp_extra_iparams));
}


int
  memory_next_traj(m)
memory m;
{
  TRAJ_ITEM	*last_traj_seg();

  if (!m) return(0);
  if (!((Memory) m)->cur_flow) return(1);
  if (++(((Memory) m)->cur_traj_num) >= ((Memory) m)->cur_flow->num_trajs)
    ((Memory) m)->cur_traj_num = 0;
  ((Memory) m)->cur_traj = last_traj_seg(((Memory) m)->cur_flow->trajs[((Memory) m)->cur_traj_num]);
  return(0);
}


int
  memory_set_cur_traj(m,i)
memory m;
int i;
{
  TRAJ_ITEM	*last_traj_seg();
  
  if (!m) return(0);
  if (!((Memory) m)->cur_flow) return(1);
  if (i<1  ||  i > ((Memory) m)->cur_flow->num_trajs) return(1);
  ((Memory) m)->cur_traj_num = --i;  /* C array numbers from 0 */
  ((Memory) m)->cur_traj = last_traj_seg(((Memory) m)->cur_flow->trajs[i]);
  return(0);
}


int 
memory_reset_read(m)
memory m;
{
  if (!m || !((Memory) m)->flow_list) return(1); /* no flows */
  ((Memory) m)->read_flow = NULL;
  ((Memory) m)->read_traj_num = 0;
  ((Memory) m)->read_traj_seg = NULL;
  ((Memory) m)->read_point = 0;
  return(0);
}


int 
memory_set_read(m,nflow,ntraj,npoint,p_extra_dparams, pn_extra_dparams, 
		p_extra_iparams, pn_extra_iparams, pn_extra_doubles, pn_extra_ints)
memory m;
int nflow,ntraj,npoint,*pn_extra_dparams,**p_extra_iparams,*pn_extra_iparams,
    *pn_extra_doubles,*pn_extra_ints;
double **p_extra_dparams;
{
  FLOW_ITEM *flow;
  TRAJ_ITEM *traj;

  if (!m || !(flow = ((Memory) m)->flow_list)) return(1); /* no flows */
  while  (flow && (--nflow > 0)) 
    flow=flow->next_flow;
  if (!flow) return(1); /* less than nflow flows */
  if (ntraj>flow->num_trajs || ntraj<1) return(1); /* bad trajectory num */
  traj = flow->trajs[ntraj-1];
  if (npoint<1) return(1); /* invalid point number */
  while (traj && (npoint > traj->cur_point))
    {
      npoint -= traj->cur_point;
      traj = traj->next_seg;
    }
  if (!traj) return(1); /* not enough points in traj */

  ((Memory) m)->read_flow = flow;
  ((Memory) m)->read_traj_num = ntraj;
  ((Memory) m)->read_traj_seg = traj;
  ((Memory) m)->read_point = npoint-1;
  if (p_extra_dparams) 
    {
      *pn_extra_dparams = ((Memory) m)->read_flow->num_dparams -
	extra_dparams_index((Memory) m);
      if (*pn_extra_dparams > 0)
	*p_extra_dparams = ((Memory) m)->read_flow->dparams + 
	  extra_dparams_index((Memory) m);
      else *p_extra_dparams = NULL;
    }
  if (p_extra_iparams)
    {
      *pn_extra_iparams = ((Memory) m)->read_flow->num_iparams -
	extra_iparams_index((Memory) m);
      if (*pn_extra_iparams > 0)
	*p_extra_iparams = ((Memory) m)->read_flow->iparams +
	  extra_iparams_index((Memory) m);
      else *p_extra_iparams = NULL;
    }
  if (pn_extra_doubles) 
    if (((Memory) m)->read_traj_seg)
      *pn_extra_doubles = ((Memory) m)->read_traj_seg->ddim - 
	extra_doubles_index((Memory) m);
    else *pn_extra_doubles = 0;
  if (pn_extra_ints)
    if (((Memory) m)->read_traj_seg)
      *pn_extra_ints = ((Memory) m)->read_traj_seg->idim - 
	extra_ints_index((Memory) m);
    else *pn_extra_ints = 0;
  return(0);
}


int
memory_read_next_flow(m, pn_trajs, p_extra_dparams, pn_extra_dparams, 
		      p_extra_iparams, pn_extra_iparams)
memory m;
double **p_extra_dparams;
int *pn_extra_dparams,**p_extra_iparams, *pn_extra_iparams, *pn_trajs;
{
  FLOW_ITEM *flow;
  
  if (!m) return(1);
  if (((Memory) m)->read_flow == NULL)
    ((Memory) m)->read_flow = ((Memory) m)->flow_list;
  else ((Memory) m)->read_flow = ((Memory) m)->read_flow->next_flow;
  ((Memory) m)->read_traj_num = 0;
  ((Memory) m)->read_traj_seg = NULL;
  if (flow = ((Memory) m)->read_flow)
    {
      if (pn_trajs) *pn_trajs = flow->num_trajs;
      if (p_extra_dparams) 
	{
	  *pn_extra_dparams = ((Memory) m)->read_flow->num_dparams -
	    extra_dparams_index((Memory) m);
	  if (*pn_extra_dparams > 0)
	    *p_extra_dparams = ((Memory) m)->read_flow->dparams + 
	      extra_dparams_index((Memory) m);
	  else *p_extra_dparams = NULL;
	}
      if (p_extra_iparams)
	{
	  *pn_extra_iparams = ((Memory) m)->read_flow->num_iparams -
	                      extra_iparams_index((Memory) m);
	  if (*pn_extra_iparams > 0)
	    *p_extra_iparams = ((Memory) m)->read_flow->iparams +
		               extra_iparams_index((Memory) m);
	  else *p_extra_iparams = NULL;
	}
      return(0);
    }
  else
    {
      if (pn_trajs) *pn_trajs = 0;
      return(1);
    }
}

int
memory_vanilla_read_next_flow(m, pn_trajs, p_dparams, pn_dparams, 
		      p_iparams, pn_iparams)
memory m;
double **p_dparams;
int *pn_dparams,**p_iparams, *pn_iparams, *pn_trajs;
{
  FLOW_ITEM *flow;
  
  if (!m) return(1);
  if (((Memory) m)->read_flow == NULL)
    ((Memory) m)->read_flow = ((Memory) m)->flow_list;
  else ((Memory) m)->read_flow = ((Memory) m)->read_flow->next_flow;
  ((Memory) m)->read_traj_num = 0;
  ((Memory) m)->read_traj_seg = NULL;
  if (flow = ((Memory) m)->read_flow)
    {
      if (pn_trajs) *pn_trajs = flow->num_trajs;
      if (p_dparams) 
	{
	  *pn_dparams = ((Memory) m)->read_flow->num_dparams;
	  if (*pn_dparams > 0)
	    *p_dparams = ((Memory) m)->read_flow->dparams;
	  else *p_dparams = NULL;
	}
      if (p_iparams)
	{
	  *pn_iparams = ((Memory) m)->read_flow->num_iparams;
	  if (*pn_iparams > 0)
	    *p_iparams = ((Memory) m)->read_flow->iparams;
	  else *p_iparams = NULL;
	}
      return(0);
    }
  else
    {
      if (pn_trajs) *pn_trajs = 0;
      return(1);
    }
}


int
  memory_read_next_traj(m, pn_points, pn_extra_doubles, pn_extra_ints)
memory m;  
int *pn_points, *pn_extra_doubles, *pn_extra_ints;
{
  if (!m) return(1);

  ((Memory) m)->read_point = 0;
  if (((Memory) m)->read_traj_num < ((Memory) m)->read_flow->num_trajs)
    {
      ((Memory) m)->read_traj_seg = ((Memory) m)->read_flow->trajs[((Memory) m)->read_traj_num];
      (((Memory) m)->read_traj_num)++;
      if (pn_points) *pn_points = mem_count_points_in_traj(((Memory) m)->read_traj_seg);
      if (pn_extra_doubles) 
	if (((Memory) m)->read_traj_seg)
	  *pn_extra_doubles = ((Memory) m)->read_traj_seg->ddim - 
	    extra_doubles_index((Memory) m);
	else *pn_extra_doubles = 0;
      if (pn_extra_ints)
	if (((Memory) m)->read_traj_seg)
	  *pn_extra_ints = ((Memory) m)->read_traj_seg->idim - 
	    extra_ints_index((Memory) m);
	else *pn_extra_ints = 0;
      return(0);
    }
  else 
    {
      if (pn_points) *pn_points = 0;
      if (pn_extra_doubles) *pn_extra_doubles = 0;
      if (pn_extra_ints) *pn_extra_ints = 0;
      return(1);
    }
}

int
  memory_vanilla_read_next_traj(m, pn_points, pn_doubles, pn_ints)
memory m;  
int *pn_points, *pn_doubles, *pn_ints;
{
  if (!m) return(1);
  ((Memory) m)->read_point = 0;
  if (((Memory) m)->read_traj_num < ((Memory) m)->read_flow->num_trajs)
    {
      ((Memory) m)->read_traj_seg = ((Memory) m)->read_flow->trajs[((Memory) m)->read_traj_num];
      (((Memory) m)->read_traj_num)++;
      if (pn_points) *pn_points = mem_count_points_in_traj(((Memory) m)->read_traj_seg);
      if (pn_doubles) 
	if (((Memory) m)->read_traj_seg)
	  *pn_doubles = ((Memory) m)->read_traj_seg->ddim;
	else *pn_doubles = 0;
      if (pn_ints)
	if (((Memory) m)->read_traj_seg)
	  *pn_ints = ((Memory) m)->read_traj_seg->idim;
	else *pn_ints = 0;
      return(0);
    }
  else 
    {
      if (pn_points) *pn_points = 0;
      if (pn_doubles) *pn_doubles = 0;
      if (pn_ints) *pn_ints = 0;
      return(1);
    }
}


int
  memory_read_next_point(m, pp_varb, pp_param, pp_color, 
		       pp_extra_doubles, pp_extra_ints)
memory m;
double **pp_varb, **pp_param, **pp_extra_doubles;
int **pp_color, **pp_extra_ints;
{
  TRAJ_ITEM *traj;
  int n;
  
  if (!m) return(1);
  if (((Memory) m)->read_traj_seg)
    {
      while ( ((Memory) m)->read_traj_seg->cur_point <= ((Memory) m)->read_point
	     && ((Memory) m)->read_traj_seg->next_seg )
	{
	  ((Memory) m)->read_traj_seg = ((Memory) m)->read_traj_seg->next_seg;
	  ((Memory) m)->read_point = 0;
	}
      traj = ((Memory) m)->read_traj_seg;
      n = ((Memory) m)->read_point;
      if (traj->cur_point > n)
	{
	  if (pp_varb) 
	    {
	      switch(where_are_varbs(((Memory) m)->mem_type))
		{
		case BODY:
		  *pp_varb = traj->dpoints + n*traj->ddim;
		  break;
		case HEADER:
		  *pp_varb = ((Memory) m)->read_flow->dparams;
		  break;
		case NOWHERE:
		  *pp_varb = NULL;
		  break;
		}		       
	    }
	  if (pp_param)
	    {
	      switch(where_are_params(((Memory) m)->mem_type))
		{
		case BODY:
		  *pp_param = traj->dpoints + n*traj->ddim + param_index((Memory) m);
		  break;
		case HEADER:
		  *pp_param = ((Memory) m)->read_flow->dparams + param_index((Memory) m);
		  break;
		case NOWHERE:
		  *pp_param = NULL;
		  break;
		}		       
	    }
	  if (pp_color)
	    {
	      switch(where_are_colors(((Memory) m)->mem_type))
		{
		case BODY:
		  *pp_color = traj->ipoints + n*traj->idim;
		  break;
		case HEADER:
		  *pp_color = ((Memory) m)->read_flow->iparams;
		  break;
		case NOWHERE:
		  *pp_color = NULL;
		  break;
		}		       
	    }
	  if (pp_extra_doubles)
	    {
	      if ( traj->ddim > extra_doubles_index((Memory) m))
		*pp_extra_doubles = traj->dpoints + n*traj->ddim + 
		                    extra_doubles_index((Memory) m);
	      else
		*pp_extra_doubles = NULL;
	    }
	  if (pp_extra_ints)
	    {
	      if ( traj->idim > extra_ints_index((Memory) m))
		*pp_extra_ints = traj->ipoints + n*traj->idim + 
		                    extra_ints_index((Memory) m);
	      else
		*pp_extra_ints = NULL;
	    }
	  (((Memory) m)->read_point)++;
	  return(0);
	}
    }
  /* no point ! */
  return(1);
}

int
  memory_vanilla_read_next_point(m, pp_doubles, pp_ints)
memory m;
double **pp_doubles;
int **pp_ints;
{
  TRAJ_ITEM *traj;
  int n;
  
  if (!m) return(1);
  if (((Memory) m)->read_traj_seg)
    {
      while ( ((Memory) m)->read_traj_seg->cur_point <= ((Memory) m)->read_point
	     && ((Memory) m)->read_traj_seg->next_seg )
	{
	  ((Memory) m)->read_traj_seg = ((Memory) m)->read_traj_seg->next_seg;
	  ((Memory) m)->read_point = 0;
	}
      traj = ((Memory) m)->read_traj_seg;
      n = ((Memory) m)->read_point;
      if (traj->cur_point > n)
	{
	  if (pp_doubles)
	    {
	      if ( traj->ddim > 0)
		*pp_doubles = traj->dpoints + n*traj->ddim;
	      else
		*pp_doubles = NULL;
	    }
	  if (pp_ints)
	    {
	      if ( traj->idim > 0)
		*pp_ints = traj->ipoints + n*traj->idim;
	      else
		*pp_ints = NULL;
	    }
	  (((Memory) m)->read_point)++;
	  return(0);
	}
    }
  /* no point ! */
  return(1);
}


int
memory_get_type(m)
memory m;
{
	if (!m) return(0);
	return (((Memory) m)->mem_type);
}


double
memory_closest_distance(m,p,distance_f,work,manifold,flow_test)
memory m;
double *p, (*distance_f)(), *work;
Manifold *manifold;
int (*flow_test)();
{
  double	*dpts,dist,min_distance = -1.0;
  FLOW_ITEM *flow;
  TRAJ_ITEM *traj;
  int *ipts,i,j;

  if (!m || !((Memory) m)->flow_list) return(min_distance);
  flow = ((Memory) m)->flow_list;
  while (flow) 
    {
      if (flow_test(flow->iparams+extra_iparams_index((Memory) m),
		    flow->num_iparams-extra_iparams_index((Memory) m))) 
	{
	  for (i=0; i<flow->num_trajs; i++) 
	    {
	      traj=flow->trajs[i];
	      while (traj) 
		{
		  dpts = traj->dpoints;
		  ipts = traj->ipoints;
		  for (j=0; j<traj->cur_point; j++) 
		    {
		      /* extend this to pass down more info ! */
		      dist = distance_f(traj->ddim,p,dpts,work,manifold);
		      if (min_distance < 0.0 || dist < min_distance)
			min_distance = dist;
		      if (dpts) dpts += traj->ddim;	
		      if (ipts) ipts += traj->idim;
		    }
		  traj=traj->next_seg;
		}
	    }
	}
      flow = flow->next_flow;
    }
  return(min_distance);
}


int
memory_stored_points(m)
memory m;
{
  if (!m) return(0);
  return(((Memory) m)->stored_points);
}
  
  
int
memory_delete_flow(m,flow_num)
memory m;
int flow_num;
{ 
  int i;
  FLOW_ITEM *flow, **p_flow;

  if (!m || flow_num<1) return(0);
  
  p_flow = &(((Memory) m)->flow_list);
  flow = *p_flow;
  for (i=1; i<flow_num && flow; i++) 
    {
      p_flow = &(flow->next_flow);
      flow = *p_flow;
    }
  if (flow)
    {
      /* check to see if this is the current flow and if so reset */
      if (flow == ((Memory) m)->cur_flow) memory_end_current_flow(m);
      /* check to see if this is the flow currently being read and if so reset */
      if (flow == ((Memory) m)->read_flow) memory_reset_read(m);

      /* relink flow list and destroy flow */
      *p_flow = flow->next_flow;
      flow->next_flow = NULL;
      dismantle_flow_list((Memory) m, flow);
    }
  return 0;
}

int
memory_delete_last_flow(m)
memory m;
{
  memory_delete_flow(m,memory_nflows(m));
  return;
}



int
memory_nflows(m)
memory m;
{
  int n=0;
  FLOW_ITEM *flow;

  if (!m || !(flow = ((Memory) m)->flow_list)) return(0); /* no flows */
  while (flow)
    {
      flow = flow->next_flow;
      n++;
    }
  return(n);
}

int
memory_ntrajs(m,nflow)
memory m;
int nflow;
{
  FLOW_ITEM *flow;

  if (!m || !(flow = ((Memory) m)->flow_list)) return(0); /* no flows */
  for ( ; flow && (--nflow > 0) ; flow=flow->next_flow) ;
  if (!flow) return(0); /* less than nflow flows */
  return(flow->num_trajs);
}

int
memory_npoints(m,nflow,ntraj)
memory m;
int nflow,ntraj;
{
  int n=0;
  FLOW_ITEM *flow;
  TRAJ_ITEM *traj;

  if (!m || !(flow = ((Memory) m)->flow_list)) return(0); /* no flows */
  for ( ; flow && (--nflow > 0) ; flow=flow->next_flow) ;
  if (!flow) return(0); /* less than nflow flows */
  if (ntraj>flow->num_trajs || ntraj<1) return(0); /* bad trajectory num */
  traj = flow->trajs[ntraj-1];
  while (traj)
    {
      n += traj->cur_point;
      traj = traj->next_seg;
    }
  return(n);
}



/********************************************************/
/***********  INTERNAL PROCEDURES  **********************/
/********************************************************/
/* 	PRIVATE primitives to memory object		*/
/********************************************************/


/*
 * add flow item to memory flow list
 */
int
  add_flow_item(m,flow)
Memory m;
FLOW_ITEM	*flow;
{
  FLOW_ITEM *attach_to_flow_list();
  
  m->flow_list = attach_to_flow_list(m->flow_list, flow);
  return(0);
}


/*
 * makes flow the current flow
 */
int
  set_current_flow(m,flow)
Memory m;
FLOW_ITEM *flow;
{
  TRAJ_ITEM	*last_traj_seg();

  m->cur_flow = flow;
  m->cur_traj_num = 0;
  if (flow) m->cur_traj = last_traj_seg(m->cur_flow->trajs[0]);
  else m->cur_traj=NULL;
  return(0);
}


/*
 * assigns memory to and sets up flow items
 */
FLOW_ITEM 
  *build_flow_item(m,n_trajs,n_dvars,n_ivars,n_points,n_dparams,n_iparams)
Memory m;
int n_trajs,n_dvars,n_ivars,n_points,n_dparams,n_iparams;
{
  int i,length;
  FLOW_ITEM *obj,*create_flow_item();
  TRAJ_ITEM *build_traj_item();
  
  if (!(obj = create_flow_item(n_dparams,n_iparams,n_trajs))) return(NULL); 
  obj->requested_length = n_points;
  obj->next_flow = NULL;
  length = memory_alloc_suggestion(m,n_trajs,n_dvars,n_ivars,n_points);
  for (i=0; i<n_trajs; i++) 
    obj->trajs[i] = build_traj_item(m,n_dvars,n_ivars,length);
  return(obj);
}


TRAJ_ITEM *
  build_traj_item(m,ddim,idim,len)
Memory m;
int len;
{
  TRAJ_ITEM *obj, *create_traj_item();

  if (!(obj = create_traj_item())) return(NULL);
  obj->ddim = ddim;
  obj->idim = idim;
  if (ddim+idim > 0) 
    obj->num_points = memory_alloc(m,&obj->dpoints,ddim,&obj->ipoints,idim,len);
  obj->cur_point = 0;
  return(obj);
}


MEM_ITEM *
  build_mem_item(m,ptr,n)
Memory m;
void *ptr;
int n;
{
  MEM_ITEM *obj, *create_mem_item();

  if (!(obj = create_mem_item())) return(NULL);
  obj->ptr = ptr;
  obj->num_bytes = n;
  return(obj);
}


int
  add_point_to_traj(m, flow, traj, p_varb, p_param, p_color, 
		    p_extra_doubles, p_extra_ints, p_extra_dparams, p_extra_iparams)
Memory m;
FLOW_ITEM *flow;
TRAJ_ITEM *traj;
int *p_color, *p_extra_ints, *p_extra_iparams;
double *p_varb, *p_param, *p_extra_doubles, *p_extra_dparams;
{
  double *dptr;
  TRAJ_ITEM *new_traj;
  int i,*iptr, need_space=0;
  
  if (!m || !flow || !traj ) return(1);
  /* check to see if anything goes in the body */
  if ( (p_varb && where_are_varbs(m->mem_type)==BODY) ||
       (p_param && where_are_params(m->mem_type)==BODY) ||
       (p_color && where_are_colors(m->mem_type)==BODY) ||
      p_extra_doubles || p_extra_ints ) need_space=1;
  if (need_space)
    {
      if (traj->cur_point >= traj->num_points) 
	{ 
	  /* no space ! */
	  if (traj->num_points) 
	    { 
	      /* OK this one's full, get another traj_seg */
	      if (!(new_traj = build_traj_item(m,traj->ddim,traj->idim,flow->requested_length))) 
		return(1);
	      if (new_traj->num_points == 0)
		{
		  dismantle_traj_list(m,new_traj);
		  return(1);
		}
	      m->cur_traj = traj->next_seg = new_traj;
	      traj = new_traj;
	    }
	  else 
	    { 
	      /* no mem was allocated - not a good sign */
	      /* TRY TO ASSIGN MEMORY, NEEDS WORK */	
	      return(1);
	    }
	}
    }
  
  /* there is space (or none is needed!) so copy it in ! */
  dptr = traj->dpoints + traj->ddim * (traj->cur_point);
  iptr = traj->ipoints + traj->idim * (traj->cur_point);
  if (p_varb)
    {
      switch(where_are_varbs(m->mem_type))
	{
	case HEADER:
	  for (i=0; i<m->varb_dim; i++) m->cur_flow->dparams[i] = p_varb[i];
	  break;
	case BODY:
	  for (i=0; i<m->varb_dim; i++) dptr[i] = p_varb[i];
	  break;
	}
    }
  if (p_param)
    {
      switch(where_are_params(m->mem_type))
	{
	case HEADER:
	  for (i=0; i<m->param_dim; i++) m->cur_flow->dparams[i+param_index(m)] = p_param[i];
	  break;
	case BODY:
	  for (i=0; i<m->param_dim; i++) dptr[i+param_index(m)] = p_param[i];
	  break;
	}
    }
  if (p_color)
    {
      switch(where_are_colors(m->mem_type))
	{
	case HEADER:
	  for (i=0; i<m->color_dim; i++) m->cur_flow->iparams[i] = p_color[i];
	  break;
	case BODY:
	  for (i=0; i<m->color_dim; i++) iptr[i] = p_color[i];
	  break;
	}
    }
  if (p_extra_doubles)
    {
      for (i=extra_doubles_index(m); i<traj->ddim; i++)
	dptr[i] = p_extra_doubles[i-extra_doubles_index(m)];
    }
  if (p_extra_ints)
    {
      for (i=extra_ints_index(m); i<traj->idim; i++)
	iptr[i] = p_extra_ints[i-extra_ints_index(m)];
    }
  if (p_extra_dparams)
    {
      for (i=extra_dparams_index(m); i<m->cur_flow->num_dparams; i++)
	m->cur_flow->dparams[i] = p_extra_dparams[i-extra_dparams_index(m)];
    }
  if (p_extra_iparams)
    {
      for (i=extra_iparams_index(m); i<m->cur_flow->num_iparams; i++)
	m->cur_flow->iparams[i] = p_extra_iparams[i-extra_iparams_index(m)];
    }
  
  if (need_space)
    {
      /* increment # of stored points */
      m->stored_points++;
      (traj->cur_point)++;
    }
  return(0);
}


int
  vanilla_add_point_to_traj(m, flow, traj, p_doubles, p_ints, p_dparams, p_iparams)
Memory m;
FLOW_ITEM *flow;
TRAJ_ITEM *traj;
int *p_ints, *p_iparams;
double *p_doubles, *p_dparams;
{
  double *dptr;
  TRAJ_ITEM *new_traj;
  int i,*iptr, need_space=0;
  
  if (!m || !flow || !traj ) return(1);
  /* check to see if anything goes in the body */
  if ( p_doubles || p_ints ) need_space=1;
  if (need_space)
    {
      if (traj->cur_point >= traj->num_points) 
	{ 
	  /* no space ! */
	  if (traj->num_points) 
	    { 
	      /* OK this one's full, get another traj_seg */
	      if (!(new_traj = build_traj_item(m,traj->ddim,traj->idim,flow->requested_length))) 
		return(1);
	      if (new_traj->num_points == 0)
		{
		  dismantle_traj_list(m,new_traj);
		  return(1);
		}
	      m->cur_traj = traj->next_seg = new_traj;
	      traj = new_traj;
	    }
	  else 
	    { 
	      /* no mem was allocated - not a good sign */
	      /* TRY TO ASSIGN MEMORY, NEEDS WORK */	
	      return(1);
	    }
	}
    }
  
  /* there is space (or none is needed!) so copy it in ! */
  dptr = traj->dpoints + traj->ddim * (traj->cur_point);
  iptr = traj->ipoints + traj->idim * (traj->cur_point);
  if (p_doubles)
    {
      for (i=0; i<traj->ddim; i++)
	dptr[i] = p_doubles[i];
    }
  if (p_ints)
    {
      for (i=0; i<traj->idim; i++)
	iptr[i] = p_ints[i];
    }
  if (p_dparams)
    {
      for (i=0; i<m->cur_flow->num_dparams; i++)
	m->cur_flow->dparams[i] = p_dparams[i];
    }
  if (p_iparams)
    {
      for (i=0; i<m->cur_flow->num_iparams; i++)
	m->cur_flow->iparams[i] = p_iparams[i];
    }
  
  if (need_space)
    {
      /* increment # of stored points */
      m->stored_points++;
      (traj->cur_point)++;
    }
  return(0);
}



/* procedure to count the number of points in a trajectory list */
int
mem_count_points_in_traj(traj)
TRAJ_ITEM *traj;
{
  int n=0;
  
  while (traj)
    {
      n += traj->cur_point;
      traj = traj->next_seg;
    }
  return(n);
}


int
  get_last_point(m,traj, pp_varb, pp_param, pp_color,
		 pp_extra_doubles, pp_extra_ints,
		 pp_extra_dparams, pp_extra_iparams)
Memory m;
TRAJ_ITEM *traj;
double **pp_varb, **pp_param, **pp_extra_doubles, **pp_extra_dparams;
int **pp_color, **pp_extra_ints, **pp_extra_iparams;
{
  int n,status = 1;
  
  while (traj) 
    {
      status = 0;
      if ((n = traj->cur_point) > 0) 
	{
	  /* here is a potential last point, fill in info! */
	  if (pp_varb)
	    {
	      switch (where_are_varbs(m->mem_type))
		{
		case BODY:
		  *pp_varb = traj->dpoints + (n-1)*traj->ddim;
		  break;
		case HEADER:
		  *pp_varb = m->cur_flow->dparams;
		  break;
		case NOWHERE:
		  *pp_varb = NULL;
		  break;
		}
	    }
	  if (pp_param)
	    {
	      switch (where_are_params(m->mem_type))
		{
		case BODY:
		  *pp_param = traj->dpoints + (n-1)*traj->ddim + param_index(m);
		  break;
		case HEADER:
		  *pp_param = m->cur_flow->dparams + param_index(m);
		  break;
		case NOWHERE:
		  *pp_param = NULL;
		  break;
		}
	    }
	  if (pp_color)
	    {
	      switch (where_are_colors(m->mem_type))
		{
		case BODY:
		  *pp_color = traj->ipoints + (n-1)*traj->idim;
		  break;
		case HEADER:
		  *pp_color = m->cur_flow->iparams;
		  break;
		default:
		  *pp_color = NULL;
		  break;
		}
	    }
	  if (pp_extra_doubles)
	    {
	      if (traj->ddim > extra_doubles_index(m))
		*pp_extra_doubles = traj->dpoints + (n-1)*traj->ddim + extra_doubles_index(m);
	      else *pp_extra_doubles = NULL;
	    }
	  if (pp_extra_ints)
	    {
	      if (traj->idim > extra_ints_index(m))
		*pp_extra_ints = traj->ipoints + (n-1)*traj->idim + extra_ints_index(m);
	      else *pp_extra_ints = NULL;
	    }
	  if (pp_extra_dparams)
	    {
	      if (m->cur_flow->num_dparams > extra_dparams_index(m))
		*pp_extra_dparams = m->cur_flow->dparams + extra_dparams_index(m);
	      else *pp_extra_dparams = NULL;
	    }
	  if (pp_extra_iparams)
	    {
	      if (m->cur_flow->num_iparams > extra_iparams_index(m))
		*pp_extra_iparams = m->cur_flow->iparams + extra_iparams_index(m);
	      else *pp_extra_iparams = NULL;
	    }
	  
	}
      traj = traj->next_seg;
    }
  return(status);
}

int
  get_first_point(m,traj, pp_varb, pp_param, pp_color,
		 pp_extra_doubles, pp_extra_ints,
		 pp_extra_dparams, pp_extra_iparams)
Memory m;
TRAJ_ITEM *traj;
double **pp_varb, **pp_param, **pp_extra_doubles, **pp_extra_dparams;
int **pp_color, **pp_extra_ints, **pp_extra_iparams;
{
  if (traj->cur_point == 0) return(1);

  if (pp_varb)
    {
      switch (where_are_varbs(m->mem_type))
	{
	case BODY:
	  *pp_varb = traj->dpoints;
	  break;
	case HEADER:
	  *pp_varb = m->cur_flow->dparams;
	  break;
	case NOWHERE:
	  *pp_varb = NULL;
	  break;
	}
    }
  if (pp_param)
    {
      switch (where_are_params(m->mem_type))
	{
	case BODY:
	  *pp_param = traj->dpoints + param_index(m);
	  break;
	case HEADER:
	  *pp_param = m->cur_flow->dparams + param_index(m);
	  break;
	case NOWHERE:
	  *pp_param = NULL;
	  break;
	}
    }
  if (pp_color)
    {
      switch (where_are_colors(m->mem_type))
	{
	case BODY:
	  *pp_color = traj->ipoints;
	  break;
	case HEADER:
	  *pp_color = m->cur_flow->iparams;
	  break;
	default:
	  *pp_color = NULL;
	  break;
	}
    }
  if (pp_extra_doubles)
    {
      if (traj->ddim > extra_doubles_index(m))
	*pp_extra_doubles = traj->dpoints + extra_doubles_index(m);
      else *pp_extra_doubles = NULL;
    }
  if (pp_extra_ints)
    {
      if (traj->idim > extra_ints_index(m))
	*pp_extra_ints = traj->ipoints + extra_ints_index(m);
      else *pp_extra_ints = NULL;
    }
  if (pp_extra_dparams)
    {
      if (m->cur_flow->num_dparams > extra_dparams_index(m))
	*pp_extra_dparams = m->cur_flow->dparams + extra_dparams_index(m);
      else *pp_extra_dparams = NULL;
    }
  if (pp_extra_iparams)
    {
      if (m->cur_flow->num_iparams > extra_iparams_index(m))
	*pp_extra_iparams = m->cur_flow->iparams + extra_iparams_index(m);
      else *pp_extra_iparams = NULL;
    }
  
return(0);
  
}


int
  free_extra_traj_mem(m,traj)
Memory m;
TRAJ_ITEM *traj;
{
  int n, status = 0;

  if ((n = traj->num_points - traj->cur_point) > 0) 
    {
      status = memory_free(m,traj->dpoints + traj->cur_point * traj->ddim, traj->ddim, 
			   traj->ipoints + traj->cur_point * traj->idim, traj->idim, n);
      traj->num_points = traj->cur_point;
    }
  return(status);
}


/****************************************************************/
/* the following routines deal with the large pools of memory.  */
/****************************************************************/


/*
 * this consultant answers the question:  
 * If I really want n trajectories with dvar_dim ddim, ivar_dim idim, and length len,
 * then how much should I ask for when I create each traj ?
 * This is important if memory is limited since we don't want
 * to allocate it all to the first trajectory built!
 */
int
  memory_alloc_suggestion(m, n, ddim, idim, len)
Memory m;
int n,ddim,idim,len;
{
  /* I'm not worth my fees ! I always assume there is enough memory! */
  /* what I should do is to figure up how much mem is available and divvy it up fairly! */
  return(len);
}


int
memory_alloc(m,dptr,ddim,iptr,idim,len)
Memory m;
double **dptr;
int ddim,**iptr,idim,len;
{
  MEM_ITEM *mem;
  int newmem, ptbytes, n=0;
  
  /* NOTE : items in the free_mem_list which contain 0 space should
     be removed from the list; except for the last item.
     My routines assume that if the free_mem_list is a NULL list
     then we are not keeping track of free'd memory. */
  
  if (!(mem=m->free_mem_list)) return(0);	/* no free mem list ! */
  if ((ddim+idim)*len ==  0) return(0);	/* not asking for any ! */
  ptbytes = ddim*sizeof(double) + idim*sizeof(int);
  do 
    {
      if (mem->num_bytes >= ptbytes) 
	{
	  /* we can give at least one point! */
	  if (mem->num_bytes >= len*ptbytes) n=len; /* we give all */
	  else n = mem->num_bytes / ptbytes;	     /* or part */
	  *dptr = (double *) mem->ptr;
	  *iptr = (int *) ((int) mem->ptr + n*ddim*sizeof(double));  /* WARNING - MAY BE SYSTEM DEPENDENT */
	  mem->ptr = (void *) ((int) mem->ptr + n*ptbytes);          /* WARNING ! */
	  mem->num_bytes -= n*ptbytes;
	  /* NOW - can we remove this mem_item if empty? - skip for now! */
	}
    }
  while ((mem=mem->next) && !n);
  if (!n) 
    { 
      /* we failed to find any memory, can we allocate another chunk */
      newmem = increase_allocated_memory(m,m->chunk_size);
      if (newmem < ptbytes) 
	{
	  system_mess_proc(0,"DANGER - OUT OF MEMORY!");
	  set_interrupt();
	}
      else 
	n = memory_alloc(m,dptr,ddim,iptr,idim,len); /* success guaranteed */
    }
  return(n);
}


int
  memory_free(m,dptr,ddim,iptr,idim,len)
Memory m;
double *dptr;
int ddim,*iptr,idim,len;
{
  MEM_ITEM *temp,*build_mem_item(), *push_ordered_mem_list();

  if (!m->free_mem_list) return(0); /* no list, so skip it */
  if (len<=0 || (ddim+idim) <= 0) return(0);		/* nothing allocated */

/*  fprintf(stdout,"memory_free: the free_mem_list on entry.\n");
  dump_mem_list(stdout,m->free_mem_list,1); */
  
  if (ddim > 0)
    {
      if (!(temp=build_mem_item(m,(void *) dptr,ddim*len*sizeof(double)))) return(-1); /* cannot release */
      m->free_mem_list = push_ordered_mem_list(m->free_mem_list, temp);
    }
  if (idim > 0)
    {
      if (!(temp=build_mem_item(m,(void *) iptr,idim*len*sizeof(int)))) return(-1); /* cannot release */
      m->free_mem_list = push_ordered_mem_list(m->free_mem_list, temp);
    }

/*  fprintf(stdout,"memory_free: the free_mem_list on exit.\n");
  dump_mem_list(stdout,m->free_mem_list,1); */
  
  return(0);
}


/*
 * destroys the free_mem_list and copies the total mem list into the
 * free mem list
 */
int
  make_free_mem_total_mem(m)
Memory m;
{
  MEM_ITEM	*temp, *new, *push_ordered_mem_list();

  if (!m) return(0);
  dismantle_mem_list(m,m->free_mem_list);
  m->free_mem_list = NULL;
  /* rebuild free_mem_list */
  temp = m->total_mem_list;
  while (temp) 
    {
      if (new=build_mem_item(m,temp->ptr,temp->num_bytes))
	m->free_mem_list = push_ordered_mem_list(m->free_mem_list,new);
      else 
	{ 
	  /* serious error ! */
	  system_mess_proc(0,"Could not rebuild free_mem_list. Memory relinquished. \n");
	  memory_destroy(m);
	  return(-1);
	}
      temp = temp->next;
    }
  return(0);
}


/* 
 * grabs more memory for allocation to trajectory data
 *
 * returns the number of dbls that will fit in newly allocated memory
 */
int
  increase_allocated_memory(m,n_bytes)
Memory m;
int n_bytes;
{
  double *ptr;
  MEM_ITEM *p1,*p2,*build_mem_item(),*push_mem_list(),*push_ordered_mem_list();
  
  /* We will try to allocate requested amount;
     On failure the amount is decreased by a factor of two
     until requested memory <= MINIMUM_MEMORY bytes or success.  This is so
     the memory routines do not eat all available memory!
     */
  if (n_bytes==0) return(0);
  do 
    {
      ptr = (void *) malloc(n_bytes);
      if (!ptr) n_bytes /= 2;
    }
  while (!ptr && n_bytes>=MINIMUM_MEMORY);

  if (!ptr) 
    return(0);
  else
    {
      /* OK we got some more, now add it to our list of freemem and return */
      p1 = build_mem_item(m,ptr,n_bytes);
      p2 = build_mem_item(m,ptr,n_bytes);
      if (!p1 || !p2) 
	{  
	  /* mem error */
	  dismantle_mem_list(m,p1);
	  dismantle_mem_list(m,p2);
	  free(ptr);
	  return(-1);
	}
      m->free_mem_list = push_ordered_mem_list(m->free_mem_list,p1);
      m->total_mem_list = push_mem_list(m->total_mem_list,p2);
    }
  return(n_bytes);	
}




/*********************** decoding type info **************/

/* the following routines define the type of memory objects 
   by specifying where the varbs, parameters, and colors are stored
*/

int
  where_are_varbs(mtype)
int mtype;
{
  switch(mtype)
    {
    case TRAJ_MEMORY:
    case FIXPT_MEMORY:
    case CONT_MEMORY:
    case SEL_PT_MEMORY:
    case MULT_MEMORY:
      return(BODY);
    case PARAM_MEMORY:
    default:
      return(NOWHERE);
    }
}      

int
  where_are_colors(mtype)
int mtype;
{
  switch(mtype)
    {
    case TRAJ_MEMORY:
    case FIXPT_MEMORY:
      return(HEADER);
    case CONT_MEMORY:
    case SEL_PT_MEMORY:
    case MULT_MEMORY:
    case PARAM_MEMORY:
      return(BODY);
    default:
      return(NOWHERE);
    }
}      


int
  where_are_params(mtype)
int mtype;
{
  switch(mtype)
    {
    case TRAJ_MEMORY:
    case FIXPT_MEMORY:
      return(HEADER);
    case CONT_MEMORY:
    case SEL_PT_MEMORY:
    case MULT_MEMORY:
    case PARAM_MEMORY:
      return(BODY);
    default:
      return(NOWHERE);
    }
}      


int 
  extra_dparams_index(m)
Memory m;
{
  int n=0;
  
  if (where_are_varbs(m->mem_type) == HEADER) n+=m->varb_dim;
  if (where_are_params(m->mem_type) == HEADER) n+=m->param_dim;
  
  return(n);
}

int 
  extra_doubles_index(m)
Memory m;
{
  int n=0;
  
  if (where_are_varbs(m->mem_type) == BODY) n+=m->varb_dim;
  if (where_are_params(m->mem_type) == BODY) n+=m->param_dim;
  
  return(n);
}

int 
  extra_iparams_index(m)
Memory m;
{
  int n=0;
  
  if (where_are_colors(m->mem_type) == HEADER) n+=m->color_dim;
  
  return(n);
}

int 
  extra_ints_index(m)
Memory m;
{
  int n=0;
  
  if (where_are_colors(m->mem_type) == BODY) n+=m->color_dim;
  
  return(n);
}


int
  param_index(m)
Memory m;
{
  if (where_are_varbs(m->mem_type) == where_are_params(m->mem_type))
    return(m->varb_dim);
  else
    return(0);
}


int
mem_get_sizes(m,pdp,pip,pd,pi)
Memory m;
int *pdp, *pip, *pd, *pi;
{
  *pdp = extra_dparams_index(m);
  *pip = extra_iparams_index(m);
  *pd = extra_doubles_index(m);
  *pi = extra_ints_index(m);
}
