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
 * paw  11/10/90
 * dstool
 *
 * mem_primitives.c
 *
 * This file contains the primitive operations for
 * the memory objects.
 *
 */

#include <stdio.h>
#include <malloc.h>

#include "mem_defs.h"

/*********************** MEM_ITEM ************************/

MEM_ITEM *
create_mem_item()
{
	MEM_ITEM *obj;

	if (!(obj = (MEM_ITEM *) calloc(1,sizeof(MEM_ITEM)))) return(NULL);
	obj->ptr = NULL;
	obj->num_bytes = 0;
	obj->next = NULL;
	return(obj);
}

int
destroy_mem_item(obj)
MEM_ITEM *obj;
{
  if (obj) cfree(obj);
	return(0);
}

int
dismantle_mem_list(m,obj)
Memory m;
MEM_ITEM *obj;
{
	MEM_ITEM *temp,*pop_mem_list();

	while (temp=pop_mem_list(&obj)) destroy_mem_item(temp);
	return(0);
}

int
dump_mem_list(fp,obj,dl)
FILE *fp;
MEM_ITEM *obj;
int dl;
{
	while(obj) {
		if (dl>0) fprintf(fp,"     ptr: 0x%x ",obj->ptr);
		fprintf(fp,"   num_bytes: %d\n",obj->num_bytes);
		obj=obj->next;
		}
	return(0);
}

MEM_ITEM *
push_mem_list(list,obj)
MEM_ITEM *list,*obj;
{
	if (!obj) return(list);
	obj->next = list;
	return(obj);
}

MEM_ITEM *
push_ordered_mem_list(list,obj)
MEM_ITEM *list,*obj;
{
  MEM_ITEM *lower;

  /* NEEDS WORK - THIS DOES THE JOB BUT SEEMS TO CUMBERSOME ! */

  if (!obj) return(list);	/* no object */
  obj -> next = NULL;     /* just in case it's not ! */
  if (!list) list=obj;	/* no list */
  else if ((int) obj->ptr < (int) list->ptr)  /* WARNING - MAY BE SYSTEM DEPENDENT */
    { /* put at head */
      if ((int) obj->ptr + obj->num_bytes == (int) list->ptr) /* WARNING - MAY BE SYSTEM DEPENDENT */
	{ 
	  /* combine them */
	  list->ptr = obj->ptr;
	  list->num_bytes += obj->num_bytes;
	  destroy_mem_item(obj);
	}
      else 
	{
	  obj->next = list;
	  list = obj;
	}
    }
  else 
    { 
      /* find position and insert */
      for (lower = list; (lower->next && ((int)lower->next->ptr < (int)obj->ptr)); lower = lower->next);
      if ((int) lower->ptr + lower->num_bytes == (int) obj->ptr ) 
	{ 
	  /* combine with lower */
	  lower->num_bytes += obj->num_bytes;
	  destroy_mem_item(obj);
	  /* now does next one fit with this one ? */
	  if (lower->next && ((int) lower->ptr+lower->num_bytes == (int) lower->next->ptr)) 
	    {
	      lower->num_bytes += lower->next->num_bytes;
	      obj = lower->next;
	      lower->next = lower->next->next;
	      destroy_mem_item(obj);
	    }	
	}
      else if (lower->next && ((int) lower->next->ptr == (int) obj->ptr+obj->num_bytes)) 
	{
	  /* combine with upper */
	  lower->next->ptr = obj->ptr;
	  lower->next->num_bytes += obj->num_bytes;
	  destroy_mem_item(obj);
	}
      else 
	{ 
	  /* insert in list */
	  obj->next = lower->next;
	  lower->next = obj;
	}
    }
  return(list);
}

MEM_ITEM *
  pop_mem_list(plist)
MEM_ITEM **plist;
{
  MEM_ITEM *temp;
  
  if (!(temp= *plist)) return(NULL);
  *plist = (*plist)->next;
  return(temp);
}


/**************************** TRAJ_ITEM ***********************/

TRAJ_ITEM *
  create_traj_item()
{
  TRAJ_ITEM *obj;
  
  if (!(obj = (TRAJ_ITEM *) calloc(1,sizeof(TRAJ_ITEM)))) return(NULL);
  obj->ddim = 0;
  obj->idim = 0;
  obj->dpoints = NULL;
  obj->ipoints = NULL;
  obj->num_points = 0;
  obj->cur_point = 0;
  obj->next_seg = NULL;
  return(obj);
}

int
  destroy_traj_item(obj)
TRAJ_ITEM *obj;
{
  if (obj) cfree(obj);
  return(0);
}

int
  dismantle_traj_list(m,obj)
Memory m;
TRAJ_ITEM *obj;
{
  TRAJ_ITEM *temp, *pop_traj_list();

  while (temp=pop_traj_list(&obj)) 
    {
      if (temp->num_points) 
	{
	  memory_free(m,temp->dpoints,temp->ddim,temp->ipoints,temp->idim,temp->num_points);
	  m->stored_points -= temp->cur_point;
	}
      destroy_traj_item(temp);
    }
  return(0);
}

int
  dump_traj_list(fp,obj,dl)
FILE *fp;
TRAJ_ITEM *obj;
int dl;
{
  int i,j;
  
  while (obj) 
    {
      fprintf(fp,"points: ");
      if (dl>0) fprintf(fp,"0x%x  0x%x ",obj->dpoints,obj->ipoints);
      fprintf(fp,"ddim=%d   idim=%d   %d / %d \n",obj->ddim, obj->idim, obj->cur_point, obj->num_points);
      if (dl>2) 
	{
	  for (i=0; i<obj->cur_point; i++) 
	    {
	      fprintf(fp,"               ");
	      for (j=0;j<obj->ddim;j++) 
		fprintf(fp,"%le ",obj->dpoints[j + i * obj->ddim]);
	      for (j=0;j<obj->idim;j++) 
		fprintf(fp,"%ld ",obj->ipoints[j + i * obj->idim]);
	      fprintf(fp,"\n");
	    }
	  fprintf(fp,"\n");
	}
      obj = obj->next_seg;
      if (obj) printf("\n               ");
    }
  fprintf(fp,"\n");
  return(0);
}

TRAJ_ITEM *
  push_traj_list(list,obj)
TRAJ_ITEM *list,*obj;
{
  if (!obj) return(list);
  obj->next_seg = list;
  return(obj);
}

TRAJ_ITEM *
  pop_traj_list(plist)
TRAJ_ITEM **plist;
{
  TRAJ_ITEM *temp;

  if (!(temp= *plist)) return(NULL);
  *plist = (*plist)->next_seg;
  return(temp);
}

TRAJ_ITEM *
  last_traj_seg(list)
TRAJ_ITEM *list;
{
  if (!list) return(NULL);
  while (list->next_seg) list=list->next_seg;
  return(list);
}


/***************************** FLOW_ITEM  *****************************/

FLOW_ITEM *
  create_flow_item(n_dparams, n_iparams, n_trajs)
int n_dparams, n_iparams, n_trajs;
{
  FLOW_ITEM *obj;

  if (!(obj = (FLOW_ITEM *) calloc(1,sizeof(FLOW_ITEM)))) return(NULL);
  if (n_dparams>0) 
    {
      if (!(obj->dparams = (double *) calloc(n_dparams,sizeof(double)))) 
	{
	  cfree(obj);
	  return(NULL);
	}
    }
  else obj->dparams = NULL;
  if (n_iparams>0) 
    {
      if (!(obj->iparams = (int *) calloc(n_iparams,sizeof(int)))) 
	{
	  if (obj->dparams) cfree(obj->dparams);
	  cfree(obj);
	  return(NULL);
	}
    }
  else obj->iparams = NULL;
  if (n_trajs>0) 
    {
      if (!(obj->trajs = (TRAJ_ITEM **) calloc(n_trajs,sizeof(TRAJ_ITEM *)))) 
	{
	  if (obj->dparams) cfree(obj->dparams);
	  if (obj->iparams) cfree(obj->iparams);
	  cfree(obj);
	  return(NULL);
	}
    }
  obj->num_dparams = n_dparams;
  obj->num_iparams = n_iparams;
  obj->num_trajs = n_trajs;
  return(obj);
}


int
  destroy_flow_item(obj)
FLOW_ITEM *obj;
{
  if (obj) 
    {
      if (obj->dparams) cfree(obj->dparams);
      if (obj->iparams) cfree(obj->iparams);
      if (obj->trajs) cfree(obj->trajs);
      cfree(obj);
    }
  return(0);
}

int
  dismantle_flow_list(m,obj)
Memory m;
FLOW_ITEM *obj;
{
  int i;
  FLOW_ITEM *temp, *pop_flow_list();

  while (temp=pop_flow_list(&obj)) 
    {
      if (temp->trajs)
	for (i=0; i<temp->num_trajs; i++)
	  dismantle_traj_list(m,temp->trajs[i]);
      destroy_flow_item(temp);
    }
  return(0);
}

int
  dump_flow_list(fp,obj,dl)
FILE *fp;
FLOW_ITEM *obj;
int dl;
{
  int i;

  while (obj) 
    {
      fprintf(fp,"FLOW: %d trajs  %d dparams  %d iparams \n",
	      obj->num_trajs, obj->num_dparams, obj->num_iparams);
      if (dl>0 && obj->num_dparams>0) 
	{
	  fprintf(fp,"     dparams: ");
	  for (i=0; i<obj->num_dparams; i++) 
	    fprintf(fp,"%le ",obj->dparams[i]);
	  fprintf(fp,"\n");
	}
      if (dl>0 && obj->num_iparams>0) 
	{
	  fprintf(fp,"     iparams: ");
	  for (i=0; i<obj->num_iparams; i++) 
	    fprintf(fp,"%ld ",obj->iparams[i]);
	  fprintf(fp,"\n");
	}
      if (obj->trajs)
	for (i=0; i<obj->num_trajs; i++) 
	  {
	    fprintf(fp,"     Traj[%d]:  ",i);
	    dump_traj_list(fp,obj->trajs[i],dl);
	  }
      obj = obj->next_flow;
    }
  return(0);
}


FLOW_ITEM *
  push_flow_list(list,obj)
FLOW_ITEM *list,*obj;
{
  if (!obj) return(list);
  obj->next_flow = list;
  return(obj);
}

FLOW_ITEM *
  attach_to_flow_list(list,obj)
FLOW_ITEM *list,*obj;
{
  FLOW_ITEM 	*temp;
  
  if (!obj) return(list); /* nothing to attach */
  if (!(temp=list)) return(obj);
  while (temp->next_flow) temp=temp->next_flow;
  temp->next_flow = obj;
  return(list);
}


FLOW_ITEM *
  pop_flow_list(plist)
FLOW_ITEM **plist;
{
  FLOW_ITEM *temp;

  if (!(temp= *plist)) return(NULL);
  *plist = (*plist)->next_flow;
  return(temp);
}


/********************************* MEMORY_OBJ ************************/

Memory
  create_memory_obj()
{
  Memory obj;

  if (!(obj = (Memory) calloc(1,sizeof(MEMORY_OBJ)))) return(NULL);
  obj->flow_list = obj->cur_flow = obj->read_flow = NULL;
  obj->cur_traj = NULL;
  obj->cur_traj_num = obj->read_traj_num = obj->read_point = 0;
  obj->total_mem_list = obj->free_mem_list = NULL;
  obj->read_traj_seg = NULL;
  obj->mem_type = obj->default_traj_length = obj->too_small_mem = obj->chunk_size = 0;
  obj->varb_dim = obj->param_dim = obj->color_dim = 0;
  obj->stored_points = 0;
  return(obj);
}

int
  destroy_memory_obj(obj)
Memory obj;
{
  if (obj) cfree(obj);
  return(0);
}

int
  dismantle_memory_obj(m)
Memory m;
{
  MEM_ITEM *tempm;

  tempm = m->total_mem_list;
  while (tempm) {
    if (tempm->ptr) free(tempm->ptr);
    tempm = tempm->next;
  }
  dismantle_mem_list(m,m->total_mem_list);
  m->total_mem_list = NULL;
  dismantle_mem_list(m,m->free_mem_list);
  m->free_mem_list = NULL;
  dismantle_flow_list(m,m->flow_list);
  destroy_memory_obj(m);
  return(0);
}

int
  dump_memory_obj(fp,m,dl)
FILE *fp;
Memory m;
int dl;
{
  fprintf(fp,"MEMORY: ");
  if (m) {
    fprintf(fp,"def_traj_len %d  too_small_mem %d  chunk_size %d  type %d\n",
	    m->default_traj_length, m->too_small_mem, m->chunk_size, m->mem_type);
    fprintf(fp,"        total stored points %d\n",m->stored_points);
    fprintf(fp,"TOTAL_MEM_LIST:\n");
    dump_mem_list(fp,m->total_mem_list,dl);
    fprintf(fp,"FREE_MEM_LIST:\n");
    dump_mem_list(fp,m->free_mem_list,dl);
    dump_flow_list(fp,m->flow_list,dl);
  }
  else fprintf(fp,"null\n");
  fprintf(fp,"\n");
  return(0);
}
