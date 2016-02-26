static char sccsid[]="@(#)sweep.c	1.1 9/30/92";
#define _IN_SWEEP_C_
#include "sweep.h"

void init_board()
{
      int row,col,i,r,c,no_adj_bombs;
      float perc;
      double randno;

      stack=NULL;
      perc=((float)(level+1.0)*0.05);
      no_bombs=(int)((float)area_width*area_height*perc);
      no_cor_marked_bombs=0;
      no_marked_bombs=0;
      for(row=0;row<area_height;row++)
	    for(col=0;col<area_width;col++){
		  bomb_grid[row][col].value=0;
		  bomb_grid[row][col].selected=UNMARKED;
	    }
      for(i=0;i<no_bombs;i++){
	    do{
		  randno=(double)drand48();
		  row=(int)(randno*(double)area_height);
		  randno=(double)drand48();
		  col=(int)(randno*(double)area_width);
	    } while( bomb_grid[row][col].value==BOMB);
	    bomb_grid[row][col].value=BOMB;
      }
      for(row=0;row<area_height;row++)
	    for(col=0;col<area_width;col++){
		  no_adj_bombs=0;
		  if(bomb_grid[row][col].value!=BOMB){
			for(r=row-1;r<=row+1;r++){
			      for(c=col-1;c<=col+1;c++){
				    if(r<0 )
					  r=0;
				    if(c<0)
					  c=0;
				    if(c >= area_width || r>=area_height)
					  break;
				    if(bomb_grid[r][c].value==BOMB) no_adj_bombs++;
			      }
			}
			bomb_grid[row][col].value=no_adj_bombs;
		  }
	    }
}

void push(x,y)
    int x,y;
{
      if(top==NULL){
	    stack=(stacktype *)malloc(sizeof(stacktype));
	    stack->next=NULL;
	    stack->prev=NULL;
	    stack->x=x;
	    stack->y=y;
	    top=stack;
      }
      else{
	    top->prev=(stacktype *)malloc(sizeof(stacktype));
	    top->prev->next=top;
	    top=top->prev;
	    top->x=x;
	    top->y=y;
      }
}

void pop()
{
      if(top==NULL){
	    fprintf(stderr,"Stack underflow\n");
	    exit(1);
      }
      top=top->next;
      if(top!=NULL){
	    free(top->prev);
	    top->prev=NULL;
      }
}


void flag(x,y,set)
    int x,y;
    marktype set;
{
      bomb_grid[x][y].selected=set;
}



