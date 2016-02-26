#include <stdio.h>
#include "treeedit.h" 
#define MaxNodes 1000
#define MaxCol 1
#define MaxRow 1

struct bst {
  struct bst *left;
  struct bst *right;
  int rank;
  int depth;
};
typedef struct bst * item;

item stack[MaxNodes];
int StackPtr;

initstack() {StackPtr=0;}

push(node)
item node;
{ 
 if (StackPtr>=MaxNodes) {
     printf("Stack Overflow\n");
     exit(1); 
 }
 else stack[StackPtr++]=node;
}

pop(node)
item *node;
{
 if (StackPtr==0) {
     printf("Stack Underflow\n");
     exit(1); 
 }
 else *node=stack[--StackPtr]; 
}
 
construct() {
   struct bst *node,*nodel,*noder;
   char c;
  
   while ((c=getchar())!='\n') {
      node=(struct bst *) malloc(sizeof(struct bst));
      node->left=NULL;
      node->right=NULL;
      if (c=='.') {
         push(node);
      }
      else if (c=='l') {
         pop(&nodel);
         node->left=nodel;
         push(node);
      } 
      else if (c=='r') {
         pop(&noder);
         node->right=noder;
         push(node);
      }
      else {
         pop(&noder);
         node->right=noder;
         pop(&nodel);
         node->left=nodel;
         push(node);
      }
   }
}

findxy(root,rank,depth,maxdepth) 
struct bst *root;
int *rank,depth,*maxdepth;
{
   if (root!=NULL) {
      if (depth>*maxdepth) *maxdepth=depth;
      findxy(root->left,rank,depth+1,maxdepth);
      root->rank= *rank;
      root->depth=depth; 
      (*rank)++;
      findxy(root->right,rank,depth+1,maxdepth);
   }
}

draw(id,root) 
struct bst *root;
int id;
{  
   cps_drawnode(id,root->rank,root->depth);
   if (root->left!=NULL) {
      cps_drawedge(id,
              root->rank,root->depth,
              root->left->rank,root->left->depth);
      draw(id,root->left);
   }
   if (root->right!=NULL) {
      cps_drawedge(id,
              root->rank,root->depth,
              root->right->rank,root->right->depth);
      draw(id,root->right);
   }
}

postout(root)
struct bst *root;
{
   int typenode;
   static char nodechar[]=".rlb";
  
   typenode=0; 
   if (root->left!=NULL) {
      postout(root->left);
      typenode+=2;
   }
   if (root->right!=NULL) {
      postout(root->right);
      typenode++;
   }
   printf("%c",nodechar[typenode]);
}

main(argc,argv) 
    char **argv;
{
   char c;
   struct bst *root;
   int maxdepth, rank;
   struct bst *rotabout,*newrot,*tempnode,*next,*parent;
   struct bst **insplace; 
   int id,cnum;
   int opt,done,x,rotx,tempx;
   float geomx;
   
   if (ps_open_PostScript()==0) {
      fprintf(stderr,"No NeWS is bad news\n");
      exit(1);
   }
   ps_flush_PostScript();
   opt=0;
   while (--argc>0) opt=((++argv)[0][0]=='-');
   if (!opt) {
      root=(struct bst *) malloc(sizeof(struct bst));
      root->left=NULL;
      root->right=NULL;
   }
   else if (argv[0][1]=='i') {
       done=0; 
       while (!done) {  
          initstack();
          construct();
          pop(&root);
          if (StackPtr>0) printf("Not a tree\n"); else done=1;
       } 
   }
   else {
       fprintf(stderr,"invalid option %c\n",argv[0]);
       exit(-1);
   }
   rotabout=root;
   rank=0; 
   maxdepth=0;
   findxy(root,&rank,0,&maxdepth);
   cps_initwindow(rank-1,maxdepth,rotabout->rank,rotabout->depth); 
   while (!psio_error(PostScriptInput)) {
          if (cps_damage(&id)) {
             draw(id,root);
             cps_Wcirc(id,rotabout->rank,rotabout->depth);
             cps_exit(id);
          }
          else if (cps_rightbutton(&id,&geomx)) {
             insplace= &root; 
             while ((*insplace)!=NULL) { 
                if ((float) ((*insplace)->rank) > geomx)
                     insplace= &((*insplace)->left);
                else insplace= &((*insplace)->right);
             }
             *insplace=(struct bst *) malloc(sizeof(struct bst));
             (*insplace)->left=NULL;
             (*insplace)->right=NULL;
             rank=0; 
             maxdepth=0;
             findxy(root,&rank,0,&maxdepth);
             cps_clearandrescale(id,rank-1,maxdepth);
             cps_exit(id); 
          }
          else if (cps_midbutton(&id,&geomx)) {
             x = (int) (geomx+0.5);
             rotx=rotabout->rank; 
             next=root;         
             while (next!=NULL) {
                tempnode=next;
                tempx=tempnode->rank;
                if ((tempx > x) && (tempx > rotx)) next=tempnode->left;
                else if ((tempx < x) && (tempx < rotx)) next=tempnode->right;
                else next=NULL;                
             }
             while (rotabout!=tempnode) {
                cps_Bcirc(id,rotabout->rank,rotabout->depth);
                pop(&rotabout);
                cps_Wcirc(id,rotabout->rank,rotabout->depth);
             }
             while (rotabout->rank!=x) { 
                cps_Bcirc(id,rotabout->rank,rotabout->depth);
                push(rotabout);
                if (rotabout->rank>x) rotabout=rotabout->left;
                else rotabout=rotabout->right;
                cps_Wcirc(id,rotabout->rank,rotabout->depth);
             }
             cps_exit(id);
          } 
          else if (cps_delbutton(&id)) {
             if ((root->left!=NULL) || (root->right!=NULL))
             switch((rotabout->left!=NULL)*2+(rotabout->right!=NULL)) {
             case 0: 
               pop(&tempnode);
               if (tempnode->left==rotabout) tempnode->left=NULL;
               else tempnode->right=NULL;
               break;
             case 1:
               if (root==rotabout) root=rotabout->right;
               else {
                  pop(&tempnode);
                  if (tempnode->left==rotabout) tempnode->left=rotabout->right;
                  else tempnode->right=rotabout->right;
               }   
               break; 
             case 2:
               if (root==rotabout) root=rotabout->left;
               else {
                  pop(&tempnode);
                  if (tempnode->left==rotabout) tempnode->left=rotabout->left;
                  else tempnode->right=rotabout->left;
               }
               break; 
             case 3:
               parent=rotabout;
               tempnode=parent->left;
               if (tempnode->right==NULL) parent->left=tempnode->left;
               else {
                  while (tempnode->right!=NULL) {
                     parent=tempnode; 
                     tempnode=parent->right;
                  }
                  parent->right=tempnode->left;
               } 
               break;
             }
             initstack();
             rotabout=root;
             maxdepth=0;
             rank=0; 
             findxy(root,&rank,0,&maxdepth);
             cps_clearandrescale(id,rank-1,maxdepth);
             cps_exit(id); 
          }
          else if (cps_get_comm(&id,&cnum)) {
             switch(cnum) {
             case 0:
                if (StackPtr>0) { 
                   cps_Bcirc(id,rotabout->rank,rotabout->depth);
                   pop(&rotabout);
                   cps_Wcirc(id,rotabout->rank,rotabout->depth);
                } 
                break;
             case 1:
                if ((rotabout->left)!=NULL) {
                   push(rotabout);
                   cps_Bcirc(id,rotabout->rank,rotabout->depth); 
                   rotabout=rotabout->left;
                   cps_Wcirc(id,rotabout->rank,rotabout->depth);
                }  
                break;
             case 2:
                if ((rotabout->right)!=NULL) {
                   push(rotabout);
                   cps_Bcirc(id,rotabout->rank,rotabout->depth);
                   rotabout=rotabout->right;
                   cps_Wcirc(id,rotabout->rank,rotabout->depth);
                }  
                break;
             case 3:
                if (StackPtr>0) {
                   pop(&newrot);
                   if (StackPtr==0) root=rotabout;
                   else {
                      pop(&tempnode);
                      push(tempnode);
                      if ((tempnode->right)==newrot) tempnode->right=rotabout;
                      else tempnode->left=rotabout;
                   }
                   if (newrot->left==rotabout) {
                      newrot->left=rotabout->right;
                      rotabout->right=newrot;
                   }
                   else {
                      newrot->right=rotabout->left;
                      rotabout->left=newrot;
                   }
                   rank=0; 
                   maxdepth=0;
                   findxy(root,&rank,0,&maxdepth);
                   cps_clearandrescale(id,rank-1,maxdepth);
                }
                break;  
             }
             cps_exit(id);
          }
          else if (cps_done() || psio_eof(PostScriptInput)) break; 
      }  
      postout(root);
      printf("\n");    
}
