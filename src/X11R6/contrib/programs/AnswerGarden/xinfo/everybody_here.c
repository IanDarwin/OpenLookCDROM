/***********************************************************

  EVERYBODY_HERE

  A program to check that all nodes in the AnswerGarden
  network exist.

  Mark Ackerman
  MIT/Center for Coordination Science
  September 27, 1990

**********************************************************/
#include <X11/Intrinsic.h>  /* make AnswerGarden.h happy */
#include <X11/StringDefs.h>
#include "AG.h"
#include <stdio.h>

#define Comma ','
#define AtSign '@'
#define MaxFileSize 5000

extern GlobalInfo global_info;
extern SortaObj *sorta_objs[];



char *malloc();
char *NodeService_Node_String_From_Type();


/********************

  From Knedit.c

********************/

#define NDelimiters  4
static char startDelimiters[NDelimiters] = {'{','[','(','<'};
static char endDelimiters[NDelimiters] = {'}',']',')','>'};

static int FindDelimiter(c)
     char c;
{
    int j;

    for (j=0;j<NDelimiters;j++)
	if (c == startDelimiters[j])
	    break;

    if (j>=NDelimiters)
	return(-2);

    return(j);
}


   /* Parse string for link name */
Get_New_Node_Name(ptr,current_file)
     char **ptr;
     char *current_file;
{
    char *loc = *ptr;
    char tempstr[MaxString];
    char *start;
    char *end;
    char end_delimiter;
    int delimiter;

    /* need delimiter */

    if ((delimiter = FindDelimiter(*loc)) < 0)
	{
	    printf("missing left delimiter in file %s\n",current_file);
	    return;
	}

    end_delimiter = endDelimiters[delimiter];
    
    if ((loc = AGindex(loc,Comma)) == (char *)0)
	{
	    printf("have broken button item in file %s\n",current_file);
	    return;
	}
    start = loc+1;
    /* assume no spaces for now */
    
    /* get end delimiter */
    while (*(++loc) != end_delimiter);

    
    /* get node name */

    end = loc-1;
    strncpy(tempstr,start,end-start+1);
    tempstr[end-start+1] = EOS;

      /* first parameter is new node name (child), second parameter
	 is parent file (curent file for this level in recursion) */
    Check_New_Node(tempstr,current_file);  
}    


Check_New_Node(name,parent_file)
     char *name;
     char *parent_file;
{
    char *label;
    int type;
    char *location;
    int n;
    FILE *fp;
    char *buffer;
    char filestring[MaxString];
    char *type_string;
    NodeInfoPtr node_info;


    /* Request nodeservice for the new node.  If there is no nodeservice
       record, complain. */

    if (NodeService_Request_By_Name(name,&node_info,
				    &label,&type,&location) < 0)
	{
	    printf("missing node %s in node service file\n",name);
	    return;
	}

    /* Get the file.  If the file exists, check its type.  If it is 
       a Discussion or Grapher, ignore.  If it is a Knedit node, read
       it and look at its buffer.  If a file does not exist, complain. */

    if (!Check_Node_Physical_Existance(location))
	{
	    printf("file %s \n\tfor node %s does not exist\n",name,location);
	    printf("\treferenced in %s\n",parent_file);
	    return;
	}

    if ((type_string = NodeService_Node_String_From_Type(type)) == NULL)
      {
	printf("node %s is of of a type %d that node service doesn't",
	       name,type);
	printf(" know about\n");
	return;
      }
    if (!strcmplo("sbrowser",type_string))
	{
	    Form_Filename(filestring,location);
	    if ((fp = fopen(filestring,"r")) == NULL)
		{   /* Check Node Physical Existance just did this */
		    printf("hard to believe, but file %s just disappeared\n",
			   location);
		    return;
		}
	    if ((buffer = malloc((unsigned)MaxFileSize)) == NULL)
		{
		    printf("out of memory\n");
		    return;
		}
	    if ((n = fread(buffer,sizeof(char), MaxFileSize, fp)) == 0)
		{   /* system error */
		    printf("could not read in file %s\n",location);
		    return;
		}
	    else
		printf("file %s being examined:  %d chars long\n",location,n);
	    buffer[n] = EOS;
	    Look_At_Buffer(buffer,location);
	    fclose(fp);
	    free(buffer);
	}
}

Look_At_Buffer(buffer,current_file)
    char *buffer;
    char *current_file;
{
    char *ptr = buffer;

    ptr = AGindex(ptr,AtSign);
    while (ptr > (char *)0)
	{
	    if (*(ptr+1) == AtSign)
		{
		    /* double @@, so skip */
		    ptr = ptr+2;
		}
	    else
		if (strncmplo(ptr,"@button",7) == 0)
		    {
			ptr = ptr+7;
			Get_New_Node_Name(&ptr,current_file);
		    }
	    ptr++;
	    ptr = AGindex(ptr,AtSign);
	}
}

char *CurrentDirectory = "./";

main(argc,argv)
     int argc;
     char *argv[];
{
    int n;
    Widget shell;

    shell = global_info.main_shell = 
      XtInitialize(argv[0],"AnswerGarden",NULL,0,&argc,argv);

    Globals_Initialize(shell);

    if (NodeService_Initialize() < 0)
      {
	XtWarning("Empty or missing NodeService file (AGNodeFileService)");
	XtWarning("You are unlikely to get good results.  Continuing...");
      }
    Check_New_Node("Root","Bogus");
    
    exit(0);
}
